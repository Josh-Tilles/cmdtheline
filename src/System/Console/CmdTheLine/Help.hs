{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Help where

import System.Console.CmdTheLine.Common
import qualified System.Console.CmdTheLine.Manpage as Man

import Control.Applicative
import Control.Arrow       ( second )

import Data.Char     ( toUpper, toLower )
import Data.List     ( intersperse, sort, sortBy, partition )
import Data.Function ( on )
import Data.Maybe    ( catMaybes )

import System.IO

invocation :: Char -> EvalInfo -> String
invocation sep ei = case evalKind ei of
  Choice -> progName ++ [sep] ++ choiceName
  _      -> progName
  where
  progName   = termName . fst $ main ei
  choiceName = termName . fst $ term ei

title :: EvalInfo -> Title
title ei = ( invocName, 1, "", leftFooter, centerHeader )
  where
  invocName = map toUpper $ invocation '-' ei

  leftFooter = prog ++ ver
    where
    ver = case version . fst $ main ei of
      ""  -> ""
      str -> ' ' : str 

  centerHeader = prog ++ " Manual"

  prog = capitalize progName
    where
    capitalize = (:) <$> toUpper . head <*> drop 1
    progName   = termName . fst $ main ei

nameSection :: EvalInfo -> [ManBlock]
nameSection ei =
  [ S "NAME"
  , P $ invocation '-' ei ++ prep (termDoc . fst $ term ei)
  ]
  where
  prep "" = ""
  prep doc = " - " ++ doc

synopsis :: EvalInfo -> String
synopsis ei = case evalKind ei of
  Main   -> concat [ "$(b,", invocation ' ' ei, ") $(i,COMMAND) ..." ]
  _      -> concat [ "$(b,", invocation ' ' ei, ") [$(i,OPTION)]...", args ]
  where
  args = concat . intersperse " " $ map snd args'
    where
    args' = sortBy revCmp . formatPos [] . snd $ term ei

  formatPos acc [] = acc
  formatPos acc (ai : ais)
    | isOpt ai  = formatPos acc ais
    | otherwise = formatPos (( posKind ai, v'' ) : acc) ais
    where
    v | argName ai == "" = "$(i,ARG)"
      | otherwise        = concat [ "$(i,", argName ai, ")" ]

    v' | absence ai == Absent = show v
       | otherwise            = concat [ "[", show v, "]" ]

    v'' = v' ++ followedBy

    followedBy = case posKind ai of
      PosN _ _ -> ""
      _        -> "..."

  revCmp ( p, _ ) ( p', _ ) = case ( p', p ) of
    ( _,             PosAny    ) -> LT
    ( PosAny,        _         ) -> GT
    ( PosL  _     _, PosR  _ _ ) -> LT
    ( PosR  _     _, PosL  _ _ ) -> GT
    ( p, p' ) -> bifurcate
    where
    bifurcate
      | not (getBool p) && not (getBool p') = case ( p, p' ) of
        ( PosL _ _,  PosN _ _ ) -> if k <= k' then LT else GT
        ( PosN _ _,  PosL _ _ ) -> if k >= k' then GT else LT
        ( PosN _ _,  _        ) -> if k <= k' then LT else GT
        _                       -> if k >= k' then GT else LT

      | getBool p && getBool p' = case ( p, p' ) of
        ( PosL _ _,  PosN _ _ ) -> if k >= k' then LT else GT
        ( PosN _ _,  PosL _ _ ) -> if k <= k' then GT else LT
        ( PosN _ _,  _        ) -> if k >= k' then LT else GT
        _                       -> if k <= k' then GT else LT
      where
      k  = getPos p
      k' = getPos p'

    getPos x = case x of
      PosL  _ pos -> pos
      PosR  _ pos -> pos
      PosN  _ pos -> pos

    getBool x = case x of
      PosL  b _ -> b
      PosR  b _ -> b
      PosN  b _ -> b

synopsisSection :: EvalInfo -> [ManBlock]
synopsisSection ei = [ S "SYNOPSIS", P (synopsis ei) ]

makeArgLabel :: ArgInfo -> String
makeArgLabel ai
  | isPos ai  = concat [ "$(i,", argName ai, ")" ]
  | otherwise = concat . intersperse ", " $ map (fmtName var) names
  where
  var | argName ai == "" = "VAL"
      | otherwise        = argName ai

  names = sort $ optNames ai

  fmtName var = case optKind ai of
    FlagKind   -> \ n -> concat [ "$(b,", n, ")" ]
    OptKind    -> mkOptMacro
    OptVal   _ -> mkOptValMacro
    where
    mkOptValMacro n = concat [ "$(b,", n, ")[", sep, "$(i,", var, ")]" ]
      where
      sep | length n > 2 = "="
          | otherwise    = ""

    mkOptMacro n = concat [ "$(b,", n, ")", sep, "$(i,", var, ")" ]
      where
      sep | length n > 2 = "="
          | otherwise    = " "

makeArgItems :: EvalInfo -> [( String, ManBlock )]
makeArgItems ei = map format xs
  where
  xs = sortBy revCmp . filter isArgItem . snd $ term ei

  isArgItem ai = not $ isPos ai && (argName ai == "" || argDoc ai == "")

  revCmp ai' ai
    | c /= EQ   = c
    | otherwise = compare' ai ai'
    where
    c        = (compare `on` argHeading) ai ai'
    compare' = case ( isOpt ai, isOpt ai' ) of
      ( True,  True  ) -> compare `on` key . optNames
      ( False, False ) -> compare `on` map toLower . argName
      ( True,  False ) -> const $ const LT
      ( False, True  ) -> const $ const GT

    key names
      | k !! 1 == '-' = drop 2 k
      | otherwise     = k
      where
      k = map toLower . head $ sortBy descCompare names

  format ai = ( argHeading ai, I label text )
    where
    label = makeArgLabel ai ++ argvDoc
    text  = substDocName (argName ai) (argDoc ai)

    argvDoc = case ( absent, optvOpt ) of
      ( "", "" ) -> ""
      ( s,  "" ) -> concat [ " (", s, ")" ]
      ( "", s  ) -> concat [ " (", s, ")" ]
      ( s,  s' ) -> concat [ " (", s, ", ", s', ")" ]


    absent = case absence ai of
      Absent     -> ""
      Present "" -> ""
      Present v  -> "absent=" ++ v

    optvOpt = case optKind ai of
      OptVal v -> "default=" ++ v
      _        -> ""

  substDocName argName =
    Man.substitute (const id) [( "argName", ("$(i," ++ argName ++ ")") )]

makeCmdItems :: EvalInfo -> [( String, ManBlock )]
makeCmdItems ei = case evalKind ei of
  Simple -> []
  Choice -> []
  Main   -> sortBy (descCompare `on` fst) . foldl addCmd [] $ choices ei
  where
  addCmd acc ( ti, _ ) = ( termHeading ti, I (label ti) (termDoc ti) )
                       : acc
  label ti = "$(b," ++ termName ti ++ ")"

-- Orphans are marked by `Nothing`. Once the algorithm is better understood,
-- perhaps we could move to `Either Orphan NotOrphan`.
mergeOrphans :: [Maybe ManBlock] -> [( String, Maybe ManBlock )]
             -> [Maybe ManBlock] -> [Maybe ManBlock]
mergeOrphans acc orphans blocks = case blocks of
  Nothing : rest -> mergeOrphans acc'           []      rest
  mBlock  : rest -> mergeOrphans (mBlock : acc) orphans rest
  []             -> acc
  where
  acc' = case orphans of
    []           -> acc
    ( s, _ ) : _ -> merge acc s orphans

  merge acc s []                 = Just (S s) : acc
  merge acc s ( ( s', i ) : rest)
    | s == s'   = merge (i : acc)              s  rest
    | otherwise = merge (i : Just (S s) : acc) s' rest

mergeItems :: [Maybe ManBlock] -> [Maybe ManBlock] -> Bool
           -> [( String, Maybe ManBlock )] -> [ManBlock]
           -> ( [Maybe ManBlock], [( String, Maybe ManBlock )] )
mergeItems acc toInsert mark is blocks = case blocks of
  sec@(S _) : rest -> transition sec rest
  t         : rest -> mergeItems (Just t : acc) toInsert mark is rest
  []              -> ( marked, is )
  where
  acc' = toInsert ++ acc
  marked
    | mark      = Nothing : acc'
    | otherwise = acc'

  transition sec@(S str) rest = mergeItems acc'' toInsert'' mark' is' rest
    where
    ( toInsert', is' ) = partition ((== str) . fst) is
    toInsert''         = map snd toInsert'
    acc''              = Just sec : marked
    mark'              = str == "DESCRIPTION"

text :: EvalInfo -> [ManBlock]
text ei = catMaybes $ mergeOrphans [] orphans revText
  where
  cmds  = makeCmdItems ei
  args  = makeArgItems ei
  cmp   = compare `on` fst
  items = map (second Just) . reverse . sortBy cmp $ cmds ++ args

  ( revText, orphans ) =
    mergeItems [Nothing] [] False items . man . fst $ term ei

eiSubst ei =
  [ ( "tname", termName . fst $ term ei )
  , ( "mname", termName . fst $ main ei )
  ]

page :: EvalInfo -> ( Title, [ManBlock] )
page ei = ( title ei, nameSection ei ++ synopsisSection ei ++ text ei )

print :: HFormat -> Handle -> EvalInfo -> IO ()
print fmt h ei = Man.print (eiSubst ei) fmt h (page ei)

prepSynopsis :: EvalInfo -> String
prepSynopsis ei = escape $ synopsis ei
  where
  escape = Man.substitute Man.plainEsc $ eiSubst ei

printVersion :: Handle -> EvalInfo -> IO ()
printVersion h ei = case version . fst $ main ei of
  ""  -> error "printVersion called on EvalInfo without version"
  str -> hPutStrLn h str
