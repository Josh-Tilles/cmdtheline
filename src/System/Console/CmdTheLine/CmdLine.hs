{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.CmdLine where

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.Err as E

import Text.PrettyPrint
import Text.Parsec as P

import qualified System.Console.CmdTheLine.Trie as T
import qualified Data.Map as M

import Data.List ( sort )

optArg :: CmdLine -> ArgInfo -> [( Int, String, Maybe String )]
optArg cl ai = case M.lookup ai cl of
  Nothing -> error "ArgInfo passed to optArg does not index CmdLine"
  Just a  -> case a of
    Opt opt -> opt
    _       -> error "ArgInfo passed to optArg indexes to positional argument"

posArg :: CmdLine -> ArgInfo -> [String]
posArg cl ai = case M.lookup ai cl of
  Nothing -> error "ArgInfo passed to posArg does not index CmdLine"
  Just a  -> case a of
    Pos opt -> opt
    _       -> error "ArgInfo passed to posArg indexes to positional argument"

chooseTerm :: TermInfo -> [( TermInfo, a )] -> [String]
           -> Err ( TermInfo, [String] )
chooseTerm ti _       []              = Right ( ti, [] )
chooseTerm ti choices args@( arg : rest )
  | length arg > 1 && head arg == '-' = Right ( ti, args )

  | otherwise = case T.lookup arg index of
    Right choice      -> Right ( choice, rest )
    Left  T.NotFound  -> Left . UsageFail $ E.unknown   (text com) (text arg)
    Left  T.Ambiguous -> Left . UsageFail . text $ E.ambiguous com arg ambs
    where
    index = foldl add T.empty choices
      where
      add acc ( choice, _ ) = T.add acc (name choice) choice

    com  = "command"
    ambs = sort $ T.ambiguities index arg

{- Returns a trie mapping the names of optional arguments to their ArgInfo, a
 - list with all ArgInfo for positional arguments, and a CmdLine mapping each
 - ArgInfo to an empty list of Arg.
 -}
argInfoIndexes :: [ArgInfo] -> ( T.Trie ArgInfo, [ArgInfo], CmdLine )
argInfoIndexes ais = go T.empty [] M.empty ais
  where
  go opti posi cl []           = ( opti, posi, cl )
  go opti posi cl (arg : rest)
    | isPos arg = go opti
                     (arg : posi)
                     (M.insert arg (Pos []) cl)
                     rest
    | otherwise = go (foldl add opti $ optNames arg)
                     posi
                     (M.insert arg (Opt []) cl)
                     rest
      where
      add t name = T.add t name arg

parseOptArg :: String -> ( String, Maybe String )
parseOptArg str
  | str !! 2 /= '-' =
    if length str == 2
       then ( str,        Nothing )
       else ( take 2 str, Just $ drop 2 str )

  | otherwise       = case P.parse assignment "" str of
    Left _       -> ( str, Nothing )
    Right result -> result
    where
    assignment = do
      label <- P.many1 $ P.satisfy (/= '=')
      value <- optionMaybe $ P.char '=' >> P.many1 P.anyChar
      return ( label, value )

{- Returns an updated CmdLine according to the options found in `args`
 - with the trie index `opti`.  Positional arguments are returned in order.
 -}
parseArgs :: T.Trie ArgInfo -> CmdLine -> [String]
          -> Err ( CmdLine, [String] )
parseArgs opti cl args = go 1 opti cl [] args
  where
  go k opti cl pargs args = case args of
    []            -> Right ( cl, reverse pargs )
    ("--" : args) -> Right ( cl, reverse $ pargs ++ args )

    (str : args)  ->
      if not $ isOpt str
         then go (k + 1) opti cl (str : pargs) args
         else consume str args
    where
    isOpt str = length str > 1 && head str == '-'

    consume str args = case T.lookup name opti of
      Left  T.NotFound  -> Left $ UsageFail unknown
      Left  T.Ambiguous -> Left $ UsageFail ambiguous
      Right a           -> result a
      where
      unknown   = E.unknown   (text "option") (text name)
      ambiguous = text $ E.ambiguous "option" name ambs
        where
        ambs = sort $ T.ambiguities opti name

      ( name, value ) = parseOptArg str

      result a = go (k + 1) opti (M.insert a arg' cl) pargs args'
        where
        arg' = Opt $ ( k, name, value' ) : optArg cl a

        ( value', args' ) =
          if value /= Nothing || optKind a == FlagKind
             then ( value, args )
             else bifurcate args

        bifurcate args@[]         = ( Nothing, args )
        bifurcate args@(v : rest)
          | isOpt v   = ( Nothing, args )
          | otherwise = ( Just v,  rest )

{- Returns an updated CmdLine in which each positional arg mentioned in the
 - the list index `posInfo`, is given a value according the list of positional
 - argument values `args`.
 -}
processPosArgs :: [ArgInfo] -> CmdLine -> [String] -> Err CmdLine
processPosArgs _       cl []   = Right cl
processPosArgs posInfo cl args
  | last <= maxSpec = Right cl'
  | otherwise       = Left  $ UsageFail excess
  where
  last   = length args
  excess = E.posExcess . map text $ drop maxSpec args

  ( cl', maxSpec ) = go cl 0 posInfo

  go cl maxSpec []         = ( cl, maxSpec )
  go cl maxSpec (ai : ais) = go cl' maxSpec' ais
    where
    cl'               = M.insert ai arg cl
    ( arg, maxSpec' ) = case posKind ai of
      PosAny       -> ( Pos args, last )
      PosN rev pos -> result rev pos False indexPositions
      PosL rev pos -> result rev pos False take
      PosR rev pos -> result rev pos True  (takeEnd . (last -))

    takeEnd n = reverse . take n . reverse

    indexPositions pos args = [args !! pos]

    result rev pos maxIsLast getPositions =
      if pos' < 1 || pos' > last
         then ( Pos [],                       maxSpec'' )
         else ( Pos $ getPositions pos' args, maxSpec'' )
      where
      pos'      = if rev       then last - pos else pos
      maxSpec'' = if maxIsLast then last       else max pos' maxSpec

create :: [ArgInfo] -> [String] -> Err CmdLine
create ais args = do
  ( cl', pargs ) <- parseArgs opti cl args
  processPosArgs posi cl pargs
  where
  ( opti, posi, cl ) = argInfoIndexes ais
