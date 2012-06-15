{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.CmdLine
  ( create, optArg, posArg ) where

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.Err as E

import Text.PrettyPrint
import Text.Parsec as P

import qualified System.Console.CmdTheLine.Trie as T
import qualified Data.Map as M

import Data.List     ( sort )
import Data.Function ( on )

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
  | str !! 1 /= '-' =
    if length str == 2
       then ( str,        Nothing )
       else ( take 3 str, Just $ drop 3 str )

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
    []            -> Right ( cl, reverse pargs)
    ("--" : rest) -> Right ( cl, reverse $ pargs ++ rest )

    (str : rest)  ->
      if not $ isOpt str
         then go (k + 1) opti cl (str : pargs) rest
         else consume str rest
    where
    isOpt str = length str > 1 && head str == '-'

    consume str args = case T.lookup name opti of
      Left  T.NotFound  -> Left $ UsageFail unknown
      Left  T.Ambiguous -> Left $ UsageFail ambiguous
      Right ai          -> result ai
      where
      unknown   = E.unknown   "option" name
      ambiguous = E.ambiguous "option" name ambs
        where
        ambs = sort $ T.ambiguities opti name

      ( name, value ) = parseOptArg str

      result ai = go (k + 1) opti (M.insert ai arg' cl) pargs args'
        where
        arg' = Opt $ ( k, name, value' ) : optArg cl ai

        ( value', args' )
          | value /= Nothing || optKind ai == FlagKind ||
            args == []       || isOpt (head args)      = ( value, args )
          | otherwise                                  = ( Just $ head args
                                                         , tail args
                                                         )

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
  excess = E.posExcess . map text $ takeEnd (last - maxSpec) args

  ( cl', maxSpec ) = go cl (-1) posInfo

  takeEnd n = reverse . take n . reverse

  go cl maxSpec []         = ( cl, maxSpec )
  go cl maxSpec (ai : ais) = go cl' maxSpec' ais
    where
    cl'               = M.insert ai arg cl
    ( arg, maxSpec' ) = case posKind ai of
      PosAny       -> ( Pos args, last )
      PosN rev pos -> result rev pos False indexPositions
      PosL rev pos -> result rev pos False take
      PosR rev pos -> result rev pos True  (takeEnd . (last -))

    indexPositions pos args = [args !! (pos+1)]

    result rev pos maxIsLast getPositions
      | pos' < 0 || pos' > last = ( Pos [],                       maxSpec'' )
      | otherwise               = ( Pos $ getPositions pos' args, maxSpec'' )
      where
      pos'      = if rev       then last - pos else pos
      maxSpec'' = if maxIsLast then last       else max pos' maxSpec

create :: [ArgInfo] -> [String] -> Err CmdLine
create ais args = do
  ( cl', pargs ) <- parseArgs opti cl args
  processPosArgs posi cl' pargs
  where
  ( opti, posi, cl ) = argInfoIndexes ais
