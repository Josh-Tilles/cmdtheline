{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Arg
  (
  -- * Creating ArgInfos
    optInfo, posInfo

  -- * Optional arguments
  -- $opt

  -- ** Flag options
  , flag, flagAll, vFlag, vFlagAll

  -- ** Assignable options
  , opt, defaultOpt, optAll, defaultOptAll

  -- * Positional arguments
  -- $pos
  , pos, revPos, posAny, posLeft, posRight, revPosLeft, revPosRight

  -- * Constraining Terms
  , required, nonEmpty, lastOf
  ) where

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.CmdLine ( optArg, posArg )
import System.Console.CmdTheLine.ArgVal  ( ArgVal(..) )
import qualified System.Console.CmdTheLine.Err  as E
import qualified System.Console.CmdTheLine.Trie as T

import Control.Applicative
import Text.PrettyPrint

import Data.List     ( sort, sortBy )
import Data.Function ( on )

argFail :: Doc -> Err a
argFail = Left . MsgFail

-- | Initialize an 'ArgInfo' by providing a list of names.  The fields
-- @argName@(found at "System.Console.CmdTheLine#argName"),
-- @argDoc@(found at "System.Console.CmdTheLine#argDoc"), and
-- @argSection@(found at "System.Console.CmdTheLine#argSection")
-- can then be manipulated post-mortem, as in
--
-- > inf =(optInfo    [ "i", "insufflation" ])
-- >     { argName    = "INSUFFERABLE"
-- >     , argDoc     = "in the haunted house's harrow"
-- >     , argSection = "NOT FOR AUGHT"
-- >     }
--
-- Names of one character in length will be prefixed by @-@ on the
-- command line, while longer names will be prefixed by @--@.
--
-- This function is meant to be used with optional arguments produced by 'flag',
-- 'opt', and friends-- not with positional arguments.  Positional arguments
-- provided with names will yield a run-time error, halting any and all program
-- runs.  Use 'posInfo' for positional arguments.
--
-- Likewise, if an optional argument is created with an 'ArgInfo' produced by
-- passing an empty list of names to 'optInfo', a run-time error will occur.
-- All optional arguments must have names.
optInfo :: [String] -> ArgInfo
optInfo names = ArgInfo
  { absence    = Present ""
  , argDoc     = ""
  , argName    = ""
  , argSection = defaultSection
  , posKind    = PosAny
  , optKind    = FlagKind
  , optNames   = map dash names
  , repeatable = False
  }
  where
  defaultSection
    | names == [] = "ARGUMENTS"
    | otherwise   = "OPTIONS"

  dash "" =
    error "System.Console.CmdTheLine.Arg.optInfo recieved empty string as name"

  dash str@[_] = "-"  ++ str
  dash str     = "--" ++ str

-- | As 'optInfo' but for positional arguments, which by virtue of their
-- positions require no names.  If a positional argument is created with a
-- name, a run-time error will occur halting any and all program runs.
--
-- If you mean to create an optional argument, use 'optInfo', and be sure to
-- give it a non-empty list of names to avoid these errors.
posInfo :: ArgInfo
posInfo = optInfo []


{- $opt

  An optional argument is specified on the command line by a /name/ possibly
  followed by a /value/.

  The name of an option can be /short/ or /long/.

  * A /short/ name is a dash followed by a single alphanumeric character:
    @-h@, @-q@, @-I@.

  * A /long/ name is two dashes followed by alphanumeric characters and dashes:
    @--help@, @--silent@, @--ignore-case@.

  More than one name may refer to the same optional argument.  For example in
  a given program the names @-q@, @--quiet@, and @--silent@ may all stand for
  the same boolean argument indicating the program to be quiet.  Long names can
  be specified by any non-ambiguous prefix.

  There are three ways to assign values to an optional argument on the command
  line.

  * As the next token on the command line: @-o a.out@, @--output a.out@.

  * Glued to a short name: @-oa.out@.

  * Glued to a long name after an equal character: @--output=a.out@.

  Glued forms are necessary if the value itself starts with a dash, as is the
  case for negative numbers, @--min=-10@.

-}

--
-- Flags
--

-- | Create a command line flag that can appear at most once on the
-- command line.  Yields @False@ in absence and @True@ in presence.
flag :: ArgInfo -> Term Bool
flag ai =
  if isPos ai
     then error E.errNotPos
     else Term [ai] yield
  where
  yield _ cl = case optArg cl ai of
    []                  -> Right   False
    [( _, _, Nothing )] -> Right   True
    [( _, f, Just v  )] -> argFail $ E.flagValue f v

    (( _, f, _ ) :
     ( _, g, _ ) :
     _           ) -> argFail $ E.optRepeated f g

-- | As 'flag' but may appear an infinity of times. Yields a list of @True@s
-- as long as the number of times present.
flagAll :: ArgInfo -> Term [Bool]
flagAll ai
  | isPos ai  = error E.errNotPos
  | otherwise = Term [ai'] yield
  where
  ai' = ai { repeatable = True }

  yield _ cl = case optArg cl ai' of
    [] -> Right []
    xs -> mapM truth xs

  truth ( _, f, mv ) = case mv of
    Nothing -> Right   True
    Just v  -> argFail $ E.flagValue f v

-- | 'vFlag' @v [ ( v1, ai1 ), ... ]@ is an argument that can be present at most
-- once on the command line. It takes on the value @vn@ when appearing as
-- @ain@.
vFlag :: a -> [( a, ArgInfo )] -> Term a
vFlag v assoc = Term (map flag assoc) yield
  where
  flag ( _, ai )
    | isPos ai  = error E.errNotPos
    | otherwise = ai

  yield _ cl = go Nothing assoc
    where
    go mv [] = case mv of
      Nothing       -> Right v
      Just ( _, v ) -> Right v

    go mv (( v, ai ) : rest) = case optArg cl ai of
      []                  -> go mv rest

      [( _, f, Nothing )] -> case mv of
        Nothing       -> go (Just ( f, v )) rest
        Just ( g, _ ) -> argFail $ E.optRepeated g f

      [( _, f, Just v )]  -> argFail $ E.flagValue f v

      (( _, f, _ ) :
       ( _, g, _ ) :
       _           ) -> argFail $ E.optRepeated g f

-- | 'vFlagAll' @vs assoc@ is as 'vFlag' except that it can be present an
-- infinity of times.  In absence, @vs@ is yielded.  When present, each
-- value is collected in the order they appear.
vFlagAll :: [a] -> [( a, ArgInfo)] -> Term [a]
vFlagAll vs assoc = Term (map flag assoc) yield
  where
  flag ( _, ai ) 
    | isPos ai  = error E.errNotOpt
    | otherwise = ai { repeatable = True }

  yield _ cl = case assoc of
    [] -> Right vs
    _  -> map snd . sortBy (compare `on` fst)
      <$> foldl addLookup (Right []) assoc
    where
    addLookup acc ( v, ai ) = case optArg cl ai of
      [] -> acc
      xs -> (++) <$> mapM flagVal xs <*> acc
      where
      flagVal ( pos, f, mv ) = case mv of
        Nothing -> Right   ( pos, v )
        Just v  -> argFail $ E.flagValue f v
   

--
-- Options
--

parseOptValue :: ArgVal a => String -> String -> Err a
parseOptValue f v = case parser v of
  Left  e -> Left  . UsageFail $ E.optParseValue f e
  Right v -> Right v

mkOpt :: ArgVal a => Maybe a -> a -> ArgInfo -> Term a
mkOpt vopt v ai
  | isPos ai  = error E.errNotOpt
  | otherwise = Term [ai'] yield
    where
    ai' = ai { absence = Present . show $ pp v
             , optKind = case vopt of
                 Nothing -> OptKind
                 Just dv -> OptVal . show $ pp dv
             }
    yield _ cl = case optArg cl ai' of
      []                  -> Right v
      [( _, f, Just v )]  -> parseOptValue f v

      [( _, f, Nothing )] -> case vopt of
        Nothing   -> argFail $ E.optValueMissing f
        Just optv -> Right   optv

      (( _, f, _ ) :
       ( _, g, _ ) :
       _           ) -> argFail $ E.optRepeated g f

-- | 'opt' @v ai@ is an optional argument that yields @v@ in absence, or an
-- assigned value in presence.  If the option is present, but no value is
-- assigned, it is considered a user-error and usage is printed on exit.
opt :: ArgVal a => a -> ArgInfo -> Term a
opt = mkOpt Nothing

-- | 'defaultOpt' @def v ai@ is as 'opt' except if it is present and no value is
-- assigned on the command line, @def@ is the result.
defaultOpt :: ArgVal a => a -> a -> ArgInfo -> Term a
defaultOpt x = mkOpt $ Just x

mkOptAll :: ( ArgVal a, Ord a ) => Maybe a -> [a] -> ArgInfo -> Term [a]
mkOptAll vopt vs ai
  | isPos ai  = error E.errNotOpt
  | otherwise = Term [ai'] yield
    where
    ai' = ai { absence    = Present ""
             , repeatable = True
             , optKind    = case vopt of
                 Nothing -> OptKind
                 Just dv -> OptVal . show $ pp dv
             }

    yield _ cl = case optArg cl ai' of
      [] -> Right vs
      xs -> map snd . sortBy (compare `on` fst) <$> mapM parse xs

    parse ( pos, f, mv' ) = case mv' of
      Just v  -> (,) pos <$> parseOptValue f v
      Nothing -> case vopt of
        Nothing -> argFail $ E.optValueMissing f
        Just dv -> Right   ( pos, dv )

-- | 'optAll' @vs ai@ is like 'opt' except that it yields @vs@ in absence and
-- can appear an infinity of times.  The values it is assigned on the command
-- line are accumulated in the order they appear.
optAll :: ( ArgVal a, Ord a ) => [a] -> ArgInfo -> Term [a]
optAll = mkOptAll Nothing

-- | 'defaultOptAll' @def vs ai@ is like 'optAll' except that if it is present
-- without being assigned a value, the value @def@ takes its place in the list
-- of results.
defaultOptAll :: ( ArgVal a, Ord a ) => a -> [a] -> ArgInfo -> Term [a]
defaultOptAll x = mkOptAll $ Just x


{- $pos

  Positional arguments are tokens on the command line that are not option names
  or the values being assigned to an optional argument.

  Since positional arguments may be mistaken as the optional value of an
  optional argument or they may need to look like an optional name, anything
  that follows the special token @--@(with spaces on both sides) on the command
  line is considered to be a positional argument.

  Positional arguments are listed in documentation sections iff they are
  assigned both an @argName@ and an @argDoc@.

-}

--
-- Positional arguments.
--

parsePosValue :: ArgVal a => ArgInfo -> String -> Err a
parsePosValue ai v = case parser v of
  Left  e -> Left  . UsageFail $ E.posParseValue ai e
  Right v -> Right v

mkPos :: ArgVal a => Bool -> Int -> a -> ArgInfo -> Term a
mkPos rev pos v ai = Term [ai'] yield
  where
  ai' = ai { absence = Present . show $ pp v
           , posKind = PosN rev pos
           }
  yield _ cl = case posArg cl ai' of
    []  -> Right v
    [v] -> parsePosValue ai' v
    _   -> error "saw list with more than one member in pos converter"

-- | 'pos' @n v ai@ is an argument defined by the @n@th positional argument
-- on the command line. If absent the value @v@ is returned.
pos :: ArgVal a => Int -> a -> ArgInfo -> Term a
pos    = mkPos False

-- | 'revPos' @n v ai@ is as 'pos' but counting from the end of the command line
-- to the front.
revPos :: ArgVal a => Int -> a -> ArgInfo -> Term a
revPos = mkPos True

posList :: ArgVal a => PosKind -> [a] -> ArgInfo -> Term [a]
posList kind vs ai
  | isOpt ai  = error E.errNotPos
  | otherwise = Term [ai'] yield
    where
    ai' = ai { posKind = kind }
    yield _ cl = case posArg cl ai' of
      [] -> Right vs
      xs -> mapM (parsePosValue ai') xs

-- | 'posAny' @vs ai@ yields a list of all positional arguments or @vs@ if none
-- are present.
posAny :: ArgVal a => [a] -> ArgInfo -> Term [a]
posAny = posList PosAny

-- | 'posLeft' @n vs ai@ yield a list of all positional arguments to the left of
-- the @n@th positional argument or @vs@ if there are none.
posLeft :: ArgVal a => Int -> [a] -> ArgInfo -> Term [a]
posLeft = posList . PosL False

-- | 'posRight' @n vs ai@ is as 'posLeft' except yielding all values to the right
-- of the @n@th positional argument.
posRight :: ArgVal a => Int -> [a] -> ArgInfo -> Term [a]
posRight = posList . PosR False

-- | 'revPosLeft' @n vs ai@ is as 'posLeft' except @n@ counts from the end of the
-- command line to the front.
revPosLeft :: ArgVal a => Int -> [a] -> ArgInfo -> Term [a]
revPosLeft = posList . PosL True

-- | 'revPosRight' @n vs ai@ is as 'posRight' except @n@ counts from the end of
-- the command line to the front.
revPosRight :: ArgVal a => Int -> [a] -> ArgInfo -> Term [a]
revPosRight = posList . PosR True


--
-- Arguments as terms.
--

absent = map (\ ai -> ai { absence = Absent })

-- | 'required' @term@ converts @term@ so that it fails in the 'Nothing' and
-- yields @a@ in the 'Just'.
--
-- This is used for required positional arguments.  There is nothing
-- stopping you from using it with optional arguments, except that they
-- would no longer be optional and it would be confusing from a user's
-- perspective.
required :: Term (Maybe a) -> Term a
required (Term ais yield) = Term ais' yield'
  where
  ais' = absent ais
  yield' ei cl = case yield ei cl of
    Left  e  -> Left  e
    Right mv -> maybe (argFail . E.argMissing $ head ais') Right mv

-- | 'nonEmpty' @term@ is a term that fails if its result is empty. Intended
-- for non-empty lists of positional arguments.
nonEmpty :: Term [a] -> Term [a]
nonEmpty (Term ais yield) = Term ais' yield'
  where
  ais' = absent ais
  yield' ei cl = case yield ei cl of
    Left  e  -> Left    e
    Right [] -> argFail . E.argMissing $ head ais'
    Right xs -> Right   xs

-- | 'lastOf' @term@ is a term that fails if its result is empty and evaluates
-- to the last element of the resulting list otherwise.  Intended for lists
-- of flags or options where the last takes precedence.
lastOf :: Term [a] -> Term a
lastOf (Term ais yield) = Term ais yield'
  where
  yield' ei cl = case yield ei cl of
    Left e   -> Left    e
    Right [] -> argFail . E.argMissing $ head ais
    Right xs -> Right   $ last xs
