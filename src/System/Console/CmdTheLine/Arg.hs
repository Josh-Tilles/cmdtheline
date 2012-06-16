{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
{-# LANGUAGE FlexibleInstances #-}
module System.Console.CmdTheLine.Arg
  (
  -- * Parsing values from the command-line
    ArgVal(..), ArgParser, ArgPrinter
  , fromParsec, just, maybePP, enum

  -- * Creating ArgInfos
  , optInfo, posInfo

  -- * Flag options
  , flag, flagAll, vFlag, vFlagAll

  -- * Assignable options
  , opt, defaultOpt, optAll, defaultOptAll

  -- * Positionals
  , pos, revPos, posAny, posLeft, posRight, revPosLeft, revPosRight

  -- * Constraining Terms
  , required, nonEmpty, lastOf
  ) where

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.CmdLine
import qualified System.Console.CmdTheLine.Err  as E
import qualified System.Console.CmdTheLine.Trie as T

import Data.Function ( on )
import Control.Applicative hiding ( (<|>), empty )
import Text.PrettyPrint
import Text.Parsec

import Data.Unique
import Data.Default
import Data.List ( sort, sortBy )
import Data.Ratio ( Ratio )

import System.IO.Unsafe

argFail :: Doc -> Err a
argFail = Left . MsgFail

-- | Initialize an 'ArgInfo' by providing a list of names.  The fields 'argDoc'
-- 'argName' and 'argHeading' can then be manipulated post-mortem, as in
--
-- > inf =(optInfo    [ "i", "insufflation" ])
-- >     { argName    = "INSUFFERABLE"
-- >     , argDoc     = "in the haunted house's harrow"
-- >     , argHeading = "NOT FOR AUGHT"
-- >     }
--
-- Names of one character in length will be prefixed by @-@ on the
-- command-line, while longer names will be prefixed by @--@.
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
  { ident      = unsafePerformIO newUnique
  , absence    = Present ""
  , argDoc     = ""
  , argName    = ""
  , argHeading = defaultHeading
  , posKind    = PosAny
  , optKind    = FlagKind
  , optNames   = map dash names
  , repeatable = False
  }
  where
  defaultHeading
    | names == [] = "ARGUMENTS"
    | otherwise   = "OPTIONS"

  dash "" = error "System.Console.CmdTheLine.Arg.info "
               ++ "recieved empty string as name"
  dash str@[_] = "-"  ++ str
  dash str     = "--" ++ str
{-# NOINLINE optInfo #-}

-- | As 'optInfo' but for positional arguments, which by virtue of their
-- positions require no names.  If a positional argument is created with a
-- name, a run-time error will occur halting any and all program runs.
--
-- If you mean to create an optional argument, use 'optInfo', and be sure to
-- give it a non-empty list of names to avoid these errors.
posInfo :: ArgInfo
posInfo = optInfo []

-- | Create a command-line flag that can appear at most once on the
-- command-line.  Yields @False@ in absence and @True@ in presence.
flag :: ArgInfo -> Term Bool
flag ai =
  if isPos ai
     then error E.errNotPos
     else Term [ai] yield
  where
  yield _ cl = case optArg cl ai of
    []                  -> Right   False
    [( _, _, Nothing )] -> Right   True
    [( _, f, Just v  )] -> argFail $ E.flagValue (text f) (text v)

    (( _, f, _ ) :
     ( _, g, _ ) :
     _           ) -> argFail $ E.optRepeated (text f) (text g)

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
    Just v  -> argFail $ E.flagValue (text f) (text v)

-- | 'vFlag' @v [ ( v1, ai1 ), ... ]@ is an argument that can be present at most
-- once on the command line. It takes on the value @vn@ when appearing as
-- @ain@.
vFlag :: a -> [( a, ArgInfo )] -> Term a
vFlag v assoc = Term (map flag assoc) yield
  where
  flag ( _, a )
    | isPos a   = error E.errNotPos
    | otherwise = a

  yield _ cl = go Nothing assoc
    where
    go mv [] = case mv of
      Nothing       -> Right v
      Just ( _, v ) -> Right v

    go mv (( v, a ) : rest) = case optArg cl a of
      []                  -> go mv rest

      [( _, f, Nothing )] -> case mv of
        Nothing       -> go (Just ( f, v )) rest
        Just ( g, _ ) -> argFail $ E.optRepeated (text g) (text f)

      [( _, f, Just v )]  -> argFail $ E.flagValue (text f) (text v)

      (( _, f, _ ) :
       ( _, g, _ ) :
       _           ) -> argFail $ E.optRepeated (text g) (text f)

-- | 'vFlagAll' @vs assoc@ is as 'vFlag' except that it can be present an
-- infinity of times.  In absence, @vs@ is yielded.  When present, each
-- appearance is collected in-order in the result.
vFlagAll :: Ord a => [a] -> [( a, ArgInfo)] -> Term [a]
vFlagAll vs assoc = Term (map flag assoc) yield
  where
  flag ( _, a ) 
    | isPos a   = error E.errNotOpt
    | otherwise = a { repeatable = True }

  yield _ cl = go [] assoc
    where
    go []  [] = Right vs
    go acc [] = Right . map snd $ sort acc

    go acc (( mv, a ) : rest) = case optArg cl a of
      [] -> go acc rest
      xs -> do
        acc' <- accumulate xs
        go acc' rest
      where
      accumulate assoc = (++ acc) <$> mapM fval assoc
      fval ( pos, f, mv' ) = case mv' of
        Nothing -> Right   ( pos, mv )
        Just v  -> argFail $ E.flagValue (text f) (text v)


--
-- Options
--

parseOptValue :: ArgVal a => String -> String -> Err a
parseOptValue f v = case parser v of
  Left  e -> Left  . UsageFail $ E.optParseValue (text f) e
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
        Nothing   -> argFail $ E.optValueMissing (text f)
        Just optv -> Right   optv

      (( _, f, _ ) :
       ( _, g, _ ) :
       _           ) -> argFail $ E.optRepeated (text g) (text f)

-- | 'opt' @v ai@ is an optional argument that yields @v@ in absence, or an
-- assigned value in presence.  If the option is present, but no value is
-- assigned, it is considered a user-error and usage is printed on exit.
opt :: ArgVal a => a -> ArgInfo -> Term a
opt = mkOpt Nothing

-- | 'defaultOpt' @def v ai@ is as 'opt' except if it is present and no value is
-- assigned on the command-line, @def@ is the result.
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
      xs -> map snd . sort <$> mapM parse xs

    parse ( pos, f, mv' ) = case mv' of
      Just v  -> (,) pos <$> parseOptValue f v
      Nothing -> case vopt of
        Nothing -> argFail $ E.optValueMissing (text f)
        Just dv -> Right   ( pos, dv )

-- | 'optAll' @vs ai@ is like 'opt' except that it yields a list of results in
-- absence and can appear an infinity of times.  The values it is assigned on
-- the command-line are accumulated in the order they appear result.
optAll :: ( ArgVal a, Ord a ) => [a] -> ArgInfo -> Term [a]
optAll = mkOptAll Nothing

-- | 'defaultOptAll' @def vs ai@ is like 'optAll' except that if it is present
-- without being assigned a value, the value @def@ takes its place in the list
-- of results
defaultOptAll :: ( ArgVal a, Ord a ) => a -> [a] -> ArgInfo -> Term [a]
defaultOptAll x = mkOptAll $ Just x


--
-- Positional arguments.
--

parsePosValue :: ArgVal a => ArgInfo -> String -> Err a
parsePosValue ai v = case parser v of
  Left  e -> Left  . UsageFail $ E.posParseValue ai e
  Right v -> Right v

mkPos :: ArgVal a => Bool -> Int -> a -> ArgInfo -> Term a
mkPos rev pos v ai = Term [ai] yield
  where
  ai' = ai { absence = Present . show $ pp v
           , posKind = PosN rev pos
           }
  yield _ cl = case posArg cl ai' of
    []  -> Right v
    [v] -> parsePosValue ai' v
    _   -> error "saw list with more than one member in pos converter"

-- | 'pos' @n v ai@ is an argument defined by the @n@th positional argument
-- on the command-line. If absent the value @v@ is returned.
pos :: ArgVal a => Int -> a -> ArgInfo -> Term a
pos    = mkPos False

-- | 'revPos' @n v ai@ is as 'pos' but counting from the end of the command-line
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
posLeft     = posList . PosL False

-- | 'posRight' @n vs ai@ is as 'posLeft' except yielding all values to the right
-- of the @n@th positional argument.
posRight :: ArgVal a => Int -> [a] -> ArgInfo -> Term [a]
posRight    = posList . PosR False

-- | 'revPosLeft' @n vs ai@ is as 'posLeft' except @n@ counts from the end of the
-- command line to the front.
revPosLeft :: ArgVal a => Int -> [a] -> ArgInfo -> Term [a]
revPosLeft  = posList . PosL True

-- | 'revPosRight' @n vs ai@ is as 'posRight' except @n@ counts from the end of
-- the command line to the front.
revPosRight :: ArgVal a => Int -> [a] -> ArgInfo -> Term [a]
revPosRight = posList . PosR True


--
-- Arguments as terms.
--

absent = map (\ a -> a { absence = Absent })

-- | @required term@ is a term that fails in the 'Nothing' and yields @a@ in
-- the 'Just'.
--
-- This is intended for required positional arguments.  There is nothing
-- stopping you from using it with optional arguments except hopefully your
-- sanity.
required :: Term (Maybe a) -> Term a
required (Term ais yield) = Term ais' yield'
  where
  ais' = absent ais
  yield' ei cl = case yield ei cl of
    Left  e  -> Left  e
    Right mv -> maybe (argFail . E.argMissing $ head ais') Right mv

-- | @nonEmpty term@ is a term that fails if its result is empty. intended
-- for non-empty lists of positional arguments.
nonEmpty :: Term [a] -> Term [a]
nonEmpty (Term ais yield) = Term ais' yield'
  where
  ais' = absent ais
  yield' ei cl = case yield ei cl of
    Left  e  -> Left    e
    Right [] -> argFail . E.argMissing $ head ais'
    Right xs -> Right   xs

-- | @lastOf term@ isa term that fails if its result is empty and evaluates
-- to the last element of the resulting list otherwise.  Intended for lists
-- of flags or options where the last takes precedence.
lastOf :: Term [a] -> Term a
lastOf (Term ais yield) = Term ais yield'
  where
  yield' ei cl = case yield ei cl of
    Left e   -> Left    e
    Right [] -> argFail . E.argMissing $ head ais
    Right xs -> Right   $ last xs


--
-- ArgVal
--


-- | The type of parsers of individual command-line argument values.
type ArgParser  a = String -> Either Doc a

-- | The type of printers of values retrieved from the command-line.
type ArgPrinter a = a -> Doc

decPoint      = string "."
digits        = many1 digit
concatParsers = foldl (liftA2 (++)) $ return []

pInteger  :: ( Read a, Integral a ) => Parsec String () a
pFloating :: ( Read a, Floating a ) => Parsec String () a
pInteger      = read <$> digits
pFloating     = read <$> concatParsers [ digits, decPoint, digits ]

-- | 'fromParsec' @onErr p@ makes an 'ArgParser' from @p@ using @onErr@ to
-- produce meaningful error messages.  On failure, @onErr@ will receive a
-- raw string of the value found on the command-line.
fromParsec :: ( String -> Doc) -> Parsec String () a -> ArgParser a
fromParsec onErr p str = either (const . Left $ onErr str) Right
                       $ parse p "" str

-- | A parser of 'Maybe' values of 'ArgVal' instance's. A convenient default
-- that merely lifts the 'ArgVal' instance's parsed value with 'Just'.
just :: ArgVal a => ArgParser (Maybe a)
just = either Left (Right . Just) . parser

-- | A printer of 'Maybe' values of 'ArgVal'. A convenient default that prints
-- nothing on the 'Nothing' and just the value on the 'Just'.
maybePP :: ArgVal a => ArgPrinter (Maybe a)
maybePP = maybe empty id . fmap pp

-- | A parser of enumerated values conveyed as an association list of
-- @( string, value )@ pairs.  Unambiguous prefixes of @string@ map to
-- @value@.
enum :: [( String, a )] -> ArgParser a
enum assoc str = case T.lookup str trie of
  Right v           -> Right v
  Left  T.Ambiguous -> Left  $ E.ambiguous "enum value" str ambs
  Left  T.NotFound  -> Left  . E.invalidVal (text str) $ text "expected" <+> alts
  where
  ambs = sort $ T.ambiguities trie str
  alts = E.alts $ map fst assoc
  trie = T.fromList assoc

invalidVal = E.invalidVal `on` text

-- | The class of values that can be parsed from the command line. Instances
-- must provide both methods.
class ArgVal a where
  parser  :: ArgParser a
  pp      :: ArgPrinter a

instance ArgVal Bool where
  parser   = fromParsec onErr
           $ (True <$ string "true") <|> (False <$ string "false")
    where
    onErr str = E.invalidVal (text str) $ E.alts [ "true", "false" ]

  pp True  = text "true"
  pp False = text "false"

instance ArgVal (Maybe Bool) where
  parser = just
  pp     = maybePP

instance ArgVal [Char] where
  parser = Right
  pp = text

instance ArgVal (Maybe [Char]) where
  parser = just
  pp     = maybePP

instance ArgVal Int where
  parser = fromParsec onErr pInteger
    where
    onErr str = invalidVal str "expected an integer"
  pp = int

instance ArgVal (Maybe Int) where
  parser = just
  pp     = maybePP

instance ArgVal Integer where
  parser = fromParsec onErr pInteger
    where
    onErr str = invalidVal str "expected an integer"
  pp = integer

instance ArgVal (Maybe Integer) where
  parser = just
  pp     = maybePP

instance ArgVal Float where
  parser = fromParsec onErr pFloating
    where
    onErr str = invalidVal str "expected a floating point number"
  pp = float

instance ArgVal (Maybe Float) where
  parser = just
  pp     = maybePP

instance ArgVal Double where
  parser = fromParsec onErr pFloating
    where
    onErr str = invalidVal str "expected a floating point number"
  pp = double

instance ArgVal (Maybe Double) where
  parser = just
  pp     = maybePP

instance ArgVal (Ratio Integer) where
  parser = fromParsec onErr
         $ read <$> concatParsers [ digits <* spaces
                                  , string "%"
                                  , spaces >> digits
                                  ]
    where
    onErr str =
      invalidVal str "expected a ratio in the form `numerator % denominator'"
  pp = rational

instance ArgVal (Maybe (Ratio Integer)) where
  parser = just
  pp     = maybePP
