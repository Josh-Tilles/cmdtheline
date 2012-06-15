{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
{-# LANGUAGE FlexibleInstances #-}
module System.Console.CmdTheLine.Arg
  ( ArgVal(..), ArgParser, ArgPrinter
  , fromParsec, just, maybePP, enum

  , info

  , flag, flagAll, vFlag, vFlagAll
  , opt, defaultOpt, optAll, defaultOptAll
  , pos, revPos, posAny, posLeft, posRight, revPosLeft, revPosRight

  , absent, required, nonEmpty, lastOf
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

info :: [String] -> ArgInfo
info names = ArgInfo
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
{-# NOINLINE info #-}

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

opt :: ArgVal a => a -> ArgInfo -> Term a
opt = mkOpt Nothing

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

optAll :: ( ArgVal a, Ord a ) => [a] -> ArgInfo -> Term [a]
optAll = mkOptAll Nothing

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

pos, revPos :: ArgVal a => Int -> a -> ArgInfo -> Term a
pos    = mkPos False
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

posAny :: ArgVal a => [a] -> ArgInfo -> Term [a]
posAny = posList PosAny

posLeft, posRight, revPosLeft, revPosRight
  :: ArgVal a => Int -> [a] -> ArgInfo -> Term [a]

posLeft     = posList . PosL False
posRight    = posList . PosR False
revPosLeft  = posList . PosL True
revPosRight = posList . PosR True


--
-- Arguments as terms.
--

absent = map (\ a -> a { absence = Absent })

required :: Term (Maybe a) -> Term a
required (Term ais yield) = Term ais' yield'
  where
  ais' = absent ais
  yield' ei cl = case yield ei cl of
    Left  e  -> Left  e
    Right mv -> maybe (argFail . E.argMissing $ head ais') Right mv

nonEmpty :: Term [a] -> Term [a]
nonEmpty (Term ais yield) = Term ais' yield'
  where
  ais' = absent ais
  yield' ei cl = case yield ei cl of
    Left  e  -> Left    e
    Right [] -> argFail . E.argMissing $ head ais'
    Right xs -> Right   xs

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

type ArgParser  a = String -> Either Doc a
type ArgPrinter a = a -> Doc

decPoint      = string "."
digits        = many1 digit
concatParsers = foldl (liftA2 (++)) $ return []

pInteger  :: ( Read a, Integral a ) => Parsec String () a
pFloating :: ( Read a, Floating a ) => Parsec String () a
pInteger      = read <$> digits
pFloating     = read <$> concatParsers [ digits, decPoint, digits ]

fromParsec :: ( String -> Doc) -> Parsec String () a -> ArgParser a
fromParsec onErr p str = either (const . Left $ onErr str) Right
                       $ parse p "" str

just :: ArgVal a => ArgParser (Maybe a)
just = either Left (Right . Just) . parser

maybePP :: ArgVal a => ArgPrinter (Maybe a)
maybePP = maybe empty id . fmap pp

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
