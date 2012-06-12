{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
{-# LANGUAGE FlexibleInstances #-}
module System.Console.CmdTheLine.Arg where

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.CmdLine
import qualified System.Console.CmdTheLine.Err  as E
import qualified System.Console.CmdTheLine.Trie as T

import Data.Char
import Control.Applicative hiding ( (<|>) )
import Text.PrettyPrint
import Text.Parsec

import Data.Unique
import Data.List ( sort, sortBy )
import Data.Ratio ( Ratio )
import System.IO.Unsafe ( unsafePerformIO )

argFail :: Doc -> Err a
argFail = Left . MsgFail

info :: Maybe String -> Maybe String -> Maybe String -> [String]
     -> ArgInfo
info docTitle docName doc names = ArgInfo
  { ident      = unsafePerformIO newUnique
  , absence    = Present ""
  , doc        = maybe ""           id doc
  , docName    = maybe ""           id docName
  , docTitle   = maybe defaultTitle id docTitle
  , posKind    = PosAny
  , optKind    = FlagKind
  , optNames   = reverse $ map dash names
  , repeatable = False
  }
  where
  defaultTitle
    | names == [] = "ARGUMENTS"
    | otherwise   = "OPTIONS"

  dash str
    | length str == 1 = "-" ++ str
    | otherwise       = "--" ++ str

flag :: ArgInfo -> Term Bool
flag ai =
  if isPos ai
     then error E.errNotPos
     else Term [ai] convert
  where
  convert _ cl = case optArg cl ai of
    []                  -> Right   False
    [( _, _, Nothing )] -> Right   True
    [( _, f, Just v  )] -> argFail $ E.flagValue (text f) (text v)

    (( _, f, _ ) :
     ( _, g, _ ) :
     _           ) -> argFail $ E.optRepeated (text f) (text g)

flagAll :: ArgInfo -> Term [Bool]
flagAll ai
  | isPos ai  = error E.errNotPos
  | otherwise = Term [ai'] convert
  where
  ai' = ai { repeatable = True }

  convert _ cl = case optArg cl ai' of
    [] -> Right []
    xs -> reverse <$> mapM truth xs

  truth ( _, f, mv ) = case mv of
    Nothing -> Right   True
    Just v  -> argFail $ E.flagValue (text f) (text v)

vFlag :: a -> [( a, ArgInfo )] -> Term a
vFlag v assoc = Term (reverse $ map flag assoc) convert
  where
  flag ( _, a )
    | isPos a   = error E.errNotPos
    | otherwise = a

  convert _ cl = go Nothing assoc
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
vFlagAll vs assoc = Term (reverse $ map flag assoc) convert
  where
  flag ( _, a ) 
    | isPos a   = error E.errNotOpt
    | otherwise = a { repeatable = True }

  convert _ cl = go [] assoc
    where
    go []  [] = Right vs
    go acc [] = Right . reverse . map snd $ sortBy descCompare acc

    go acc (( mv, a ) : rest) = case optArg cl a of
      [] -> go acc rest
      xs -> do
        acc' <- accumulate xs
        go acc' rest
      where
      accumulate assoc = reverse . (++ acc) . reverse <$> mapM fval assoc
      fval ( pos, f, mv' ) = case mv' of
        Nothing -> Right   ( pos, mv )
        Just v  -> argFail $ E.flagValue (text f) (text v)


--
-- Options
--

parseOptValue :: ArgVal a => String -> String -> Err a
parseOptValue f v = case parser v of
  Left e  -> Left  . UsageFail $ E.optParseValue (text f) (text e)
  Right v -> Right v

opt :: ArgVal a => Maybe a -> a -> ArgInfo -> Term a
opt mv v ai
  | isPos ai  = error E.errNotOpt
  | otherwise = Term [ai'] convert
    where
    ai' = ai { absence = Present . show $ pp v
             , optKind = case mv of
                 Nothing -> OptKind
                 Just dv -> OptVal . show $ pp dv
             }
    convert _ cl = case optArg cl ai' of
      []                  -> Right v
      [( _, f, Just v )]  -> parseOptValue f v

      [( _, f, Nothing )] -> case mv of
        Nothing   -> argFail $ E.optValueMissing (text f)
        Just optv -> Right   optv

      (( _, f, _ ) :
       ( _, g, _ ) :
       _           ) -> argFail $ E.optRepeated (text g) (text f)

opt' :: ArgVal a => a -> ArgInfo -> Term a
opt' = opt Nothing

optAll :: ( ArgVal a, Ord a ) => Maybe a -> [a] -> ArgInfo -> Term [a]
optAll mv vs ai
  | isPos ai  = error E.errNotOpt
  | otherwise = Term [ai'] convert
    where
    ai' = ai { absence    = Present ""
             , repeatable = True
             , optKind    = case mv of
                 Nothing -> OptKind
                 Just dv -> OptVal . show $ pp dv
             }

    convert _ cl = case optArg cl ai' of
      [] -> Right vs
      xs -> map snd . sort <$> mapM parse xs

    parse ( pos, f, mv' ) = case mv' of
      (Just v) -> (,) pos <$> parseOptValue f v
      Nothing  -> case mv of
        Nothing -> argFail $ E.optValueMissing (text f)
        Just dv -> Right   ( pos, dv )

parsePosValue :: ArgVal a => ArgInfo -> String -> Err a
parsePosValue ai v = case parser v of
  Left  e -> Left  . UsageFail . E.posParseValue ai $ text e
  Right v -> Right v


--
-- Positional arguments.
--

pos :: ArgVal a => Maybe Bool -> Int -> a -> ArgInfo -> Term a
pos mRev pos v ai = Term [ai] convert
  where
  rev = maybe False id mRev
  ai' = ai { absence = Present . show $ pp v
           , posKind = PosN rev pos
           }
  convert _ cl = case posArg cl ai' of
    []  -> Right v
    [v] -> parsePosValue ai' v
    _   -> error "saw list with more than one member in pos converter"

posList :: ArgVal a => PosKind -> [a] -> ArgInfo -> Term [a]
posList kind vs ai
  | isOpt ai  = error E.errNotPos
  | otherwise = Term [ai'] convert
    where
    ai' = ai { posKind = kind }
    convert _ cl = case posArg cl ai' of
      [] -> Right vs
      xs -> mapM (parsePosValue ai') xs

posAll :: ArgVal a => [a] -> ArgInfo -> Term [a]
posAll = posList PosAny

posLeft, posRight :: ArgVal a
                  => Maybe Bool -> Int -> [a] -> ArgInfo -> Term [a]

posLeft mRev pos = posList $ PosL rev pos
  where
  rev = maybe False id mRev

posRight mRev pos = posList $ PosR rev pos
  where
  rev = maybe False id mRev

posLeft', posRight' :: ArgVal a
                    => Int -> [a] -> ArgInfo -> Term [a]
posLeft'  = posLeft  Nothing
posRight' = posRight Nothing


--
-- Arguments as terms.
--

absentError = reverse . map (\ a -> a { absence = Absent })

required :: Term (Maybe a) -> Term a
required (Term as convert) = Term as' convert'
  where
  as' = absentError as
  convert' ei cl = case convert ei cl of
    Left  e  -> Left  e
    Right mv -> maybe (argFail . E.argMissing $ head as') Right mv

nonEmpty :: Term [a] -> Term [a]
nonEmpty (Term as convert) = Term as' convert'
  where
  as' = absentError as
  convert' ei cl = case convert ei cl of
    Left  e  -> Left    e
    Right [] -> argFail . E.argMissing $ head as'
    Right xs -> Right   xs

lastOf :: Term [a] -> Term a
lastOf (Term as convert) = Term as convert'
  where
  convert' ei cl = case convert ei cl of
    Left e   -> Left    e
    Right [] -> argFail . E.argMissing $ head as
    Right xs -> Right   $ last xs


--
-- ArgVal
--

type ArgParser  a = String -> Either String a
type ArgPrinter a = a -> String

decPoint      = string "."
digits        = many1 digit
concatParsers :: [Parsec String () [a]] -> Parsec String () [a]
concatParsers = foldl (liftA2 (++)) $ return []

pInteger  :: ( Read a, Integral a ) => Parsec String () a
pFloating :: ( Read a, Floating a ) => Parsec String () a
pInteger      = read <$> digits
pFloating     = read <$> concatParsers [ digits, decPoint, digits ]

fromParsec :: ( String -> String) -> Parsec String () a -> ArgParser a
fromParsec onErr p str = either (const . Left $ onErr str) Right
                       $ parse p "" str

just :: ArgParser a -> ArgParser (Maybe a)
just p = either Left (Right . Just) . p

enum :: [( String, a )] -> ArgParser a
enum assoc str = case T.lookup str trie of
  Right v           -> Right v
  Left  T.Ambiguous -> Left  $ E.ambiguous "enum value" str ambs
  Left  T.NotFound  -> Left  . E.invalidVal str $ "expected " ++ E.alts alts
  where
  ambs = sort $ T.ambiguities trie str
  alts = map fst assoc
  trie = T.fromList assoc

class ArgVal a where
  parser  :: ArgParser a
  pp      :: ArgPrinter a

instance ArgVal Bool where
  parser   = fromParsec onErr
           $ (True <$ string "true") <|> (False <$ string "false")
    where
    onErr str = E.invalidVal str $ E.alts [ "true", "false" ]

  pp = show

instance ArgVal [Char] where
  parser = Right
  pp = show

instance ArgVal Int where
  parser = fromParsec onErr pInteger
    where
    onErr str = E.invalidVal str "expected an integer"
  pp = show

instance ArgVal Integer where
  parser = fromParsec onErr pInteger
    where
    onErr str = E.invalidVal str "expected an integer"
  pp = show

instance ArgVal Float where
  parser = fromParsec onErr pFloating
    where
    onErr str = E.invalidVal str "expected a floating point number"
  pp = show

instance ArgVal Double where
  parser = fromParsec onErr pFloating
    where
    onErr str = E.invalidVal str "expected a floating point number"
  pp = show

instance ArgVal (Ratio Integer) where
  parser = fromParsec onErr
         $ read <$> concatParsers [ digits <* spaces
                                  , string "%"
                                  , spaces >> digits
                                  ]
    where
    onErr str =
      E.invalidVal str "expected a ratio in the form `numerator % denominator'"
  pp = show
