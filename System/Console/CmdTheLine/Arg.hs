{-# LANGUAGE FlexibleInstances #-}
module System.Console.CmdTheLine.Arg where

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.CmdLine
import qualified System.Console.CmdTheLine.Err as E
import Data.Char
import Control.Applicative hiding ( (<|>) )
import Text.PrettyPrint
import Text.Parsec
import Control.Monad.Error

import Data.Unique
import Data.List ( sort, sortBy )
import Data.Ratio ( Ratio )
import System.IO.Unsafe ( unsafePerformIO )

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

flag :: ArgInfo -> Err (Term Bool)
flag ai =
  if isPos ai
     then Left  E.errNotPos
     else Right ( [ai], convert )
  where
  convert _ cl = case optArg cl ai of
    []                  -> Right False
    [( _, _, Nothing )] -> Right True
    [( _, f, Just v  )] -> Left  $ E.flagValue (text f) (text v)

    (( _, f, _ ) :
     ( _, g, _ ) :
     _           ) -> Left $ E.optRepeated (text f) (text g)

flagAll :: ArgInfo -> Err (Term [Bool])
flagAll ai
  | isPos ai  = Left  E.errNotPos
  | otherwise = Right ( [ai'], convert )
  where
  ai' = ai { repeatable = True }

  convert _ cl = case optArg cl ai' of
    [] -> Right []
    xs -> reverse <$> mapM truth xs

  truth ( _, f, mv ) = case mv of
    Nothing  -> Right True
    (Just v) -> Left  $ E.flagValue (text f) (text v)

vFlag :: a -> [( a, ArgInfo )] -> Err (Term a)
vFlag v assoc = do
  assoc' <- reverse <$> mapM flag assoc
  return ( assoc', convert )
  where
  flag ( _, a )
    | isPos a   = Left  E.errNotPos
    | otherwise = Right a

  convert _ cl = go Nothing assoc
    where
    go mv [] = case mv of
      Nothing         -> Right v
      (Just ( _, v )) -> Right v

    go mv (( v, a ) : rest) = case optArg cl a of
      []                  -> go mv rest

      [( _, f, Nothing )] -> case mv of
        Nothing         -> go (Just ( f, v )) rest
        (Just ( g, _ )) -> Left $ E.optRepeated (text g) (text f)

      [( _, f, Just v )]  -> Left $ E.flagValue (text f) (text v)

      (( _, f, _ ) :
       ( _, g, _ ) :
       _           ) -> Left $ E.optRepeated (text g) (text f)

vFlagAll :: Ord a => [a] -> [( a, ArgInfo)] -> Err (Term [a])
vFlagAll vs assoc = do
  assoc' <- reverse <$> mapM flag assoc
  return ( assoc', convert )
  where
  flag ( _, a ) 
    | isPos a   = Left  E.errNotOpt
    | otherwise = Right a { repeatable = True }

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
        Nothing  -> Right ( pos, mv )
        (Just v) -> Left  $ E.flagValue (text f) (text v)

parseOptValue :: ArgVal a => String -> String -> Err a
parseOptValue f v = case parse parser "" v of
  (Left e)  -> Left $ E.optParseValue (text f) (text $ show e)
  (Right v) -> Right v

opt :: ArgVal a => Maybe a -> a -> ArgInfo -> Err (Term a)
opt mv v ai
  | isPos ai  = Left  E.errNotOpt
  | otherwise = Right ( [ai'], convert )
    where
    ai' = ai { absence = Present . show $ pp v
             , optKind = case mv of
                 Nothing   -> OptKind
                 (Just dv) -> OptVal . show $ pp dv
             }
    convert _ cl = case optArg cl ai' of
      []                  -> Right v
      [( _, f, Just v )]  -> parseOptValue f v

      [( _, f, Nothing )] -> case mv of
        Nothing     -> Left $ E.optValueMissing (text f)
        (Just optv) -> Right optv

      (( _, f, _ ) :
       ( _, g, _ ) :
       _           ) -> Left $ E.optRepeated (text g) (text f)

opt' :: ArgVal a => a -> ArgInfo -> Err (Term a)
opt' = opt Nothing

optAll :: ( ArgVal a, Ord a ) => Maybe a -> [a] -> ArgInfo -> Err (Term [a])
optAll mv vs ai
  | isPos ai  = Left E.errNotOpt
  | otherwise = Right ( [ai'], convert )
    where
    ai' = ai { absence    = Present ""
             , repeatable = True
             , optKind    = case mv of
                 Nothing   -> OptKind
                 (Just dv) -> OptVal . show $ pp dv
             }

    convert _ cl = case optArg cl ai' of
      [] -> Right vs
      xs -> map snd . sort <$> mapM parse xs

    parse ( pos, f, mv' ) = case mv' of
      (Just v) -> (,) pos <$> parseOptValue f v
      Nothing  -> case mv of
        Nothing   -> Left $ E.optValueMissing (text f)
        (Just dv) -> Right ( pos, dv )

parsePosValue :: ArgVal a => ArgInfo -> String -> Err a
parsePosValue ai v = case parse parser "" v of
  (Left  e) -> Left  . E.posParseValue ai . text $ show e
  (Right v) -> Right v


--
-- Positional arguments.
--

pos :: ArgVal a => Maybe Bool -> Int -> a -> ArgInfo -> Err (Term a)
pos mRev pos v ai = Right ( [ai], convert )
  where
  rev = maybe False id mRev
  ai' = ai { absence = Present . show $ pp v
           , posKind = PosN rev pos
           }
  convert _ cl = case posArg cl ai' of
    []  -> Right v
    [v] -> parsePosValue ai' v
    _   -> error "saw list with more than one member in pos converter"

posList :: ArgVal a => PosKind -> [a] -> ArgInfo -> Err (Term [a])
posList kind vs ai
  | isOpt ai  = Left E.errNotPos
  | otherwise = Right ( [ai'], convert )
    where
    ai' = ai { posKind = kind }
    convert _ cl = case posArg cl ai' of
      [] -> Right vs
      xs -> mapM (parsePosValue ai') xs

posAll :: ArgVal a => [a] -> ArgInfo -> Err (Term [a])
posAll = posList PosAny

posLeft, posRight :: ArgVal a
                  => Maybe Bool -> Int -> [a] -> ArgInfo -> Err (Term [a])

posLeft mRev pos = posList $ PosL rev pos
  where
  rev = maybe False id mRev

posRight mRev pos = posList $ PosR rev pos
  where
  rev = maybe False id mRev

posLeft', posRight' :: ArgVal a
                    => Int -> [a] -> ArgInfo -> Err (Term [a])
posLeft'  = posLeft  Nothing
posRight' = posRight Nothing


--
-- Arguments as terms.
--

absentError = reverse . map (\ a -> a { absence = Absent })

required :: Term (Maybe a) -> (Term a)
required ( as, convert ) = ( as', convert' )
  where
  as' = absentError as
  convert' ei cl = case convert ei cl of
    (Left  e)  -> Left e
    (Right mv) -> maybe (Left . E.argMissing $ head as') Right mv

nonEmpty :: Term [a] -> (Term [a])
nonEmpty ( as, convert ) = ( as', convert' )
  where
  as' = absentError as
  convert' ei cl = case convert ei cl of
    (Left  e)  -> Left e
    (Right []) -> Left . E.argMissing $ head as'
    (Right xs) -> Right xs

lastOf :: Term [a] -> (Term a)
lastOf ( as, convert ) = ( as, convert' )
  where
  convert' ei cl = case convert ei cl of
    (Left e)   -> Left  e
    (Right []) -> Left  . E.argMissing $ head as
    (Right xs) -> Right $ last xs


--
-- ArgVal
--

type ArgParser = Parsec String ()

decPoint      = string "."
digits        = many1 digit
concatParsers = foldl (liftA2 (++)) $ return []

pInteger  :: ( Read a, Integral a ) => ArgParser a
pFloating :: ( Read a, Floating a ) => ArgParser a
pInteger      = read <$> digits
pFloating     = read <$> concatParsers [ digits, decPoint, digits ]


class ArgVal a where
  parser  :: ArgParser a
  parsers :: ArgParser [a]
  pp      :: a -> Doc
  pps     :: [a] -> Doc

  parsers = sepBy1 parser $ spaces >> Text.Parsec.char ',' >> spaces
  pps     = hsep . map pp

instance ArgVal Bool where
  parser   = (True <$ string "true") <|> (False <$ string "false")
  pp True  = text "true"
  pp False = text "false"

instance ArgVal ([Char]) where
  parser = many1 . satisfy $ not . isSpace
  pp     = text

instance ArgVal Int where
  parser = pInteger
  pp     = int

instance ArgVal Integer where
  parser = pInteger
  pp     = integer

instance ArgVal Float where
  parser = pFloating
  pp     = float

instance ArgVal Double where
  parser = pFloating
  pp     = double

instance ArgVal (Ratio Integer) where
  parser = read <$> concatParsers [ digits <* spaces, string "%", spaces >> digits ]
  pp     = rational
