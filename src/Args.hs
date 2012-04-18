module CmdTheLine.Args ( ArgInfo ) where

import CmdTheLines.Common
import Data.Char

data Absence = Absent | Present String

data OptKind = Flag | Opt | OptVal String

type Pos     = Int
data PosKind = PosAny
             | PosN Bool Pos
             | PosL Bool Pos
             | PosR Bool Pos

data Arg     = O [( Pos, (Maybe String) )]
             | P [String]

data ArgInfo =
  { id        :: Int
  , absence   :: Absence
  , doc       :: String
  , docv      :: String
  , docs      :: String
  , posKind   :: PosKind
  , optKind   :: OptKind
  , optNames  :: [String]
  }

decPoint      = string "."
digits        = many1 digit
concatParsers = foldl (liftA2 (++)) []

pInteger      = read <$> digits
pFloating     = read <$> concatParsers [ digits, decPoint, digits ]

class ArgVal a where
  parser  :: Parser a
  parsers :: Parser [a]
  pp      :: a -> Doc
  pps     :: [a] -> Doc

  parsers = sepBy1 parser $ spaces >> Text.Parsec.char ',' >> spaces
  pps     = vcat . map pp

instance ArgVal Bool where
  parser = (True <$ string "true") <|> (False <$ string "false")
  pp     = bool

instance ArgVal String where
  parser = many1 . satisfy $ not isSpace
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

instance ArgVal Rational where
  parser = read <$> concatParsers [ digits, spaces, string "%", spaces, digits ]
  pp     = rational

newtype Arg
info :: Maybe String -> Maybe String -> [String] -> Arg a
