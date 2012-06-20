{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
{-# LANGUAGE FlexibleInstances #-}
module System.Console.CmdTheLine.ArgVal
  (
  -- * Parsing values from the command line
    ArgVal(..), ArgParser, ArgPrinter

  -- ** Helpers for instantiating ArgVal
  , fromParsec
  , enum
  -- *** Maybe values
  , just, maybePP
  -- *** List values
  , list, listPP
  -- *** Tuple values
  , pair, pairPP
  , triple, triplePP
  , quadruple, quadruplePP
  , quintuple, quintuplePP
  ) where

import qualified System.Console.CmdTheLine.Err as E
import qualified System.Console.CmdTheLine.Trie as T

import Control.Arrow ( first, (***) )
import Data.Function ( on )
import Data.List     ( sort, unfoldr )
import Data.Ratio    ( Ratio )
import Data.Default

import Control.Applicative hiding ( (<|>), empty )
import Text.Parsec         hiding ( char )
import Text.PrettyPrint

-- | The type of parsers of individual command line argument values.
type ArgParser  a = String -> Either Doc a

-- | The type of printers of values retrieved from the command line.
type ArgPrinter a = a -> Doc

decPoint      = string "."
digits        = many1 digit
concatParsers = foldl (liftA2 (++)) $ return []
sign          = option "" $ string "-"

pInteger  :: ( Read a, Integral a ) => Parsec String () a
pFloating :: ( Read a, Floating a ) => Parsec String () a
pInteger      = read <$> concatParsers [ sign, digits ]
pFloating     = read <$> concatParsers [ sign, digits, decPoint, digits ]

splitOn sep str = ( left, rest' )
  where
  rest' = if rest == [] then rest else tail rest -- Skip the 'sep'.
  ( left, rest ) = span (/= sep) str

-- | 'fromParsec' @onErr p@ makes an 'ArgParser' from @p@ using @onErr@ to
-- produce meaningful error messages.  On failure, @onErr@ will receive a
-- raw string of the value found on the command line.
fromParsec :: ( String -> Doc) -> Parsec String () a -> ArgParser a
fromParsec onErr p str = either (const . Left $ onErr str) Right
                       $ parse p "" str

-- | A parser of 'Maybe' values of 'ArgVal' instances. A convenient default
-- that merely lifts the 'ArgVal' instance's parsed value with 'Just'.
just :: ArgVal a => ArgParser (Maybe a)
just = either Left (Right . Just) . parser

-- | A printer of 'Maybe' values of 'ArgVal' instances. A convenient default
-- that prints nothing on the 'Nothing' and just the value on the 'Just'.
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

-- | @'list' sep@ creates a parser of lists of an 'ArgVal' instance separated
-- by @sep@.
list :: ArgVal a => Char -> ArgParser [a]
list sep str = either (Left . E.element "list" str)
                      Right
                      . sequence $ unfoldr parseElem str
  where
  parseElem []  = Nothing
  parseElem str = Just . first parser $ splitOn sep str

-- | @'listPP' sep@ creates a pretty printer of lists of an 'ArgVal' instance
-- seperated by @sep@.
listPP :: ArgVal a => Char -> ArgPrinter [a]
listPP sep = fsep . punctuate (char sep) . map pp

-- | @'pair' sep@ creates a parser of pairs of 'ArgVal' instances separated
-- by @sep@.
pair :: ( ArgVal a, ArgVal b ) => Char -> ArgParser ( a, b )
pair sep str = do
  case yStr of
    [] -> Left $ E.sepMiss sep str
    _  -> return ()
  case ( eX, eY ) of
    ( Right x, Right y ) -> Right ( x, y )
    ( Left  e, _       ) -> Left $ E.element "pair" xStr e
    ( _,       Left e  ) -> Left $ E.element "pair" yStr e
  where
  ( eX, eY ) = parser *** parser $ xyStr
  xyStr@( xStr, yStr ) = splitOn sep str

-- | @'pairPP' sep@ creates a pretty printer of pairs of 'ArgVal' instances
-- separated by @sep@
pairPP :: ( ArgVal a, ArgVal b ) => Char -> ArgPrinter ( a, b )
pairPP sep ( x, y ) = pp x <> char sep <+> pp y

-- | @'triple' sep@ creates a parser of triples of 'ArgVal' instances separated
-- by @sep@.
triple :: ( ArgVal a, ArgVal b, ArgVal c ) => Char -> ArgParser ( a, b, c )
triple sep str = do
  [ xStr, yStr, zStr ] <-
    if length strs == 3
       then Right strs
       else Left  $ E.sepMiss sep str
  case ( parser xStr, parser yStr, parser zStr ) of
    ( Right x, Right y, Right z ) -> Right ( x, y, z )
    ( Left  e, _     ,  _       ) -> Left $ E.element "pair" xStr e
    ( _,       Left e,  _       ) -> Left $ E.element "pair" yStr e
    ( _,       _     ,  Left e  ) -> Left $ E.element "pair" zStr e
  where
  strs = unfoldr split str

  split []  = Nothing
  split str = Just $ splitOn sep str

-- | @'triplePP' sep@ creates a pretty printer of triples of 'ArgVal' instances
-- separated by @sep@
triplePP :: ( ArgVal a, ArgVal b, ArgVal c ) => Char -> ArgPrinter ( a, b, c )
triplePP sep ( x, y, z ) = pp x <> char sep <+> pp y <> char sep <+> pp z

-- | @'quadruple' sep@ creates a parser of quadruples of 'ArgVal' instances
-- separated by @sep@.
quadruple :: ( ArgVal a, ArgVal b, ArgVal c, ArgVal d ) =>
  Char -> ArgParser ( a, b, c, d )
quadruple sep str = do
  [ xStr, yStr, zStr, wStr ] <-
    if length strs == 4
       then Right strs
       else Left  $ E.sepMiss sep str

  case ( parser xStr, parser yStr, parser zStr, parser wStr ) of
    ( Right x, Right y, Right z, Right w ) -> Right ( x, y, z, w )
    ( Left  e, _     ,  _      , _       ) -> Left $ E.element "pair" xStr e
    ( _,       Left e,  _      , _       ) -> Left $ E.element "pair" yStr e
    ( _,       _     ,  Left e , _       ) -> Left $ E.element "pair" zStr e
    ( _,       _     ,  _      , Left e  ) -> Left $ E.element "pair" wStr e
  where
  strs = unfoldr split str

  split []  = Nothing
  split str = Just $ splitOn sep str

-- | @'quadruplePP' sep@ creates a pretty printer of quadruples of 'ArgVal'
-- instances separated by @sep@
quadruplePP :: ( ArgVal a, ArgVal b, ArgVal c, ArgVal d ) =>
  Char -> ArgPrinter ( a, b, c, d )
quadruplePP sep ( x, y, z, w ) =
  pp x <> char sep <+> pp y <> char sep <+> pp z <> char sep <+> pp w

-- | @'quintuple' sep@ creates a parser of quintuples of 'ArgVal' instances
-- separated by @sep@.
quintuple :: ( ArgVal a, ArgVal b, ArgVal c, ArgVal d, ArgVal e ) =>
  Char -> ArgParser ( a, b, c, d, e )
quintuple sep str = do
  [ xStr, yStr, zStr, wStr, vStr ] <-
    if length strs == 3
       then Right strs
       else Left  $ E.sepMiss sep str
  case ( parser xStr, parser yStr, parser zStr, parser wStr, parser vStr ) of
    ( Right x, Right y, Right z, Right w, Right v ) -> Right ( x, y, z, w, v )
    ( Left  e, _     ,  _      , _      , _       ) ->
      Left $ E.element "pair" xStr e
    ( _,       Left e,  _      , _      , _       ) ->
      Left $ E.element "pair" yStr e
    ( _,       _     ,  Left e , _      , _       ) ->
      Left $ E.element "pair" zStr e
    ( _,       _     ,  _      , Left e , _       ) ->
      Left $ E.element "pair" wStr e
    ( _,       _     ,  _      , _      , Left e  ) ->
      Left $ E.element "pair" vStr e
  where
  strs = unfoldr split str

  split []  = Nothing
  split str = Just $ splitOn sep str

-- | @'quintuplePP' sep@ creates a pretty printer of quintuples of 'ArgVal'
-- instances separated by @sep@
quintuplePP :: ( ArgVal a, ArgVal b, ArgVal c, ArgVal d, ArgVal e ) =>
  Char -> ArgPrinter ( a, b, c, d, e )
quintuplePP sep ( x, y, z, w, v ) =
  pp x <> char sep <+> pp y <> char sep <+> pp z <> char sep <+>
    pp w <> char sep <+> pp v

invalidVal = E.invalidVal `on` text

-- | The class of values that can be parsed from the command line. Instances
-- must provide both 'parser' and 'pp'.
class ArgVal a where
  parser :: ArgParser  a -- ^ A parser of instance values.
  pp     :: ArgPrinter a -- ^ A pretty printer for instance values.

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
         $ read <$> concatParsers [ int <* spaces
                                  , string "%"
                                  , spaces >> int
                                  ]
    where
    int = concatParsers [ sign, digits ]
    onErr str =
      invalidVal str "expected a ratio in the form '<numerator> % <denominator>'"

  pp = rational

instance ArgVal (Maybe (Ratio Integer)) where
  parser = just
  pp     = maybePP
