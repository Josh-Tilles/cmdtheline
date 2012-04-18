module CmdTheLine.Parser where

import Data.Text
import Data.Attoparsec.Text
import Control.Applicative

opt default info = short <|> long
  where
  short = (tok $ char '-' str)
