module CmdTheLine.Common ( module Text.PrettyPrint
                         , module Text.Parsec
                         ) where

import qualified Text.PrettyPrint
import qualified Text.Parsec hiding ( char )

descCompare = flip compare
