import System.Console.CmdTheLine
import Control.Applicative

import Data.List ( intersperse )

import System.Cmd  ( system )
import System.Exit ( exitWith )

grep :: String -> [String] -> IO ()
grep pattern dests = do
  exitWith =<< system (concat . intersperse " " $ [ "grep", pattern ] ++ dests)

grepTerm = ( grep <$> pattern <*> files, termInfo )
  where
  pattern  = required $ pos 0 Nothing posInfo { argName = "PATTERN" }
  files    = nonEmpty $ posRight 0 [] posInfo { argName = "FILE"    }
  termInfo = def
    { termName = "grep"
    , version  = "2.5"
    , termDoc  = "Search for PATTERN in FILE(s)."
    }

main = run grepTerm
