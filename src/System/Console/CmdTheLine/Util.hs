{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Util
  (
  -- * File path validation
    existsFile, existsDir, existsPath
  , validPath
  ) where

import Control.Applicative
import Text.PrettyPrint

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.Err
import System.Console.CmdTheLine.Term

import Control.Monad.IO.Class ( liftIO )

import System.Directory ( doesFileExist, doesDirectoryExist )
import System.FilePath  ( isValid )

validate :: (String -> IO Bool) -> String -> Term String -> Term String
validate test errStr = ret . fmap check
  where
  check path = do
    isDir <- liftIO $ test path
    if isDir
       then return path
       else msgFail $ no errStr path

-- | 'existsFile' @term@ checks that 'String' in @term@ is a path to an existing
-- file. If it is not, exit with an explanitory message for the user.
existsFile :: Term String -> Term String
existsFile = validate doesFileExist      "file"

-- | 'existsDir' @term@ checks that 'String' in @term@ is a path to an existing
-- directory. If it is not, exit with an explanitory message for the user.
existsDir  :: Term String -> Term String
existsDir  = validate doesDirectoryExist "directory"

-- | 'existsPath' @term@ checks that 'String' in @term@ is a path to an existing
-- file or directory. If it is not, exit with an explanitory message for the
-- user.
existsPath :: Term String -> Term String
existsPath = validate exists             "file or directory"
  where
  exists = liftA2 (||) <$> doesFileExist <*> doesDirectoryExist

-- | 'validPath' @term@ checks that 'String' in @term@ is a valid path under
-- the current opperating system. If it is not, exit with an explanitory
-- message for the user.
validPath :: Term String -> Term String
validPath = ret . fmap check
  where
  check   str = if isValid str then return str else msgFail $ failDoc str
  failDoc str = quotes (text str) <+> text "is not a valid file path."
