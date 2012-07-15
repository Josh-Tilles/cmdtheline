{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Util
  (
  -- * File path validation
  -- ** Existing path check
    existsFile,  existsDir,  existsPath
  -- ** Existing paths check
  , existsFiles, existsDirs, existsPaths
  -- ** Valid path
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

doesFileOrDirExist :: String -> IO Bool
doesFileOrDirExist = liftA2 (||) <$> doesFileExist <*> doesDirectoryExist

check :: (String -> IO Bool) -> String -> String -> Err String
check test errStr path = do
  isDir <- liftIO $ test path
  if isDir
     then return path
     else msgFail $ no errStr path

validate :: (String -> IO Bool) -> String -> Term String -> Term String
validate test errStr = ret . fmap (check test errStr)

validates :: (String -> IO Bool) -> String -> Term [String] -> Term [String]
validates test errStr = ret . fmap (mapM $ check test errStr)

-- | 'existsFile' @term@ checks that 'String' in @term@ is a path to an existing
-- /file/. If it is not, exit with an explanitory message for the user.
existsFile :: Term String -> Term String
existsFile = validate doesFileExist      "file"

-- | 'existsDir' @term@ checks that 'String' in @term@ is a path to an existing
-- /directory/. If it is not, exit with an explanitory message for the user.
existsDir  :: Term String -> Term String
existsDir  = validate doesDirectoryExist "directory"

-- | 'existsPath' @term@ checks that 'String' in @term@ is a path to an existing
-- /file or directory/. If it is not, exit with an explanitory message for the
-- user.
existsPath :: Term String -> Term String
existsPath = validate doesFileOrDirExist "file or directory"

-- | 'existsFiles' @term@ is as 'existsFile' but for a @term@ containing a list
-- of file paths.
existsFiles :: Term [String] -> Term [String]
existsFiles = validates doesFileExist      "file"

-- | 'existsDirs' @term@ is as 'existsDir' but for a @term@ containing a list
-- of directory paths.
existsDirs  :: Term [String] -> Term [String]
existsDirs  = validates doesDirectoryExist "directory"

-- | 'existsPaths' @term@ is as 'existsPath' but for a @term@ containing a list
-- of paths.
existsPaths :: Term [String] -> Term [String]
existsPaths = validates doesFileOrDirExist "file or directory"

-- | 'validPath' @term@ checks that 'String' in @term@ is a valid path under
-- the current opperating system. If it is not, exit with an explanitory
-- message for the user.
validPath :: Term String -> Term String
validPath = ret . fmap check
  where
  check   str = if isValid str then return str else msgFail $ failDoc str
  failDoc str = quotes (text str) <+> text "is not a valid file path."
