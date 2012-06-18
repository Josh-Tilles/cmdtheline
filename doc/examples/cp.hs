import System.Console.CmdTheLine
import Control.Applicative

import System.Directory ( copyFile
                        , doesFileExist
                        , doesDirectoryExist
                        )
import System.FilePath  ( takeFileName
                        , pathSeparator
                        , hasTrailingPathSeparator
                        )

import System.IO
import System.Exit

sep = [pathSeparator]

infixr 1 ?
-- Like C's ternary operator. 'predicate ? then-clause $ else-clause'.
-- Nice for nested, if-elseif style boolean bifurcation.
(?) True  = const
(?) False = flip const

cp :: [String] -> String -> IO ()
cp sources dest = choosePath =<< doesDirectoryExist dest
  where
  choosePath isDir = 
    singleton ? singleCopy isDir $
    not isDir ? notDirErr $
    mapM_ copySourcesToDir sources

  singleton = length sources == 1

  withTrailingSep =
    hasTrailingPathSeparator dest ? dest $ dest ++ sep

  notDirErr = do
    hPutStrLn stderr $ "cp: target '" ++ dest ++ "' is not a directory"
    exitFailure

  notFileErr str = do
    hPutStrLn stderr $ "cp: '" ++ str ++ "': No such file"
    exitFailure

  copyToDir filePath =
    copyFile filePath $ withTrailingSep ++ takeFileName filePath

  singleCopy isDir = do
    choosePath =<< doesFileExist filePath
    where
    choosePath isFile =
      isFile && isDir ? copyToDir filePath $
      isFile          ? copyFile  filePath dest $
      notFileErr filePath

    filePath        = head sources

  copySourcesToDir filePath = do
    isFile <- doesFileExist filePath
    isFile ? copyToDir  filePath
           $ notFileErr filePath

cpTerm = cp <$> sources <*> dest
  where
  sources = nonEmpty $ revPosLeft 0 [] posInfo
          { argName = "SOURCES"
          , argDoc  = "Source file(s) to copy"
          }

  dest    = required $ revPos 0 Nothing posInfo
          { argName = "DEST"
          , argDoc  = "Destination of the copy. Must be a directory if there "
                   ++ "is more than one $(i,SOURCE)."
          }

termInfo = def
  { termName = "cp"
  , version  = "v1.0"
  , termDoc  = "copy files from SOURCES to DEST"
  , man      = [ S "BUGS"
               , P "Email bug reports to <portManTwo@example.com>"
               ]
  }

main = run ( cpTerm, termInfo )
