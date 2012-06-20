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

cp :: Bool -> [String] -> String -> IO ()
cp dry sources dest =
  choose =<< doesDirectoryExist dest
  where
  choose isDir = 
    singleFile ? singleCopy isDir $
    not isDir  ? notDirErr $
    mapM_ copySourcesToDir sources

  singleFile = length sources == 1

  notDirErr = do
    hPutStrLn stderr $ "cp: target '" ++ dest ++ "' is not a directory"
    exitFailure

  notFileErr str = do
    hPutStrLn stderr $ "cp: '" ++ str ++ "': no such file"
    exitFailure

  singleCopy isDir = do
    choose =<< doesFileExist filePath
    where
    choose isFile =
      isFile && isDir ? copyToDir filePath $
      isFile          ? copyFile  filePath dest $
      notFileErr filePath

    filePath = head sources

  copyToDir filePath = if dry
    then putStrLn $ concat [ "cp: copying ", filePath, " to ", dest ]
    else copyFile filePath dest
    where
    dest = underDest filePath

  underDest filePath = withTrailingSep ++ takeFileName filePath
    where
    withTrailingSep = hasTrailingPathSeparator dest ? dest $ dest ++ sep

  copyToFile filePath = if dry
    then putStrLn $ concat [ "cp: copying ", filePath, " to ", dest ]
    else copyFile filePath dest

  copySourcesToDir filePath = if dry
    then return ()
    else do isFile <- doesFileExist filePath
            isFile ? copyToDir  filePath
                   $ notFileErr filePath

cpTerm = cp <$> dry <*> sources <*> dest
  where
  dry = flag (optInfo [ "dry", "d" ])
      { argName = "DRY"
      , argDoc  = "Perform a dry run.  Print what would be copied, but do not "
               ++ "copy it."
      }

  sources = nonEmpty $ revPosLeft 0 [] posInfo
          { argName = "SOURCES"
          , argDoc  = "Source file(s) to copy."
          }

  dest    = required $ revPos 0 Nothing posInfo
          { argName = "DEST"
          , argDoc  = "Destination of the copy. Must be a directory if there "
                   ++ "is more than one $(i,SOURCE)."
          }

termInfo = def
  { termName = "cp"
  , version  = "v1.0"
  , termDoc  = "Copy files from SOURCES to DEST."
  , man      = [ S "BUGS"
               , P "Email bug reports to <portManTwo@example.com>"
               ]
  }

main = run ( cpTerm, termInfo )
