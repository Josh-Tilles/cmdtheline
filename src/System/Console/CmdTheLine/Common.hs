{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Common where

import Prelude
import Text.PrettyPrint
import Control.Exception
import System.IO
import Data.Function
import System.Environment
import System.Directory
import Data.Unique

import qualified Data.Map as M

data HFormat = Pager | Plain | Groff

data Absence = Absent
             | Present String
               deriving ( Eq )

data OptKind = FlagKind
             | OptKind
             | OptVal String
               deriving ( Eq )

data PosKind = PosAny
             | PosN Bool Int
             | PosL Bool Int
             | PosR Bool Int

data ArgInfo = ArgInfo
  { ident      :: Unique
  , absence    :: Absence
  , doc        :: String
  , docName    :: String
  , docTitle   :: String
  , posKind    :: PosKind
  , optKind    :: OptKind
  , optNames   :: [String]
  , repeatable :: Bool
  }

instance Eq ArgInfo where
  (==) = (==) `on` ident

instance Ord ArgInfo where
  compare = compare `on` ident

data Arg = Opt [( Int, String, Maybe String )]
         | Pos [String]

type CmdLine = M.Map ArgInfo Arg

isOpt, isPos :: ArgInfo -> Bool
isOpt a = optNames a /= []
isPos a = optNames a == []

data ManBlock = S String
              | P String
              | I String String
              | NoBlank
                deriving ( Eq )

type Title = ( String, Int, String, String, String )

type Page = ( Title, [ManBlock] )

data TermInfo = TermInfo
  { name     :: String
  , version  :: Maybe String
  , termDoc  :: String
  , termDocs :: String
  , sDocs    :: String
  , man      :: [ManBlock]
  } deriving ( Eq )

type Command = ( TermInfo, [ArgInfo] )

data EvalInfo = EvalInfo
  { term    :: Command
  , main    :: Command
  , choices :: [Command]
  }

data EvalKind = Simple
              | Main
              | Choice

data Fail = MsgFail   Doc
          | UsageFail Doc

type Err = Either Fail

type Yield a = EvalInfo -> CmdLine -> Err a

data Term a = Term [ArgInfo] (Yield a)

evalKind :: EvalInfo -> EvalKind
evalKind ei
  | choices ei == []               = Simple
  | fst (term ei) == fst (main ei) = Main
  | otherwise                      = Choice

descCompare :: Ord a => a -> a -> Ordering
descCompare = flip compare

printToTempFile :: (Handle -> Page -> IO ()) -> Page
                -> IO (Maybe String)
printToTempFile print v = handle handler $ do
  progName <- getProgName
  tempDir  <- getTemporaryDirectory

  let fileName = tempDir ++ "/" ++ progName ++ ".out"

  h        <- openFile fileName ReadWriteMode

  print h v
  hFlush h

  return $ Just fileName
  where
  handler :: SomeException -> IO (Maybe String)
  handler = const $ return Nothing
