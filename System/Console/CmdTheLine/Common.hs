module System.Console.CmdTheLine.Common where

import Prelude hiding ( catch )
import Text.PrettyPrint
import Control.Monad.Error
import Control.Exception
import System.IO
import Data.Function
import System.Environment
import System.Directory
import Data.Unique

import qualified Data.Map as M

data Absence = Absent
             | Present String

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

data TermInfo = TermInfo
  { name    :: String
  , version :: Maybe String
  , tDoc    :: String
  , tDocs   :: String
  , sDocs   :: String
  , man     :: [ManBlock]
  } deriving ( Eq )

type Command = ( TermInfo, [ArgInfo] )

data EvalInfo = EvalInfo
  { term    :: Command
  , main    :: Command
  , choices :: [Command]
  }

data EvalKind = Simple
              | MMain
              | MChoice

instance Error Doc where
  strMsg = text

type Err = Either Doc

type Yield a = EvalInfo -> CmdLine -> Err a

type Term a = ( [ArgInfo], Yield a )

evalKind :: EvalInfo -> EvalKind
evalKind ei
  | choices ei == []               = Simple
  | fst (term ei) == fst (main ei) = MMain
  | otherwise                      = MChoice

descCompare :: Ord a => a -> a -> Ordering
descCompare = flip compare

printWhiteStr :: Bool -> Handle -> String -> IO ()
printWhiteStr spaces h s = go 0 0
  where
  len = length s

  go left right
    | right == len                = if left /= len
                                       then flush '\0'
                                       else return ()
    | s !! right == '\n'          = flush '\n'
    | spaces && s !! right == ' ' = flush ' '
    | otherwise                   = go left (right + 1)
    where
    flush char = do
      hPutStr h . take (right - left) $ drop left s
      hPutChar h char
      go right (right + 1)

printText, printLines :: Handle -> String -> IO ()
printText  = printWhiteStr True
printLines = printWhiteStr False

printToTempFile :: (Handle -> String -> IO ()) -> String
                -> IO (Maybe Handle)
printToTempFile pr v = catch writeAndReturn handleException
  where
  handleException :: SomeException -> IO (Maybe Handle)
  handleException = const $ return Nothing

  writeAndReturn :: IO (Maybe Handle)
  writeAndReturn = do
    progName <- getProgName
    tempDir  <- getTemporaryDirectory
    h        <- openFile (tempDir ++ "/" ++ progName ++ ".out")
                         ReadWriteMode
    pr h v
    hFlush h
    return $ Just h
