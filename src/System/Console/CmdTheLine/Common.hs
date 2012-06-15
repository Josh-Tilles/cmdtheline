{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Common where

import Data.Unique
import Data.Function    ( on )
import Text.PrettyPrint ( Doc )

import qualified Data.Map as M

data HFormat = Pager | Plain | Groff

data Absence = Absent
             | Present String
               deriving ( Eq, Show )

data OptKind = FlagKind
             | OptKind
             | OptVal String
               deriving ( Eq, Show )

data PosKind = PosAny
             | PosN Bool Int
             | PosL Bool Int
             | PosR Bool Int
               deriving ( Show )

instance Show Unique where
  show = show . hashUnique

data ArgInfo = ArgInfo
  { ident      :: Unique
  , absence    :: Absence
  , argDoc     :: String
  , argName    :: String
  , argHeading :: String
  , posKind    :: PosKind
  , optKind    :: OptKind
  , optNames   :: [String]
  , repeatable :: Bool
  } deriving ( Show )

instance Eq ArgInfo where
  (==) = (==) `on` ident

instance Ord ArgInfo where
  compare = compare `on` ident

data Arg = Opt [( Int, String, Maybe String )]
         | Pos [String]
           deriving ( Show )

type CmdLine = M.Map ArgInfo Arg

isOpt, isPos :: ArgInfo -> Bool
isOpt a = optNames a /= []
isPos a = optNames a == []

data ManBlock = S String
              | P String
              | I String String
              | NoBlank
                deriving ( Eq, Show )

type Title = ( String, Int, String, String, String )

type Page = ( Title, [ManBlock] )

data TermInfo = TermInfo
  { termName      :: String
  , termDoc       :: String
  , termHeading   :: String
  , stdOptHeading :: String
  , version       :: String
  , man           :: [ManBlock]
  } deriving ( Eq, Show )

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
          | HelpFail  HFormat (Maybe String)

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
