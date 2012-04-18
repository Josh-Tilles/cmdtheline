module CmdTheLine.Term where

import CmdTheLine.Arg

import Data.Text
import Data.Attoparsec.Text
import Control.Applicative


data TermInfo =
  { name :: String
  , version :: Maybe String
  , tDoc    :: String
  , tDocs   :: String
  , sDocs   :: String
  , man     :: [ManBlock]
  }

type Command = ( TermInfo, [ArgInfo] )

data EvalInfo = EvalInfo
  { term    :: Command
  , main    :: Command
  , choices :: [Command]
  }

type Yield a = EvalInfo -> M.Map ArgInfo Arg -> a

data Term a = ( [ArgInfo], Yield a )

data RERR = Parse | Term | Exn

data Result a = OK a
              | Error RERR
              | Version
              | Help

instance Functor Term where
  fmap = second . result . result
    where
    result = (.)

instance Applicative Term where
  pure x = ( [], \ _ _ -> [x] )

  ( args, f ) <*> ( args', x) = ( rev args ++ args', wrapped )
    where
    wrapped evalInfo cmdline = f evalInfo cmdline $ x evalInfo cmdline

type HelpFmt = EvalInfo -> Doc
type ErrFmt  = RERR -> Doc

addStdOpts :: EvalInfo -> EvalInfo
addStdOpts (EvalInfo t m c) =
  where
  ( docs, _ ) = t
  ( args, vLookup =
    if version $ fst m
       then ( [], Nothing )
       else

evalTerm :: HelpFmt -> ErrFmt -> EvalInfo -> Yield -> [String]
         -> Either String (Result a)
evalTerm helpFmt errFmt evalInfo yield args =

eval :: HelpFmt -> ErrFmt -> [String] -> ( Term a, TermInfo ) -> Result a
eval helpFmt errFmt args ( term, termInfo ) = either handleErr id $ 
  evalTerm helpFmt errFmt evalInfo yield args
  where
  evalInfo           = EvalInfo evalTerm evalTerm []
  evalTerm           = ( termInfo, argInfo )
  ( argInfo, yield ) = term

  handleErr = undefined

eval' :: [String] -> ( Term a, TermInfo ) -> Result a
eval' = eval defaultHelpFmt defaultErrFmt

exec :: ( Term a, TermInfo ) -> IO (Result a)
exec term = eval' <$> getArgs <*> return term
