{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Term where

import System.Console.CmdTheLine.Common
import qualified System.Console.CmdTheLine.Arg     as A
import qualified System.Console.CmdTheLine.Err     as E
import qualified System.Console.CmdTheLine.Help    as H
import qualified System.Console.CmdTheLine.CmdLine as CmdLine

import Control.Applicative hiding ( (<|>) )
import Control.Arrow       ( second )

import Data.List ( find )

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import System.IO

import Text.PrettyPrint
import Text.Parsec

instance A.ArgVal HFormat where
  parser = A.enum [ ( "pager", Pager )
                  , ( "plain", Plain )
                  , ( "groff", Groff )
                  ]

  pp Pager = "pager"
  pp Plain = "plain"
  pp Groff = "groff"

instance A.ArgVal (Maybe HFormat) where
  parser = A.just A.parser

  pp = maybe "" id . fmap A.pp

data EvalFail = Help  HFormat (Maybe Doc)
              | Usage Doc
              | Msg   Doc
              | Version

type EvalErr = Either EvalFail

fromFail :: Fail -> EvalErr a
fromFail (MsgFail   d) = Left $ Msg   d
fromFail (UsageFail d) = Left $ Usage d

printEvalErr :: EvalInfo -> EvalFail -> IO ()
printEvalErr ei (Help fmt mDoc) = either print (H.print fmt stderr) eEi
  where
  eEi = maybe (Right $ ei { term = main ei })
              process
              mDoc
  process doc = do
    cmd' <- case find (\ ( i, _ ) -> name i == show doc) (choices ei) of
      (Just x) -> Right x
      Nothing  -> Left  $ E.errHelp doc
    return ei { term = cmd' } 

printEvalErr ei (Usage doc) = E.printUsage stderr ei doc
printEvalErr ei (Msg   doc) = E.print stderr ei doc

printEvalErr ei Version = H.printVersion stdout ei

instance Functor Term where
  fmap = (\ f (Term ais yield) -> Term ais (f yield)) . result . result . fmap
    where
    result = (.)

instance Applicative Term where
  pure v = Term [] (\ _ _ -> Right v)

  (Term args f) <*> (Term args' v) = Term (reverse $ args ++ args') wrapped
    where
    wrapped ei cl = f ei cl <*> v ei cl

mainName :: Term String
mainName    = Term [] yield
  where
  yield ei _ = Right . name . fst $ main ei

choiceNames :: Term [String]
choiceNames = Term [] yield
  where
  yield ei _ = Right . reverse . map (name . fst) $ choices ei

manFormat :: Term HFormat
manFormat = A.opt Nothing Pager $ A.info docTitle docName doc ["man-format"]
  where
  docTitle = Nothing
  docName  = Just "FMT"
  doc      = Just "Show output in format $(docName) (pager, plain, or groff)."

addStdOpts :: EvalInfo -> ( Yield (Maybe HFormat)
                          , Maybe (Yield Bool)
                          , EvalInfo
                          )
addStdOpts ei = ( hLookup, vLookup, ei' )
  where
  ( args, vLookup ) = case version . fst $ main ei of
    Nothing -> ( [], Nothing )
    _       -> ( a, Just lookup )
    where
    (Term a lookup) = A.flag $ A.info Nothing
                                      (Just docs)
                                      (Just "Show version information")
                                      ["version"]
  ( args', hLookup ) = ( reverse $ a ++ args, lookup )
    where
    (Term a lookup) = A.opt (Just (Just Pager)) Nothing
                    $ A.info (Just docs) (Just "FMT") (Just doc) ["help"]

  ei'  = ei { term = second (reverse . (args ++)) $ term ei }
  docs = sDocs . fst $ term ei
  doc  = "Show this help in format $(docName) (pager, plain, or groff)."

evalTerm :: EvalInfo -> Yield a -> [String] -> EvalErr a
evalTerm ei yield args = do
    ( cl, mResult ) <- either fromFail Right $ do

      cl      <- CmdLine.create (snd $ term ei) args
      mResult <- helpArg ei' cl

      return ( cl, mResult )

    let success = either fromFail Right $ yield ei' cl

    case ( mResult, versionArg ) of
      ( Just fmt, _ )         -> Left $ Help fmt Nothing
      ( Nothing,  Just vArg ) -> case vArg ei' cl of
                                      (Left e)  -> fromFail e
                                      (Right b) -> if b
                                                      then Left Version
                                                      else success
      _                       -> success
    where
    ( helpArg, versionArg, ei' ) = addStdOpts ei

mkCommand :: ( Term a, TermInfo ) -> Command
mkCommand ( (Term ais _), ti ) = ( ti, ais )

eiMain :: ( Term a, TermInfo ) -> EvalInfo
eiMain main = EvalInfo command command []
  where
  command = mkCommand main

eiChoice :: ( Term a, TermInfo ) -> [( Term a, TermInfo )] -> EvalInfo
eiChoice main choices = EvalInfo main' main' eiChoices
  where
  eiChoices = reverse $ map mkCommand choices
  main'     = mkCommand main

eval :: [String] -> ( Term a, TermInfo ) -> EvalErr a
eval args ( term, termInfo ) = evalTerm ei yield args
  where
  (Term ais yield) = term
  ei               = EvalInfo command command []
  command          = ( termInfo, ais )

eval' :: [String] -> ( Term a, TermInfo ) -> IO a
eval' args term = either handleErr return $ eval args term
  where
  handleErr e = do printEvalErr (eiMain term) e
                   exitFailure

exec :: ( Term a, TermInfo ) -> IO (EvalErr a)
exec term = eval <$> getArgs <*> return term

exec' :: ( Term a, TermInfo ) -> IO a
exec' term = either handleErr return =<< exec term
  where
  handleErr e = do printEvalErr (eiMain term) e
                   exitFailure

defaultTermInfo = TermInfo
  { name     = "???"
  , version  = Nothing
  , termDoc  = ""
  , termDocs = "COMMANDS"
  , sDocs    = "OPTIONS"
  , man      = []
  }
