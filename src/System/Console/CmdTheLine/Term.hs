{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Term
  (
  -- * User error reporting
    ret

  -- * Simple command-line programs
  , eval, exec, run

  -- * Command-based command-line programs
  , evalChoice, execChoice, runChoice
  ) where

import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.CmdLine
import System.Console.CmdTheLine.Arg
import qualified System.Console.CmdTheLine.Err     as E
import qualified System.Console.CmdTheLine.Help    as H
import qualified System.Console.CmdTheLine.Trie    as T

import Control.Applicative hiding ( (<|>), empty )
import Control.Arrow       ( second )
import Control.Monad       ( join )

import Data.List    ( find, sort )
import Data.Maybe   ( fromJust )

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import System.IO

import Text.PrettyPrint
import Text.Parsec


--
-- EvalErr
--


data EvalFail = Help  HelpFormat 
                      (Maybe String) -- The name of the command being run
              | Usage Doc
              | Msg   Doc
              | Version

type EvalErr = Either EvalFail

fromFail :: Fail -> EvalErr a
fromFail (MsgFail   d)         = Left $ Msg   d
fromFail (UsageFail d)         = Left $ Usage d
fromFail (HelpFail  fmt mName) = Left $ Help  fmt mName

fromErr :: Err a -> EvalErr a
fromErr = either fromFail return

printEvalErr :: EvalInfo -> EvalFail -> IO ()
printEvalErr ei (Help fmt mName) = either print (H.print fmt stdout) eEi
  where
  -- Either we are in the default term, or the commands name is in `mName`.
  eEi = maybe (Right ei { term = main ei })
              process
              mName

  -- Either the command name exists, or else it does not and we're in trouble.
  process name = do
    cmd' <- case find (\ ( i, _ ) -> termName i == name) (choices ei) of
      Just x  -> Right x
      Nothing -> Left  $ E.errHelp (text name)
    return ei { term = cmd' } 

printEvalErr ei (Usage doc) = E.printUsage stderr ei doc
printEvalErr ei (Msg   doc) = E.print stderr ei doc

printEvalErr ei Version = H.printVersion stdout ei


--
-- Terms as Applicative Functors
--

instance Functor Term where
  fmap = yield . result . result . fmap
    where
    yield f (Term ais y) = Term ais (f y)
    result = (.)

instance Applicative Term where
  pure v = Term [] (\ _ _ -> Right v)

  (Term args f) <*> (Term args' v) = Term (args ++ args') wrapped
    where
    wrapped ei cl = f ei cl <*> v ei cl


--
-- Standard Options
--

instance ArgVal HelpFormat where
  parser = enum [ ( "pager", Pager )
                , ( "plain", Plain )
                , ( "groff", Groff )
                ]

  pp Pager = text "pager"
  pp Plain = text "plain"
  pp Groff = text "groff"

instance ArgVal (Maybe HelpFormat) where
  parser = just

  pp = maybePP

addStdOpts :: EvalInfo -> ( Yield (Maybe HelpFormat)
                          , Maybe (Yield Bool)
                          , EvalInfo
                          )
addStdOpts ei = ( hLookup, vLookup, ei' )
  where
  ( args, vLookup ) = case version . fst $ main ei of
    "" -> ( [],  Nothing )
    _  -> ( ais, Just lookup )
    where
    Term ais lookup = flag (optInfo ["version"])
                    { argHeading = heading
                    , argDoc     = "Show version information."
                    }

  ( args', hLookup ) = ( ais ++ args, lookup )
    where
    Term ais lookup = defaultOpt (Just Pager) Nothing (optInfo ["help"])
                    { argHeading = heading
                    , argName    = "FMT"
                    , argDoc     = doc
                    }

  heading = stdOptHeading . fst $ term ei'
  doc     = "Show this help in format $(argName) (pager, plain, or groff)."

  addArgs = second (args' ++)
  ei' = ei { term = addArgs $ term ei
           , main = addArgs $ main ei
           , choices = map addArgs $ choices ei
           }
{-
  ais' = zipWith (\ ai n -> ai { ident = n }) ais [1..]
    where
    ais = reverse $ args' ++ snd (term ei)
-}


--
-- Evaluation of Terms
--

evalTerm :: EvalInfo -> Yield a -> [String] -> IO a
evalTerm ei yield args = either handleErr return $ do
  ( cl, mResult ) <- fromErr $ do
    cl      <- create (snd $ term ei') args
    mResult <- helpArg ei' cl

    return ( cl, mResult )

  let success = fromErr $ yield ei' cl

  case ( mResult, versionArg ) of
    ( Just fmt, _         ) -> Left $ Help fmt mName
    ( Nothing,  Just vArg ) -> case vArg ei' cl of
                                    Left  e     -> fromFail e
                                    Right True  -> Left Version
                                    Right False -> success
    _                       -> success
  where
  ( helpArg, versionArg, ei' ) = addStdOpts ei

  mName = if defName == evalName
             then Nothing
             else Just evalName

  defName  = termName . fst $ main ei'
  evalName = termName . fst $ term ei'

  handleErr e = do printEvalErr ei' e
                   exitFailure

chooseTerm :: TermInfo -> [( TermInfo, a )] -> [String]
           -> Err ( TermInfo, [String] )
chooseTerm ti _       []              = Right ( ti, [] )
chooseTerm ti choices args@( arg : rest )
  | length arg > 1 && head arg == '-' = Right ( ti, args )

  | otherwise = case T.lookup arg index of
    Right choice      -> Right ( choice, rest )
    Left  T.NotFound  -> Left . UsageFail $ E.unknown   com arg
    Left  T.Ambiguous -> Left . UsageFail $ E.ambiguous com arg ambs
    where
    index = foldl add T.empty choices
    add acc ( choice, _ ) = T.add acc (termName choice) choice

    com  = "command"
    ambs = sort $ T.ambiguities index arg

mkCommand :: ( Term a, TermInfo ) -> Command
mkCommand ( Term ais _, ti ) = ( ti, ais )
 
-- Prep an EvalInfo suitable for catching errors raised by `chooseTerm`.
chooseTermEi :: ( Term a, TermInfo ) -> [( Term a, TermInfo )] -> EvalInfo
chooseTermEi mainTerm choices = ei
  where
  ei        = EvalInfo command command eiChoices
  eiChoices = map mkCommand choices
  command   = mkCommand mainTerm


--
-- User-Facing Functionality
--

-- | 'ret' @term@ folds @term@'s 'Err' context into the library to be handled
-- internally and seamlesly as other error messages that are built in.
ret :: Term (Err a) -> Term a
ret (Term ais yield) = Term ais yield'
  where
  yield' ei cl = join $ yield ei cl

-- | 'eval' @args ( term, termInfo )@ allows the user to pass @args@ directly to
-- the evaluation mechanism.  This is usefull if some kind of pre-processing is
-- required.  If you do not need to pre-process command-line arguments, use one
-- of 'exec' or 'run'.  On failure the program exits.
eval :: [String] -> ( Term a, TermInfo ) -> IO a
eval args ( term, termInfo ) = evalTerm ei yield args
  where
  (Term ais yield) = term
  ei               = EvalInfo command command []
  command          = ( termInfo, ais )

-- | 'exec' @( term, termInfo )@ executes a command-line program, directly
-- grabing the command-line arguments and returning the result upon succesfull
-- evaluation of @term@.  On failure the program exits.
exec :: ( Term a, TermInfo ) -> IO a
exec term = do
  args <- getArgs
  eval args term

-- | 'run' @( term, termInfo )@ runs a @term@ containing an 'IO' action,
-- performs the action, and returns the result on success. On failure the
-- program exits.
run :: ( Term (IO a), TermInfo ) -> IO a
run term = do
  action <- exec term
  action

-- | 'evalChoice' @args mainTerm choices@ is analagous to 'eval', but for
-- programs that provide a choice of commands.
evalChoice :: [String] -> ( Term a, TermInfo ) -> [( Term a, TermInfo )] -> IO a
evalChoice args mainTerm@( term, termInfo ) choices = do
  ( ei, yield, args' ) <- either handleErr return $ do

    ( chosen, args' ) <- fromErr $ chooseTerm termInfo eiChoices args

    let (Term ais yield) = fst . fromJust $ find ((== chosen) . snd)
                                          $ mainTerm : choices

        ei = EvalInfo ( chosen, ais ) mainEi eiChoices

    return ( ei, yield, args' )

  evalTerm ei yield args'

  where
  eiChoices = map mkCommand choices
  mainEi    = mkCommand mainTerm

  -- Only handles errors caused by chooseTerm.
  handleErr e = do printEvalErr (chooseTermEi mainTerm choices) e
                   exitFailure

-- | Analagous to 'exec', but for programs that provide a choice of commands.
execChoice :: ( Term a, TermInfo ) -> [( Term a, TermInfo )] -> IO a
execChoice main choices = do
  args <- getArgs
  evalChoice args main choices

-- | Analagous to 'run', but for programs that provide a choice of commands.
runChoice :: ( Term (IO a), TermInfo ) -> [( Term (IO a), TermInfo )] -> IO a
runChoice main choices = do
  action <- execChoice main choices
  action
