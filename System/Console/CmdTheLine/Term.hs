module System.Console.CmdTheLine.Term where

import CmdTheLine.Arg

import Data.Text
import Data.Attoparsec.Text
import Control.Applicative

data ResultErr = Parse | Term | Exn

data Result a = OK a
              | RError  ResultErr
              | Version EvalInfo
              | RHelp   EvalInfo

data HFormat = Pager | Plain | Groff

instance ArgValue HFormat where
  parser =  Pager <$ P.string "pager"
        <|> Plain <$ P.string "plain"
        <|> Groff <$ P.string "groff"

data EvalFail = Help      HFormat (Maybe Doc)
              | EvalError Bool    Doc
              | Version   Doc
              | Msg       Doc

display :: EvalInfo -> EvalFail -> IO ()
display ei (Help fmt mDoc) = either print (H.print fmt) eEi
  where
  eEi = maybe (Right $ ei { term = main ei })
              process
              mDoc
  process doc = ei { term = cmd' }
    where
    cmd' = case find (\ ( i, _ ) -> name i == show cmd) (choices ei) of
      (Just x) -> Right x
      Nothing  -> Left  $ errHelp doc

display ei (EvalError usage doc)
  | usage     = E.
  | otherwise = 

instance Error EvalFail where
  strMsg = EvalError True

type EvalErr = Either EvalFail

instance Functor Term where
  fmap = second . result . result . fmap
    where
    result = (.)

instance Applicative Term where
  pure v = ( [], \ _ _ -> Right v )

  ( args, f ) <*> ( args', v) = ( reverse $ args ++ args', wrapped )
    where
    wrapped ei cl = f ei cl $ v ei cl

info :: Maybe String -> Maybe [ManBlock] -> Maybe String -> Maybe String
     -> Maybe String -> String -> TermInfo
info sdocs man docs doc version name = TermInfo name version doc docs sdocs man

mainName, chaiceNames :: Term a
mainName    = ( [], \ ei _ -> name . fst $ main ei )
choiceNames = ( [], \ ei _ -> reverse . map (name . fst) $ choices ei )

manFormat :: Err (Term a)
manFormat = Arg.opt Pager $ Arg.info docTitle docName doc ["man-format"]
  where
  docTitle = Nothing
  docName  = Just "FMT"
  doc      = Just "Show output in format $(docv) (pager, plain, or groff)."

addStdOpts :: EvalInfo -> ( Yield a, Yield a, EvalInfo )
addStdOpts ei = ( hLookup, vLookup, ei' )
  where
  ei' = ei { term = second (reverse . (args ++)) $ term ei }

  docs = fst $ term ei

  ( args, vLookup ) = case version . fst $ main ei of
    Nothing -> ( [], Nothing )
    _       -> ( a, Just lookup )
    where
    ( a, lookup ) = Arg.flag $ Arg.info Nothing
                                          (Just docs)
                                          (Just "Show version information")
  ( args', hLookup ) = reverse $ a ++ args, lookup
    where
    ( a, lookup ) = Arg.opt (Just (Just Pager)) Nothing
                  $ Arg.info (Just docs) (Just "FMT") doc
    doc = "Show this help in format $(docv) (pager, plain, or groff)."

evalTerm :: EvalInfo -> Yield -> [String] -> EvalErr (Result a)
evalTerm ei yield args = either Msg id result
  where
  result = do
    cl      <- CmdLine.create (snd $ term ei) args
    mResult <- helpArg ei' cl
    return $ process cl result

  process cl result = case ( result, versArg ) of
    ( Just fmt, _ ) -> Help.print fmt ei' >> Right RHelp

  ( helpArg, versArg, ei' ) = addStdOpts ei
  cl                        = CmdLine.create (snd $ term ei') args

eval :: [String] -> ( Term a, TermInfo ) -> Result a
eval args ( term, termInfo ) = either handleErr id $ 
  evalTerm helpFmt errFmt evalInfo yield args
  where
  evalInfo           = EvalInfo evalTerm evalTerm []
  evalTerm           = ( termInfo, argInfo )
  ( argInfo, yield ) = term

  handleErr = undefined

exec :: ( Term a, TermInfo ) -> IO (Result a)
exec term = eval' <$> getArgs <*> return term
