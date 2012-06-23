{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine
  ( module System.Console.CmdTheLine.Term
  , module System.Console.CmdTheLine.Arg
  , module System.Console.CmdTheLine.ArgVal

  -- * Terms
  -- $term
  , Term()
  , TermInfo(..)
  , Default(..)

  -- * Manpages
  , ManBlock(..)

  -- * Argument information
  , ArgInfo( argDoc, argName, argSection )

  -- * User error reporting
  -- $err
  , Fail(..), HelpFormat(..), Err, ret
  )
  where

import Data.Default
import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.Term
import System.Console.CmdTheLine.Arg
import System.Console.CmdTheLine.ArgVal

import Control.Monad    ( join )

{-$term

  CmdTheLine is centered around the 'Term' Applicative Functor.  It allows us
  to define command line programs like the following.

> import System.Console.CmdTheLine
> import Control.Applicative
>
> -- Define a flag argument under the names '--silent' and '-s'
> silent :: Term Bool
> silent = flag $ optInfo [ "silent", "s" ]
>
> -- Define the 0th positional argument, defaulting to the value '"world"' in
> -- absence.
> greeted :: Term String
> greeted = pos 0 "world" posInfo { argName = "GREETED" }
> 
> hello :: Bool -> String -> IO ()
> hello silent str =
>   if silent
>      then return ()
>      else putStrLn $ "Hello, " ++ str ++ "!"
>
> term :: Term (IO ())
> term = hello <$> silent <*> greeted
> 
> termInfo :: TermInfo
> termInfo = def { termName = "Hello", version = "1.0" }
> 
> main :: IO ()
> main = run ( term, termInfo )

  CmdTheLine then generates usage, help in the form of man-pages, and manages
  all the related tedium of getting values from the command line into our
  program so we can go on thinking in regular Haskell functions.

  See the accompanying examples(including the above) provided under the
  @doc/examples@ directory of the distributed package, or go to
  <http://github.com/eli-frey/cmdtheline> and peruse them there.

-}

{-$err

  There is nothing stopping you from printing and formating your own error
  messages.  However, some of the time you will want more tight integration
  with the library.  That is what 'Fail', the 'Err' monad, and 'ret' are for.

  Here is a snippet of an example program that can be found at
  @doc\/examples\/fail.hs@ in the library distribution tarball, or at
  <http://github.com/eli-frey/cmdtheline>.

> import System.Console.CmdTheLine
> import Control.Applicative
>
> import Text.PrettyPrint ( fsep   -- Paragraph fill a list of 'Doc'.
>                         , text   -- Make a 'String' into a 'Doc'.
>                         , quotes -- Quote a 'Doc'.
>                         , (<+>)  -- Glue two 'Doc' together with a space.
>                         )
>
> import Data.List ( intersperse )
>
> failMsg, failUsage, success :: [String] -> Err String
> failMsg   strs = Left  . MsgFail   . fsep $ map text strs
> failUsage strs = Left  . UsageFail . fsep $ map text strs
> success   strs = Right . concat $ intersperse " " strs
>
> help :: String -> Err String
> help name
>   | any (== name) cmdNames = Left . HelpFail Pager $ Just name
>   | name == ""             = Left $ HelpFail Pager Nothing
>   | otherwise              =
>     Left . UsageFail $ quotes (text name) <+> text "is not the name of a command"
>
> noCmd :: Err String
> noCmd = Left $ HelpFail Pager Nothing

  We can now turn any of these functions into a @Term String@ by lifting into
  'Term' and passing the result to 'ret' to fold the 'Err' monad into the
  library.  Here is an example of what it might look like to do this with @noCmd@.

> noCmdTerm :: Term (Err String)
> noCmdTerm = pure noCmd
>
> prepedNoCmdTerm :: Term String
> prepedNoCmdTerm = ret noCmdTerm
-}

-- | 'ret' @term@ folds @term@'s 'Err' context into the library to be handled
-- internally and as seamlessly as other error messages that are built in.
ret :: Term (Err a) -> Term a
ret (Term ais yield) = Term ais yield'
  where
  yield' ei cl = join $ yield ei cl
