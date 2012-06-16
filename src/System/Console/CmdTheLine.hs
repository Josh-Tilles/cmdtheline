{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine
  ( module System.Console.CmdTheLine.Term
  , module System.Console.CmdTheLine.Arg

  -- * Terms
  -- $term
  , Term()
  , TermInfo(..)
  , Default(..)

  -- * Argument information
  -- $arg
  , ArgInfo( argDoc, argName, argHeading )
  , ManBlock(..)

  -- * User error reporting
  -- $err
  , Fail(..), Err, HelpFormat(..)
  )
  where

import Data.Default
import System.Console.CmdTheLine.Common
import System.Console.CmdTheLine.Term
import System.Console.CmdTheLine.Arg

{-$term

  CmdTheLine is centered around the 'Term' Applicative Functor.  It allows us
  to define command-line programs like the following.

>  import System.Console.CmdTheLine
>  import Control.Applicative
>
>  opt1 :: Term A
>  opt1 = ...
>
>  opt2 :: Term B
>  opt2 = ...
>
>  foo :: A -> B -> C
>  foo = ...
>
>  fooTerm :: Term C
>  fooTerm = foo <$> opt1 <*> opt2 <*> opt3
>
>  fooTermInfo = def { termName = "foo", version = "1.0" }
>
>  main :: IO C
>  main = exec ( fooTerm, fooTermInfo )

  CmdTheLine then generates usage, help in the form of man-pages, and manages
  all the related tedium of getting values from the command line into our
  program so we can go on with our day, thinking in terms of regular haskell
  functions.

  See the acompanying examples provided under the @doc/examples@ directory of
  the distributed package, or go to <http://github.com/eli-frey/cmdtheline>
  and peruse them there.
-}

{-$arg


-}

{-$err


-}
