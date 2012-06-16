{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Common where

import Data.Default
import Data.Unique
import Data.Function    ( on )
import Text.PrettyPrint ( Doc )

import qualified Data.Map as M

-- | The format to print help in.
data HelpFormat = Pager | Plain | Groff

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

-- | Information about an argument.  The following fields are exported for your
-- use.
--
-- [@argName@] :: 'String' A name to be used in the documentation to refer
-- to the argument's value. Defaults to @\"\"@.
--
-- [@argDoc@] :: 'String' A documentation string for the argument. Defaults
-- to @\"\"@.
--
-- [@argHeading@] :: 'String' The heading under which to place the argument's
-- documentation.  Defaults to @\"OPTIONS\"@ for optional arguments and
-- @\"ARGUMENTS\"@ for positional arguments.
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
  }

instance Eq ArgInfo where
  (==) = (==) `on` ident

instance Ord ArgInfo where
  compare = compare `on` ident

data Arg = Opt [( Int          -- The position were the argument was found.
                , String       -- The name by which the argument was supplied.
                , Maybe String -- If present, a value assigned to the argument.
                )]
         | Pos [String]        -- A list of positional arguments

type CmdLine = M.Map ArgInfo Arg

isOpt, isPos :: ArgInfo -> Bool
isOpt a = optNames a /= []
isPos a = optNames a == []

data ManBlock = S String        -- ^ A section heading.
              | P String        -- ^ A paragraph.
              | I String String -- ^ A label-content pair. As in an argument
                                --   definition and its acompanying
                                --   documentation.
              | NoBlank         -- ^ Suppress the normal blank line following
                                --   a 'P' or an 'I'.
                deriving ( Eq )

type Title = ( String, Int, String, String, String )

type Page = ( Title, [ManBlock] )

-- | Information about a 'Term'.  It is recomended that 'TermInfo's be
-- created by customizing the 'Data.Default' instance, as in
--
-- > termInfo = def
-- >   { termName = "caroline-no"
-- >   , termDoc  = "carry a line off"
-- >   }
data TermInfo = TermInfo
  { termName      :: String     -- ^ The name of the command or program
                                --   represented by the term. Defaults to
                                --   @\"\"@.
  , termDoc       :: String     -- ^ Documentation for the term. Defaults to
                                --   @\"\"@.
  , termHeading   :: String     -- ^ The heading under which to place the terms
                                --   documentation. Defaults to @\"COMMANDS\"@.
  , stdOptHeading :: String     -- ^ The Heading under which to place a term's
                                --   argument's documentation by default.
                                --   Defaults to @\"OPTIONS\"@.
  , version       :: String     -- ^ A version string.  Must be left blank for
                                --   commands. Defaults to @\"\"@.
  , man           :: [ManBlock] -- ^ A list of 'ManBlock's to append to the
                                --   default @[ManBlock]@. Defaults to @[]@.
  } deriving ( Eq )

instance Default TermInfo where
  def = TermInfo
    { termName      = ""
    , version       = ""
    , termDoc       = ""
    , termHeading   = "COMMANDS"
    , stdOptHeading = "OPTIONS"
    , man           = []
    }

type Command = ( TermInfo, [ArgInfo] )

data EvalInfo = EvalInfo
  { term    :: Command   -- The chosen term for this run.
  , main    :: Command   -- The default term.
  , choices :: [Command] -- A list of command-terms.
  }

data EvalKind = Simple   -- The program has no commands.
              | Main     -- The default program is running.
              | Choice   -- A command has been chosen.

data Fail = MsgFail   Doc   -- ^ An arbitrary message to be printed on failure.
          | UsageFail Doc   -- ^ A message to be printed along with the usage
                            --   on failure.

          -- | A format to print help in, and the name of the command that
          -- failed if applicable.
          | HelpFail  HelpFormat (Maybe String) 

-- | A monad for errors that can be handled seamlessly by the library.
-- 'Left' your `Fail`ure messages into 'Err', and fold them into the
-- library by processing your 'Term' with 'System.Console.CmdTheLine.Term.ret'
-- before passing it to 'System.Console.CmdTheLine.eval' and friends.
type Err = Either Fail

type Yield a = EvalInfo -> CmdLine -> Err a

-- | The underlying Applicative of the library.  A @Term@ represents a value
-- in the context of being computed from the command-line arguments.
data Term a = Term [ArgInfo] (Yield a)

evalKind :: EvalInfo -> EvalKind
evalKind ei
  | choices ei == []               = Simple
  | fst (term ei) == fst (main ei) = Main
  | otherwise                      = Choice

descCompare :: Ord a => a -> a -> Ordering
descCompare = flip compare
