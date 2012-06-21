{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Common where

import Data.Default
import Data.Function    ( on )
import Text.PrettyPrint ( Doc )

import qualified Data.Map as M

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
               deriving ( Eq, Ord )

-- | Information about an argument.  The following fields are exported for your
-- use.
--
-- #argName# 
--
-- [@argName@] :: 'String' A name to be used in the documentation to
-- refer to the argument's value. Defaults to @\"\"@.
--
-- #argDoc# 
--
-- [@argDoc@] :: 'String' A documentation string for the argument.
-- Defaults to @\"\"@.
--
-- #argSection# 
--
-- [@argSection@] :: 'String' The section under which to place the argument's
-- documentation.  Defaults to @\"OPTIONS\"@ for optional arguments and
-- @\"ARGUMENTS\"@ for positional arguments.
data ArgInfo = ArgInfo
  { absence    :: Absence
  , argDoc     :: String
  , argName    :: String
  , argSection :: String
  , posKind    :: PosKind
  , optKind    :: OptKind
  , optNames   :: [String]
  , repeatable :: Bool
  }

instance Eq ArgInfo where
  ai == ai'
    | isPos ai && isPos ai' = ((==) `on` posKind) ai ai'
    | isOpt ai && isOpt ai' = ((==) `on` optNames) ai ai'
    | otherwise             = False

-- This Ord instance works for placing in 'Data.Map's, but not much else.
instance Ord ArgInfo where
  compare ai ai'
    | isPos ai && isPos ai' = (compare `on` posKind) ai ai'
    | isOpt ai && isOpt ai' = (compare `on` optNames) ai ai'
    | isOpt ai && isPos ai' = LT
    | otherwise             = GT


data Arg = Opt [( Int          -- The position were the argument was found.
                , String       -- The name by which the argument was supplied.
                , Maybe String -- If present, a value assigned to the argument.
                )]
         | Pos [String]        -- A list of positional arguments

type CmdLine = M.Map ArgInfo Arg

isOpt, isPos :: ArgInfo -> Bool
isOpt ai = optNames ai /= []
isPos ai = optNames ai == []

{- |
  Any 'String' argument to a 'ManBlock' constructor may contain the
  following significant forms for a limited kind of meta-programing.

  * $(i,text): italicizes @text@.

  * $(b,text): bolds @text@.

  * $(mname): evaluates to the name of the default term if there are choices
    of commands, or the only term otherwise.

  * $(tname): evaluates to the name of the currently evaluating term.

  Additionally, text inside the content portion of an 'I' constructor may
  contain one of the following significant forms.

  * $(argName): evaluates to the name of the argument being documented.

-}
data ManBlock = S String        -- ^ A section title.
              | P String        -- ^ A paragraph.
              | I String String -- ^ A label-content pair. As in an argument
                                --   definition and its accompanying
                                --   documentation.
              | NoBlank         -- ^ Suppress the normal blank line following
                                --   a 'P' or an 'I'.
                deriving ( Eq )

type Title = ( String, Int, String, String, String )

type Page = ( Title, [ManBlock] )

-- | Information about a 'Term'.  It is recommended that 'TermInfo's be
-- created by customizing the 'Data.Default' instance, as in
--
-- > termInfo = def
-- >   { termName = "caroline-no"
-- >   , termDoc  = "carry a line off"
-- >   }
data TermInfo = TermInfo
  {
  -- | The name of the command or program represented by the term. Defaults to
  -- @\"\"@.
    termName      :: String

  -- | Documentation for the term. Defaults to @\"\"@.
  , termDoc       :: String

  -- | The section under which to place the terms documentation.
  -- Defaults to @\"COMMANDS\"@.
  , termSection   :: String

  -- | The section under which to place a term's argument's
  -- documentation by default. Defaults to @\"OPTIONS\"@.
  , stdOptSection :: String

  -- | A version string.  Must be left blank for commands. Defaults to @\"\"@.
  , version       :: String

  -- | A list of 'ManBlock's to append to the default @[ManBlock]@. Defaults
  -- to @[]@.
  , man           :: [ManBlock]
  } deriving ( Eq )

instance Default TermInfo where
  def = TermInfo
    { termName      = ""
    , version       = ""
    , termDoc       = ""
    , termSection   = "COMMANDS"
    , stdOptSection = "OPTIONS"
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

-- | The format to print help in.
data HelpFormat = Pager | Plain | Groff

data Fail =
          -- | An arbitrary message to be printed on failure.
            MsgFail   Doc

          -- | A message to be printed along with the usage on failure.
          | UsageFail Doc

          -- | A format to print the help in and an optional name of the term
          -- to print help for.  If 'Nothing' is supplied, help will be printed
          -- for the currently evaluating term.
          | HelpFail  HelpFormat (Maybe String)

-- | A monad for values in the context of possibly failing with a helpful
-- message.
type Err a = Either Fail a

type Yield a = EvalInfo -> CmdLine -> Err a

-- | The underlying Applicative of the library.  A @Term@ represents a value
-- in the context of being computed from the command line arguments.
data Term a = Term [ArgInfo] (Yield a)

evalKind :: EvalInfo -> EvalKind
evalKind ei
  | choices ei == []               = Simple
  | fst (term ei) == fst (main ei) = Main
  | otherwise                      = Choice

descCompare :: Ord a => a -> a -> Ordering
descCompare = flip compare

splitOn sep xs = ( left, rest' )
  where
  rest' = if rest == [] then rest else tail rest -- Skip the 'sep'.
  ( left, rest ) = span (/= sep) xs
