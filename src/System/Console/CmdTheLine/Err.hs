{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
module System.Console.CmdTheLine.Err where

import System.Console.CmdTheLine.Common
import qualified System.Console.CmdTheLine.Help as H

import Text.PrettyPrint
import Data.List ( intersperse )

import System.IO

hsepMap :: (a -> Doc) -> [a] -> Doc
hsepMap f = hsep . map f

doc `leadBy` str = str $+$ nest 0 doc

errArgv     = text "argv array must have at least one element"
errNotOpt   = "Option argument without name"
errNotPos   = "Positional argument with a name"
errHelp doc = text "term error, help requested for unknown command" <+> doc


alts []    = error "called on empty list"
alts [x]   = error "called on singleton list"
alts [x,y] = concat [ "either ", x, " or ", y ]
alts xs    = concat $ [ "one of: "] ++ intersperse ", " xs

invalid kind s exp = concat [ "invalid ", kind, " `", s, "', ", exp ]

invalidVal = invalid "value"

no kind s = sep [ text "no", quotes $ s, kind ]

notDir  s = quotes (s) <+> text "is not a directory"

isDir   s = quotes (s) <+> text "is a directory"

element kind s exp = sep
  [ text "invalid element in", kind, parens . quotes $ s, exp ]

sepMiss sepChar s = invalidVal s $
  concat [ "missing a `", [sepChar], "' separator" ]

unknown kind v = sep [ text "unkown", kind, quotes $ v ]

ambiguous kind s ambs = concat
  [ kind, " `", s, "' ", "ambiguous, could be ", alts ambs ]

posExcess excess = text "too many arguments, don't know what to do with"
               <+> hsepMap prep excess
  where
  prep = (<> text ",") . quotes

flagValue f v = hsep
  [ text "option", quotes f
  , text "is a flag, it cannot take the argument", quotes v
  ]

optValueMissing f = hsep [ text "option", quotes f, text "needs an argument" ]
optParseValue f e = hsep [ text "option", quotes f<>char ':', e ]
optRepeated f f'
  | show f == show f' = hsep
    [ text "option", quotes f, text "cannot be repeated" ]
  | otherwise         = hsep
    [ text "options", quotes f, text "and", quotes f'
    , text "cannot be present at the same time"
    ]

posParseValue :: ArgInfo -> Doc -> Doc
posParseValue ai e
  | docName ai == "" = e
  | otherwise        = case posKind ai of
    (PosN _ _) -> hsep [ name, arg, e ]
    _          -> hsep [ name<>text "...", arg, e ]
    where
    name = text $ docName ai
    arg  = text "arguments:"

argMissing :: ArgInfo -> Doc
argMissing ai
  | isOpt ai  = hsepMap text [ "required option", longName $ optNames ai ]
  | otherwise =
    if name == ""
       then text "a required argument is missing"
       else hsepMap text [ "required argument", name, "is missing" ]
    where
    name = docName ai

    longName (x : xs)
      | length x > 2 || xs == [] = x
      | otherwise                = longName xs

print :: Handle -> EvalInfo -> Doc -> IO ()
print h ei e = hPrint h $ sep [ text . name . fst $ main ei, e ]

prepTryHelp :: EvalInfo -> String
prepTryHelp ei =
  if execName == mainName
     then concat [ "Try `", execName, " --help' for more information." ]
     else concat [ "Try `", execName, " --help' or `"
                 , mainName, " --help' for more information" ]
  where
  execName = H.invocation '-' ei
  mainName = name . fst $ main ei

printUsage :: Handle -> EvalInfo -> Doc -> IO ()
printUsage h ei e = hPrint h $ sep
  [ text $ concat [ name . fst $ main ei, ": " ]
  , e
  , text ","
  , sep [ text "Usage: ", text $ H.prepSynopsis ei ]
  , text ","
  , text $ prepTryHelp ei
  ]
