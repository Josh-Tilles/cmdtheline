module System.Console.CmdTheLine.Err where

import System.Console.CmdTheLine.Common
import Text.PrettyPrint

hsepMap :: (a -> Doc) -> [a] -> Doc
hsepMap f = hsep . map f

doc `leadBy` str = str $+$ nest 0 doc

errArgv     = text "argv array must have at least one element"
errNotOpt   = text "Option argument without name"
errNotPos   = text "Positional argument with a name"
errHelp str = text "term error, help requested for unknown command" <+> text str


alts []    = error "called on empty list"
alts [x]   = error "called on singleton list"
alts [x,y] = sep $ map text [ "either", x, "or", y ]
alts xs    = fsep altDocs `leadBy` text "one of:"
  where
  altDocs = punctuate (char ',') $ map text xs

invalid kind s exp = sep [ text "invalid", kind, quotes s <> char ',', exp ]

invalidVal = invalid $ text "value"

no kind s = sep [ text "no", quotes $ s, kind ]

notDir  s = quotes (s) <+> text "is not a directory"

isDir   s = quotes (s) <+> text "is a directory"

element kind s exp = sep
  [ text "invalid element in", kind, parens . quotes $ s, exp ]

sepMiss sepChar s = invalidVal s $
  hsep [ text "missing a '", char sepChar, text "' separator" ]

unknown kind v = sep [ text "unkown", kind, quotes $ v ]

ambiguous kind s ambs = hsep [ kind
                             , quotes $ s
                             , text "ambiguous, could be"
                             , alts ambs ]

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
argMissing a
  | isOpt a   = hsepMap text [ "required option", longName $ optNames a ]
  | otherwise =
    if name == ""
       then text "a required argument is missing"
       else hsepMap text [ "required argument", name, "is missing" ]
    where
    name = docName a

    longName (x : xs)
      | length x > 2 || xs == [] = x
      | otherwise                = longName xs

{-
print :: Handle -> EvalInfo -> -> IO ()
print h ei e = Prelude.print $ sep [ text fst $ main ei, printText
-}
