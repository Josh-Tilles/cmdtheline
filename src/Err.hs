module CmdTheLine.Err where

import Text.PrettyPrint

doc `leadBy` str = text str $+$ nest 0 doc

errArgv     = text "argv array must have at least one element"
errNotOpt   = text "Option argument without name"
errNotPos   = text "Positional argument with a name"
errHelp str = text "term error, help requested for unknown command" <+> text str


alts []    = error "called on empty list"
alts [x]   = error "called on singleton list"
alts [x,y] = sep $ map text [ "either", x, "or", y ]
alts xs    = fsep altDocs `leadBy` "one of:"
  where
  altDocs = punctuate (char ',') $ map text xs

invalid kind s exp = sep [ text "invalid", kind', quotes s' <> char ',', exp' ]
  where
  kind' = text kind
  s'    = text s
  exp'  = text exp

invalidVal = invalid "value"

no kind s = sep [ text "no", quotes $ text s, text kind ]

notDir  s = quotes (text s) <+> text "is not a directory"

isDir   s = quotes (text s) <+> text "is a directory"

element kind s exp = sep
  [ text "invalid element in", text kind, parens . quotes $ text s, text exp ]

sepMiss sepChar s = invalidVal s $
  concat [ "missing a '", return sepChar, "' separator" ]

unknown kind v = sep [ text "unkown", text kind, quotes $ text v ]

ambiguous kind s ambs = hsep [ text kind
                             , quotes $ text s
                             , text "ambiguous, could be"
                             , alts ambs ]
