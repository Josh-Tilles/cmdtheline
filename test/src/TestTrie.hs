{- Copyright © 2012, Vincent Elisha Lee Frey.  All rights reserved.
 - This is open source software distributed under a MIT license.
 - See the file 'LICENSE' for further information.
 -}
import Prelude hiding ( lookup )
import System.Console.CmdTheLine.Trie
import Control.Arrow ( first )

assoc = [( "one",        1 )
        ,( "two",        2 )
        ,( "three",      3 )
        ,( "four",       4 )
        ,( "five",       5 )
        ,( "six",        6 )
        ,( "seven",      7 )
        ,( "eight",      8 )
        ,( "nine",       9 )
        ,( "ten",       10 )
        ,( "eleven",    11 )
        ,( "twelve",    12 )
        ,( "thirteen",  13 )
        ,( "fourteen",  14 )
        ,( "fifteen",   15 )
        ,( "sixteen",   16 )
        ,( "seventeen", 17 )
        ,( "eighteen",  18 )
        ,( "nineteen",  19 )
        ]

printResults t ( k, v ) = case lookup k t of
  (Left failure)  -> print failure
  (Right success) ->
    if success == v
       then putStrLn $ concat [ k, " yielded ", show success, "." ]
       else putStrLn $ concat [ k, " yielded ", show success
                              , ", but ", show v, " was expected."
                              ]

main = do
  mapM (printResults t) assoc
  mapM (printResults t) (map (first init) assoc)
  where
  t = fromList assoc
