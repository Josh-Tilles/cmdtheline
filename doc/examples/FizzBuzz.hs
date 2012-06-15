import System.Console.CmdTheLine
import Control.Applicative
import Data.Default

fizzBuzz :: String -> String -> Int -> IO ()
fizzBuzz fizz buzz n = mapM_ fizzOrBuzzOr [1..n]
  where
  fizzOrBuzzOr n = putStrLn output
    where
    output = case fizz' ++ buzz' of
      ""  -> show n
      str -> str

    fizz'
      | (n `mod` 3) == 0 = fizz
      | otherwise        = ""

    buzz'
      | (n `mod` 5) == 0 = buzz
      | otherwise        = ""

fizz, buzz :: Term String
fizz = opt "Fizz" $
      (info   [ "Fizz", "fizz" ])
     { argDoc = "A string to print in the 'Fizz' case." }

buzz = opt "Buzz" $
      (info   [ "Buzz", "buzz" ])
     { argDoc = "A string to print in the 'Buzz' case." }

times :: Term Int
times = opt 100 $
       (info    [ "times", "t" ])
      { argName = "TIMES"
      , argDoc  = "Run FizzBuzz for the numbers 1 to $(argName)."
      }

term :: Term (IO ())
term = fizzBuzz <$> fizz <*> buzz <*> times

termInfo :: TermInfo
termInfo = def
  { termName = "FizzBuzz"
  , version  = "v1.0"
  , termDoc  = "An implementation of the world renowned FizzBuzz algorithm."
  , man      = [ S "BUGS"
               , P "Email bug reports to <eli.lee.frey@gmail.com>"
               ]
  }

main = run ( term, termInfo )
