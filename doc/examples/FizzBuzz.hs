import System.Console.CmdTheLine
import Control.Applicative

fizzBuzz :: String -> String -> Int -> IO ()
fizzBuzz fizz buzz n = mapM_ fizzAndBuzzOr [1..n]
  where
  fizzAndBuzzOr n = putStrLn output
    where
    output = case fizz' ++ buzz' of
      ""  -> show n
      str -> str

    fizz' = if (n `mod` 3) == 0 then fizz else ""
    buzz' = if (n `mod` 5) == 0 then buzz else ""

fizz, buzz :: Term String
fizz = opt "Fizz" $ (optInfo [ "Fizz", "fizz", "f" ])
     { argDoc = "A string to print in the 'Fizz' case." }

buzz = opt "Buzz" $ (optInfo [ "Buzz", "buzz", "b" ])
     { argDoc = "A string to print in the 'Buzz' case." }

times :: Term Int
times = opt 100 $ (optInfo [ "times", "t" ])
      { argName = "TIMES"
      , argDoc  = "Run $(mname) for the numbers 1 to $(argName)."
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
