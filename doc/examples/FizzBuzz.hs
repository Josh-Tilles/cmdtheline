import System.Console.CmdTheLine
import Control.Applicative

fizzBuzz :: String -> String -> Int -> IO ()
fizzBuzz fizz buzz n = mapM_ fizzOrBuzzOr [1..n]
  where
  fizzOrBuzzOr n = putStrLn output

  output = case fizz ++ buzz of
    ""  -> show n
    str -> str

  fizz'
    | n `mod` 3 == 0 = "Fizz"
    | otherwise      = ""

  buzz'
    | n `mod` 5 == 0 = "Buzz"
    | otherwise      = ""

fizz = opt Nothing "Fizz"
     $ info Nothing Nothing (Just "A string to print in the `Fizz' case.")
                            [ "Fizz", "fizz" ]

buzz = opt Nothing "Buzz"
     $ info Nothing Nothing (Just "A string to print in the `Buzz' case.")
                            [ "Buzz", "buzz" ]

times = opt Nothing 100
      $ info Nothing
             (Just "Times")
             (Just "An implementation of the renown FizzBuzz algorithm.")
             [ "times", "t" ]

term = fizzBuzz <$> fizz <*> buzz <*> times

termInfo = defaultTermInfo
  { name    = "FizzBuzz"
  , version = Just "1.0"
  }

main = exec' ( term, termInfo )
