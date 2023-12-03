module Main where
 
import Control.Monad (forM_)
import IO.AdventOfCode (solves)
import Day1 (day1)


main :: IO ()
main = do
    putStrLn "Solutions:"
    solutions <- sequence solvers
    forM_ (zip [1..] solutions) $ \(day, solution) ->
        putStrLn (show day <> ": " <> show solution)
  where solvers =
          [ day1 `solves` "day1"
          ]


