module Main where
 
import Control.Monad (forM_)
import IO.AdventOfCode (solves, Solution)
import Day1 (day1)
import Day2 (day2)


main :: IO ()
main = do
    putStrLn "Solutions:"
    solutions <- sequence solvers
    forM_ (zip [1..] solutions) $ \(day, (solvedPart1, solvedPart2)) -> do
        putStrLn (show day <> ".1: " <> solvedPart1)
        putStrLn (show day <> ".2: " <> solvedPart2)
  where 
      solvers =
          [ day1 `solves` "day1"
          , day2 `solves` "day2"
          ]


