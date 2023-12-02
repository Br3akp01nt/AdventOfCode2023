module Main where

import Data.Char (isNumber)
import Control.Lens ((??))
import IO.AdventOfCode (solves)
import Control.Monad (forM_)


main :: IO ()
main = do 
    putStrLn "Solutions:"
    solutions <- sequence solvers
    forM_ (zip [1..] solutions) $ \(day, solution) -> 
        putStrLn (show day <> ": " <> show solution)
  where solvers =  
          [ day1
          ]

day1 = (sum . map lineValue) `solves` "day1"
  where
    lineValue = read . ([head, last] ??) . filter isNumber
     
