module IO.AdventOfCode (solves, Solution) where
import Control.Monad ((<=<))

type Solution = [String] -> String

solves :: ([String] -> String) -> FilePath -> IO String
solves solve = fmap (solve . lines) 
             . readFile . (<> ".txt") . ("./input/" <>)

