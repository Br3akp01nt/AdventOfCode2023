module IO.AdventOfCode (solves) where
import Control.Monad ((<=<))

solves :: ([String] -> a) -> FilePath -> IO a
solves solve = fmap (solve . lines) 
             . readFile . (<> ".txt") . ("./input/" <>)

