module IO.AdventOfCode (solves, Solution, SolutionPart) where
import Control.Monad ((<=<))

type SolutionPart = [String] -> String

type Solution = (SolutionPart, SolutionPart)

solves :: Solution -> FilePath -> IO (String, String)
solves (solvePart1, solvePart2) 
  = fmap ((\ls -> (solvePart1 ls, solvePart2 ls)) . lines) 
  . readFile . (<> ".txt") . ("./input/" <>)

