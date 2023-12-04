module Day1 (day1) where

import Data.Char (isDigit)
import Control.Lens ((??))
import Control.Monad (void)
import qualified Text.Parsec as P
import Control.Applicative (some, Alternative (many))
import Data.Maybe (catMaybes)
import IO.AdventOfCode (Solution, SolutionPart)

day1 :: Solution
day1 = (part1, part2)

part1 :: SolutionPart
part1 = show . sum . map lineValue
  where
    lineValue = read
              . ([head, last] ??)
              . filter isDigit

part2 :: SolutionPart
part2 = show . sum . map lineValue
  where
    lineValue = read
              . ([head, last] ??)
              . concatMap show
              . either (error . show) id
              . P.parse trebuchetLineParser ""

trebuchetLineParser :: P.Parsec String () [Int]
trebuchetLineParser = fmap catMaybes 
                    $ many 
                    $ P.choice [ Just <$> stepLookAhead stringDigit
                               , Just . read <$> some P.digit 
                               , Nothing <$ P.anyChar 
                               ]

stepLookAhead :: P.Parsec String m a -> P.Parsec String m a
stepLookAhead p = fst <$> ((,) <$> P.lookAhead p <*> P.anyChar)

stringDigit :: P.Parsec String m Int
stringDigit = P.choice $ (\(s, n) -> n <$ P.try (P.string s)) <$> numstrings
  where
    numstrings =
        [ ("one",    1)
        , ("two",    2)
        , ("three",  3)
        , ("four",   4)
        , ("five",   5)
        , ("six",    6)
        , ("seven",  7)
        , ("eight",  8)
        , ("nine",   9)
        , ("ten",   10)
        ]

