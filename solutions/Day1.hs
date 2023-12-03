module Day1 (day1) where

import Data.Char (isNumber, isDigit)
import Control.Lens ((??))
import Control.Monad (void)
import qualified Text.Parsec as P
import Data.Either (fromRight)
import Data.Functor (($>))
import Control.Applicative (some)
import Data.Maybe (fromMaybe)
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
              . fromRight []
              . P.parse trebuchetLineParser ""

trebuchetLineParser :: P.Parsec String () [Int]
trebuchetLineParser = fmap concat $ P.many $ do
    digitAtCurPos <- P.lookAhead
                   $ P.choice
                   $ map P.try
                   $ concat
                   [ [ Just . read . pure <$> P.digit ]
                   , map (\(s, n) -> P.string s $> Just n) numstrings
                   , [ P.anyChar $> Nothing ]
                   ]
    void P.anyChar
    remainingDigits <- trebuchetLineParser
    pure $ maybe [] pure digitAtCurPos <> remainingDigits

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
