{-# LANGUAGE FlexibleContexts #-}

module Day3 where

import Text.Parsec as P
import IO.AdventOfCode (Solution, SolutionPart)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Control.Monad (foldM, guard, (<=<), join)
import Data.List (transpose, intercalate)
import qualified GHC.Arr as A
import GHC.Arr (Array, (!))
import Text.Parsec.Pos (updatePosChar)
import Control.Lens ((??))
import Data.Char (isDigit)
import Control.Applicative (some, Alternative (empty))

-- Types and general functions

newtype PartNumber = PartNumber { partNumberToInt :: Int }
  deriving Show

data Frame a = Frame 
    { cell      :: a 
    , perimeter :: [Maybe a]
    } deriving Show

type FrameColumn = [Maybe Char]

type EngineSchematic = [[FrameColumn]]

type FrameStream = [Frame Char]

frame :: [FrameColumn] -> Frame Char
frame [] = error "invalid schematic"
frame cols = Frame (fromJust $ rows !! 1 !! 1) 
           $ concat [ head rows
                    , [head, last] ?? (rows !! 1)
                    , last rows
                    ]
  where
    rows = transpose $ take 3 cols

schematic :: [String] -> Maybe EngineSchematic
schematic [] = Nothing
schematic ls = do 
    width <- collapse (length <$> ls)
    Just $ map transpose $ slidingWindow 3 $ paddedLines width
  where
    paddedLines :: Int -> [[Maybe Char]]
    paddedLines w = map (surroundWith Nothing)
                  $ surroundWith (nothingLine w) 
                  $ map Just <$> ls

    nothingLine w = replicate w Nothing

    surroundWith x xs = x : xs ++ [x]
    

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow _ [] = []
slidingWindow n a@(x:xs)
  | n > length a  = []
  | n == length a = [a]
  | otherwise = take n a : slidingWindow n xs
    

collapse :: Eq a => [a] -> Maybe a
collapse [] = Nothing
collapse (x:xs) = foldM (\a b -> if a == b then Just a else Nothing) x xs

schematicToList :: EngineSchematic -> FrameStream
schematicToList = concatMap frameFromRow
  where 
    frameFromRow [] = error "invalid schematic"
    frameFromRow r@(_:cs)
       | length cs > 2 = frame r : frameFromRow cs
       | otherwise = pure $ frame r


-- Solution

day3 :: Solution
day3 = (part1, part2)

part1 :: SolutionPart
part1 = show
      . sum
      . map partNumberToInt
      . either (error . show) id
      . P.parse partNumberParser ""
      . schematicToList
      . fromMaybe (error "invalid schematic")
      . schematic

part2 :: SolutionPart
part2 = const "not implemented"

partNumberParser :: P.Parsec FrameStream () [PartNumber]
partNumberParser = do
    partNumbers <- some $ choice [ Just    <$> P.try partNumber
                                 , Nothing <$  satisfy (const True)
                                 ]
    pure $ catMaybes partNumbers
  where 
    partNumber = do
        numberFrames <- some $ satisfy $ isDigit . cell
        if any (any (`elem` "*&@/+#$%=-") . catMaybes . perimeter) numberFrames 
           then pure $ PartNumber $ read $ map cell numberFrames
           else parserFail "no adjacent symbols"

    satisfy c = P.tokenPrim show
                            (\pos f fs -> updatePosChar pos (cell f))
                            (\f -> if c f then Just f else Nothing)

