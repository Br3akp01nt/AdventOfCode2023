module Day2 (day2) where

import qualified Text.Parsec as P
import Control.Monad (void)
import Control.Applicative (some, (<|>))
import Data.Functor (($>))
import Data.Either (fromRight)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Function (on)
import IO.AdventOfCode (Solution, SolutionPart)

type Handful = [GameCube]

data GameCube = Green | Red | Blue
  deriving (Eq, Show)

data Game = Game
  { gameIndex :: Int
  , gameHandfuls :: [Handful]
  } deriving Show

day2 :: Solution
day2 = (part1, part2)

part1 :: SolutionPart
part1 = show 
      . sum
      . map gameIndex
      . filter (isValidGame availableCubes)
      . map parseGame

part2 :: SolutionPart
part2 = show 
      . sum
      . map (minGameProduct . parseGame)
  where
    minGameProduct g = 
      let mc = minShownCubes g
       in product $ map (\color -> length $ filter (== color) mc) 
                        [Green, Red, Blue]

minShownCubes :: Game -> [GameCube]
minShownCubes (Game _ hs) = concatMap maximumSeenColor [Green, Red, Blue] 
  where 
    maximumSeenColor color = maximumBy (comparing length) 
                           $ map (filter (== color)) hs

isValidGame :: [GameCube] -> Game -> Bool
isValidGame availableCubes g = 
    all (\color -> ((<=) `on` (length . filter (== color))) shown availableCubes)
        [Green, Red, Blue]
  where
    shown = minShownCubes g

availableCubes :: [GameCube]
availableCubes = concatMap (uncurry replicate) [(12, Red), (13, Green), (14, Blue)]

parseGame :: String -> Game
parseGame = either (error . show) id 
          . P.parse gameParser ""
  where
    gameParser :: P.Parsec String () Game
    gameParser = do
        void $ P.string "Game "
        i <- read <$> some P.digit
        void $ P.string ": "
        Game i . concat <$> P.manyTill handfulParser P.eof

    handfulParser = flip P.manyTill (void (P.string "; ") <|> void P.eof) $ do
            nCubes <- read <$> some P.digit
            void P.space
            cubeType <- P.choice
                      [ P.string "green" $> Green
                      , P.string "red"   $> Red
                      , P.string "blue"  $> Blue
                      ]
            P.optional $ P.string ", "
            pure $ replicate nCubes cubeType

