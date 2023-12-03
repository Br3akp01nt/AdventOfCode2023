module Day2 (day2) where

import qualified Text.Parsec as P
import Control.Monad (void)
import Control.Applicative (some, (<|>))
import Data.Functor (($>))
import Data.Either (fromRight)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Function (on)
import IO.AdventOfCode (Solution)
import Debug.Trace (trace)

type Handful = [GameCube]

data GameCube = Green | Red | Blue
  deriving (Eq, Show)

data Game = Game
  { gameIndex :: Int
  , gameHandfuls :: [Handful]
  } deriving Show

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

day2 :: Solution
day2 = show 
     . sum
     . map gameIndex
     . filter (isValidGame availableCubes)
     . map parseGame
  where
    availableCubes = concatMap (uncurry replicate) [(12, Red), (13, Green), (14, Blue)]
    parseGame = either (error . show) id . P.parse gameParser ""

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

