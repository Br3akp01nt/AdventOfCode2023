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

-- Types and general function

data GameCube = Green | Red | Blue
  deriving (Eq, Show)

type Handful = [GameCube]

data Game = Game
  { gameIndex :: Int
  , gameHandfuls :: [Handful]
  } deriving Show

cubesOfColor :: GameCube -> Handful -> Handful
cubesOfColor = filter . (==)

numberOfColor :: GameCube -> Handful -> Int
numberOfColor c = length . cubesOfColor c

-- Solution

availableCubes :: [GameCube]
availableCubes = concatMap (uncurry replicate) [(12, Red), (13, Green), (14, Blue)]

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
    minGameProduct g = product $ map (`numberOfColor` minimallyShownCubes g) 
                                     [Green, Red, Blue]

minimallyShownCubes :: Game -> [GameCube]
minimallyShownCubes (Game _ handfuls) = concatMap minimallyShownOfColor [Green, Red, Blue]
  where 
    minimallyShownOfColor color = maximumBy (comparing length) 
                                $ map (cubesOfColor color) handfuls

isValidGame :: [GameCube] -> Game -> Bool
isValidGame availableCubes g = 
    all (\c -> ((<=) `on` numberOfColor c) (minimallyShownCubes g) availableCubes)
        [Green, Red, Blue]

parseGame :: String -> Game
parseGame = either (error . show) id 
          . P.parse gameParser ""
  where
    gameParser :: P.Parsec String () Game
    gameParser = (\_ i _ hs -> Game i hs)
             <$> P.string "Game "
             <*> (read <$> some P.digit)
             <*> P.string ": "
             <*> (concat <$> P.manyTill handfulParser P.eof)

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

