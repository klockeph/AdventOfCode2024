module Day06 (day06) where

import Prelude hiding (Left, Right)
import Control.Monad.State
import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set

import Lib (readInputForDay,(+^))

type Pos = (Int, Int)
data Dir = Up | Left | Down | Right deriving (Eq, Show)

turnRight :: Dir -> Dir
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

delta :: Dir -> (Int, Int)
delta Up = (-1, 0)
delta Right = (0, 1)
delta Down = (1, 0)
delta Left = (0, -1)

data Guard = Guard {
    pos :: Pos
  , dir :: Dir
} deriving Show

data Tile = Free | Blocked deriving (Eq, Show)

type Map = Array (Int, Int) Tile

-- ChatGPT generated this method and I'm fascinated!
parseInput :: [String] -> (Map, Guard)
parseInput input =
      let
        -- Determine the bounds of the array.
        rows = length input
        cols = length (head input)
        bounds = ((0, 0), (rows - 1, cols - 1))

        -- Parse the grid into tiles and locate the guard.
        parseLine y line = [( (y, x), parseChar x y char ) | (x, char) <- zip [0..] line]
        parseChar x y '^' = (Free, Just (Guard (y, x) Up))
        parseChar x y '.' = (Free, Nothing)
        parseChar x y '#' = (Blocked, Nothing)

        -- Flattened list of grid data and potential guard info.
        parsedData = concatMap (\(y, line) -> parseLine y line) (zip [0..] input)

        -- Separate tiles and guard information.
        tiles = [(pos, tile) | (pos, (tile, _)) <- parsedData]
        guardInfo = [guard | (_, (_, Just guard)) <- parsedData]

        -- Ensure there's exactly one guard.
        guard = case guardInfo of
            [g] -> g
            _   -> error "Invalid input: must contain exactly one guard."
    in
        (array bounds tiles, guard)

-- probably not worth it to use a State Monad here but ... I learned something
walkGuard :: State (Maybe Guard, Map, Set Pos) (Set Pos)
walkGuard= do
  (guard, m, visited) <- get
  case guard of
    Nothing -> return (visited)
    Just g -> do
      let (y, x) = pos g
      let nextPos = (pos g) +^ (delta $ dir g)
      if inBounds m nextPos then 
        if m ! nextPos == Free then 
            put (Just $ g {pos = nextPos}, m, Set.insert nextPos visited)
        else
          put (Just $ g {dir = turnRight $ dir g}, m, visited)
      else 
        put (Nothing, m, visited)
      walkGuard

testinput = [ "....#....."
            , ".........#"
            , ".........."
            , "..#......."
            , ".......#.."
            , ".........."
            , ".#..^....."
            , "........#."
            , "#........."
            , "......#..."
            ]

day06 :: [String] -> IO ()
day06 input = do
  let (map, guard) = parseInput input
  print $ length $ evalState walkGuard (Just guard, map, Set.empty)

inBounds :: Map -> Pos -> Bool
inBounds map (y, x) =
    let ((minY, minX), (maxY, maxX)) = bounds map
    in minY <= y && y <= maxY && minX <= x && x <= maxX
  