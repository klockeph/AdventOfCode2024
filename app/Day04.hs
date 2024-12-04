module Day04 (day04) where

import Lib (readInputForDay)
import Data.Array

parse :: [String] -> Array (Int, Int) Char
parse strings = array ((1, 1), (rows, cols)) elems
  where
    rows = length strings
    cols = length (head strings)
    elems = [((i, j), (strings !! (i - 1)) !! (j - 1)) | i <- [1..rows], j <- [1..cols]]

get :: Array (Int, Int) Char -> (Int, Int) -> Maybe Char
get arr idx
  | inRange (bounds arr) idx = Just (arr ! idx)
  | otherwise = Nothing

(+^) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) +^ (x2, y2) = (x1 + x2, y1 + y2)

(*^) :: Num a => (a, a) -> a -> (a, a)
(x, y) *^ s = (x * s, y * s)

scanForXmas :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Maybe ()
scanForXmas arr pos dir = do
    c1 <- get arr pos
    c2 <- get arr (pos +^ dir)
    c3 <- get arr (pos +^ (dir *^ 2))
    c4 <- get arr (pos +^ (dir *^ 3))
    if c1:c2:c3:c4:[] == "XMAS" then Just () else Nothing

axisDirs = [(-1,0),(0,-1),(1,0),(0,1)]
diagonalDirs = [(-1,-1),(-1,1),(1,-1),(1,1)]
allDirs = axisDirs ++ diagonalDirs

findXmasAt :: Array (Int, Int) Char -> (Int, Int) -> Int
findXmasAt arr pos = length $ filter (== Just ()) $ fmap (scanForXmas arr pos) allDirs

findAllXmas :: Array (Int, Int) Char -> Int
findAllXmas arr = sum $ (\(p,_) ->  findXmasAt arr p) <$> (assocs arr)

scanFor_Mas :: Array (Int, Int) Char -> (Int, Int) -> (Int,Int) -> Maybe ()
scanFor_Mas arr pos dir = do
    a <- get arr pos
    m <- get arr (pos +^ dir)
    s <- get arr (pos +^ (dir *^ (-1)))
    if m:a:s:[] == "MAS" then Just () else Nothing

findX_masAt :: Array (Int, Int) Char -> (Int,Int) -> Int
findX_masAt arr pos =
    if (length $ filter (== Just ()) $ fmap (scanFor_Mas arr pos) diagonalDirs) == 2 then 1 else 0

findAllX_Mas :: Array (Int, Int) Char -> Int
findAllX_Mas arr = sum $ (\(p,_) ->  findX_masAt arr p) <$> (assocs arr)

day04 :: IO ()
day04 = do
    input <- parse <$> readInputForDay 4
    print $ findAllXmas input
    print $ findAllX_Mas input
