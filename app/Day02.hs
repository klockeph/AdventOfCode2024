module Day02 (day02) where

readInput :: IO [String]
readInput = lines <$> readFile "data/day2/input.txt"

parseLine :: String -> [Int]
parseLine x = read <$> (words x)

isSafe :: [Int] -> Bool
isSafe xs = let diffs = (\(x,y) -> x-y) <$> zip xs (tail xs) in
        (all (\x -> -3 <= x && x <= -1) diffs || all (\x -> 1 <= x && x <= 3) diffs)

isSafe2 :: [Int] -> Bool
isSafe2 = any isSafe . drops1
    where
        drops1 :: [Int] -> [[Int]]
        drops1 [] = []
        drops1 (a:as) = as:((a :) <$> drops1 as)

solve :: ([Int] -> Bool) -> [String] -> Int
solve f = length . filter f . (parseLine <$>)

day02 :: IO ()
day02 = do
    input <- readInput
    print $ solve isSafe input
    print $ solve isSafe2 input