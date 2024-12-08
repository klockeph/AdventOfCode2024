module Day07 (day07) where
import Lib (readPosNum, parseListOn)
import Data.Maybe (fromJust)

parse :: String -> (Int, [Int])
parse s = fromJust tryParse
  where
    tryParse = do
        (num, r) <- readPosNum s
        return (num, parseListOn " " $ drop 2 r)

applyOps :: [Int -> Int -> Int] -> [Int] -> Int  -> [Int]
applyOps ops is x = ops <*> is <*> [x]

allOptions :: [Int -> Int -> Int] -> [Int] -> [Int]
allOptions ops l = foldl (applyOps ops) [head l] (tail l)

solvable :: Int -> [Int] -> Int
solvable x l = if x `elem` l then x else 0

solve :: [Int->Int->Int] -> [(Int, [Int])] -> Int
solve ops l = sum $ (\(x, is) -> solvable x $ allOptions ops is) <$> l

concatNums :: Int -> Int -> Int
concatNums x y = read (show x ++ show y)

day07 :: [String] -> IO ()
day07 input = do
--     let input = testinput
    let parsed = parse <$> input
    print $ solve [(*),(+)] parsed
    print $ solve [(*),(+),concatNums] parsed
