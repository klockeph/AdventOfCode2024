module Day07 (day07) where
import Lib (readPosNum, parseListOn)
import Data.Maybe (fromJust)

parse :: String -> (Int, [Int])
parse s = fromJust tryParse
  where
    tryParse = do
        (num, r) <- readPosNum s
        return (num, parseListOn " " $ drop 2 r)

-- multiplies or adds x to every element of the list
addmul :: Int -> [Int] -> [Int]
addmul x is = [(*), (+)] <*> [x] <*> is

allOptions :: [Int] -> [Int]
allOptions l = foldl (flip addmul) [head l] (tail l)

solvable :: Int -> [Int] -> Int
solvable x l = if x `elem` l then x else 0

solve1 :: [(Int, [Int])] -> Int
solve1 l = sum $ (\(x, is) -> solvable x $ allOptions is) <$> l

day07 :: [String] -> IO ()
day07 input = do
    -- let input = testinput
    let parsed = parse <$> input
    print $ solve1 parsed
    return ()