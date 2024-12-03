module Day01 (day01) where

import Data.List ( sort )

readInput :: IO [String]
readInput = lines <$> readFile "data/day1/input.txt"

parseOneLine :: String -> (Int, Int)
parseOneLine str =
    case words str of
        [a, b] -> (read a, read b)
        _ -> error $ "Unexpected line " ++ str

parseInput :: [String] -> ([Int], [Int])
parseInput = foldr (\str (f,s) -> let (a,b) = parseOneLine str in (a:f, b:s)) ([], [])

solve1 :: ([Int], [Int]) -> Int
solve1 (l1, l2) = sum [abs (x - y) | (x, y) <- zip (sort l1) (sort l2)]

solve2 :: ([Int], [Int]) -> Int
solve2 (l1,l2) =
    recs (sort l1) (sort l2) 0
    where 
        recs as bs acc = case (as, bs) of
            (_, []) -> acc
            ([], _) -> acc
            (a:ass, b:bss) -> case compare a b of
                LT -> recs ass bs acc
                GT -> recs as bss acc
                EQ -> recs ass bs (acc + a * length (takeWhile (== a) bs))

day01 :: IO ()
day01 = do
    input <- parseInput <$> readInput
    print $ solve1 input
    print $ solve2 input
