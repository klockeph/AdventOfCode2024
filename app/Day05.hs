module Day05 (day05) where

import Lib (readInputForDay, readPosNum, parseList)

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import qualified Data.IntMap.Strict as IntMap

type Rule = (Int, Int)

parseRule :: String -> Maybe Rule
parseRule r = do
    (a, s) <- readPosNum r
    (b, _) <- readPosNum (drop 1 s)
    Just (a,b)

parseRules :: [String] -> ([Rule], [String])
parseRules [] = ([], [])
parseRules (s:ss) = case parseRule s of
    Just r -> let (rs, sss) = parseRules ss in (r:rs, sss)
    Nothing -> ([], s:ss)

parseInput :: [String] -> ([Rule], [[Int]])
parseInput s =
    let (rs, ss) = parseRules s in
    (rs, parseList <$> drop 1 ss)

checkRule :: IntMap.IntMap Int -> Rule -> Bool
checkRule m r = fromMaybe True (liftM2 (<) (IntMap.lookup (fst r) m) (IntMap.lookup (snd r) m))

isValid :: [Rule] -> [Int] -> Bool
isValid rs is =
    let m = IntMap.fromList (zip is [0..]) in
    and $ checkRule m <$> rs

median :: [Int] -> Int
median [] = 0
median l = l !! (length l `div` 2)

getValidMedian :: [Rule] -> [Int] -> Int
getValidMedian rs is = if isValid rs is then median is else 0

solve1 :: [Rule] -> [[Int]] -> Int
solve1 rs iss = sum (getValidMedian rs <$> iss)

-- I'm still wondering why this is okay .. don't we need to check transitively?
comp :: IntMap.IntMap [Int] -> Int -> Int -> Ordering
comp rules a b = case IntMap.lookup a rules of
    Just aRules -> if b `elem` aRules then LT else
        case IntMap.lookup b rules of
            Just bRules -> if a `elem` bRules then GT else EQ
            Nothing -> EQ 
    Nothing -> EQ

processRules :: [(Int, Int)] -> IntMap.IntMap [Int]
processRules rs = IntMap.fromListWith (++)  [(a, [b]) | (a,b) <- rs]

solve2 :: [Rule] -> [[Int]] -> Int
solve2 rs iss =
    let invalids = filter (not . isValid rs) iss in
    let reordered = sortBy (comp $ processRules rs) <$> invalids in
    sum $ median <$> reordered

day05 :: IO ()
day05 = do
    (rs, is) <- parseInput <$> readInputForDay 5
    print $ solve1 rs is
    print $ solve2 rs is