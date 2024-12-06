module Day05 (day05) where

import Lib (readInputForDay, readPosNum, parseList)

import Control.Monad (liftM2)
import Data.Array as Array
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.List (nub, sort)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

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

median :: [Int] -> Maybe Int
median [] = Nothing
median l = Just $ l !! (length l `div` 2)

getValidMedian :: [Rule] -> [Int] -> Maybe Int
getValidMedian rs is = if isValid rs is then median is else Nothing

solve1 :: [Rule] -> [[Int]] -> Int
solve1 rs iss = sum (fromMaybe 0 . getValidMedian rs <$> iss)

failingRules :: [Rule] -> IntMap.IntMap Int -> [Rule]
failingRules rs m = [r | r <- rs, not $ checkRule m r]

reorderInvalids :: [Rule] -> [Int] -> Maybe [Int]
reorderInvalids rs is =
    let m = IntMap.fromList (zip is [0..]) in
    let frs = failingRules rs m in
    if null frs then Nothing else
    let a = listArray (0, length is - 1) is in
    let nodes = buildAllNodes frs in
    let wrongSlots = sort $ map (m IntMap.!) nodes in
    let correctOrder = dfsAll (buildAdjList frs) Set.empty nodes in
    Just $ toList $ a // zip wrongSlots correctOrder

solve2 :: [Rule] -> [[Int]] -> Int
solve2 rs iss =
    let reordered = reorderInvalids rs <$> iss in
    let medians = (median =<<) <$> reordered in
    sum $ fromMaybe 0 <$> medians

testinput = [
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47"]

day05 :: IO ()
day05 = do
    (rs, is) <- parseInput <$> readInputForDay 5
    -- let (rs, is) = parseInput testinput
    print $ solve1 rs is
    -- 4434 too low
    print $ solve2 rs is
    -- let g = rs
    -- let adj = buildAdjList g
    -- let nodes = buildAllNodes g
    -- print $ dfsAll adj Set.empty nodes

dfs :: IntMap.IntMap [Int] -> Set.Set Int -> Int -> [Int]
dfs adjList visited x
    | Set.member x visited = []
    | otherwise = case IntMap.lookup x adjList of
        Nothing -> [x]
        Just nxt -> x : foldr (\n l -> dfs adjList (foldr Set.insert visited (x:l)) n ++ l) [] nxt

dfsAll :: IntMap.IntMap [Int] -> Set.Set Int -> [Int] -> [Int]
dfsAll _ _ [] = []
dfsAll adjList visited (x:xs)
    | Set.member x visited = dfsAll adjList visited xs
    | otherwise = 
        let vs = dfs adjList visited x in
        (dfsAll adjList (foldr Set.insert visited vs) xs) ++ vs

buildAdjList :: [(Int, Int)] -> IntMap.IntMap [Int]
buildAdjList es = IntMap.fromListWith (++)  [(a, [b]) | (a,b) <- es]

buildAllNodes :: [(Int, Int)] -> [Int]
buildAllNodes = nub . concatMap (\(a,b) -> [a,b])