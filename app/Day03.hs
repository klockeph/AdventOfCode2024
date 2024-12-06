module Day03 (day03) where

import Lib (readInputForDay, readNumMax)

import Data.List (isPrefixOf)

readFixed :: String -> String -> Maybe String
readFixed needle s
    | needle `isPrefixOf` s = Just $ drop (length needle) s
    | otherwise = Nothing

readMul :: String -> Maybe (Int, Int, String)
readMul s = do
    s1 <- readFixed "mul(" s
    (x, s2) <- readNumMax 999 s1
    s3 <- readFixed "," s2
    (y, s4) <- readNumMax 999 s3
    s5 <- readFixed ")" s4
    pure (x, y, s5)

advance :: String -> String
advance = drop 1

getAllMuls :: String -> [(Int, Int)]
getAllMuls [] = []
getAllMuls s = 
    case readMul s of
      Just (x, y, ss) -> (x,y):(getAllMuls ss)
      Nothing -> getAllMuls (advance s)

skipUntilDo :: String -> String
skipUntilDo [] = []
skipUntilDo s =
    case readFixed "do()" s of
        Just ss -> ss
        Nothing -> skipUntilDo $ advance s

getEnabledMuls :: String -> [(Int, Int)]
getEnabledMuls [] = []
getEnabledMuls s =
 case readMul s of
    Just (x, y, ss) -> (x,y):(getEnabledMuls ss)
    Nothing -> case readFixed "don't()" s of
        Just ss -> getEnabledMuls (skipUntilDo ss)
        Nothing -> getEnabledMuls (advance s)
        

solve :: (String -> [(Int, Int)]) -> String -> Int
solve parseFn s = sum $ (uncurry (*)) <$> (parseFn s)

day03 :: IO ()
day03 = do
    input <- concat <$> readInputForDay 3
    print $ solve getAllMuls input
    print $ solve getEnabledMuls input