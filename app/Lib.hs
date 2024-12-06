module Lib where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Text.Printf (printf)

readInputForDay :: Int -> IO [String]
readInputForDay day = lines <$> readFile ( "data/day" ++ printf "%02d" day ++ "/input.txt" )

readPosNum :: String -> Maybe (Int, String)
readPosNum s = 
    let ds = takeWhile isDigit s in
    let dl = length ds in
    if dl > 0 then Just (read ds, dropWhile isDigit s) else Nothing

readNumMax :: Int -> String -> Maybe (Int, String)
readNumMax m s = readPosNum s >>= \(num,str) -> if num <= m then Just (num,str) else Nothing

parseList :: Read a => String -> [a]
parseList s = map read (splitOn "," s)


