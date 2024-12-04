module Lib where

import Text.Printf (printf)

readInputForDay :: Int -> IO [String]
readInputForDay day = lines <$> readFile ( "data/day" ++ (printf "%02d" day) ++ "/input.txt" )