module Main where

import Data.Char
import Data.List

run :: String -> [(Char, Int)]
run = display . group . sort . canonical

canonical :: String -> String
canonical = filter (/= ' ') . map normalise

normalise c | isUpper c = c | isLower c = toUpper c | otherwise = ' '

display = map (\x -> (head x, length x))

main :: IO ()
main = putStrLn "Hello Haskell!"