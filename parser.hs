import Data.Tree (Tree)
parseJson :: String -> Tree String
parseJson = undefined

parseStructure :: String -> String
parseStructure (x:xs) = 
    if (x EQ '{') then "let's go"