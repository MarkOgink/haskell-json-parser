data Json = JsonArray Array | JsonObject Object deriving (Show)
type Array = [Value]
type Object = [Pair]
data Pair = Pair String Value deriving (Show)
data Null = Null deriving (Show)
data Value = String String | Integer Int | Array Array | Object Object | Boolean Bool | JsonNull Null deriving (Show)
main = do
    fileText <- readFile "json.txt"
    let parsed = parse jsonParser "json.txt" fileText
    print parsed

jsonParser :: Parser Json
jsonParser = try jsonObjectParser <|> try jsonArrayParser

jsonObjectParser :: Parser Json
jsonObjectParser = spaces >> JsonObject <$> (char '{' >> spaces >> pairsParser) <* spaces <* char '}' <* spaces <* eof

jsonArrayParser :: Parser Json
jsonArrayParser = spaces >> JsonArray <$> (spaces >> char '[' >> spaces >> arrayValueParser <* spaces <* char ']') <* spaces <* eof

jsonValueParser :: Parser Value
jsonValueParser = try stringParser <|> try numberParser <|> try objectParser <|> try arrayParser <|> try boolParser <|> try nullParser

objectParser :: Parser Value
objectParser = spaces >> Object <$> (char '{' >> spaces >> pairsParser) <* spaces <* char '}' <* spaces

pairsParser :: Parser [Pair]
pairsParser = pairParser `sepBy` (char ',' >> spaces)

pairParser :: Parser Pair
pairParser = Pair <$> (spaces >> char '\"' >> manyTill anyChar (char '\"') <* spaces <* char ':')  <*> (spaces >> jsonValueParser <* spaces)

stringParser :: Parser Value
stringParser = spaces >> String <$> (spaces >> char '\"' >> manyTill anyChar (char '\"') <* spaces)

numberParser :: Parser Value
numberParser = spaces >> Integer <$> (spaces >> int <* spaces)

arrayParser :: Parser Value
arrayParser = spaces >> Array <$> (char '[' >> spaces >> arrayValueParser) <* spaces <* char ']' <* spaces

arrayValueParser :: Parser [Value]
arrayValueParser = jsonValueParser `sepBy` (char ',' >> spaces)

boolParser :: Parser Value
boolParser = Boolean . (== "true") <$> (string "true" <|> string "false")

nullParser :: Parser Value
nullParser = JsonNull <$> option Null (string "null" >> return Null)
