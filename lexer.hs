{-# LANGUAGE LambdaCase #-}
newtype Parser i o =
  Parser { runParser :: i -> Maybe (i, o) }

char1 :: Char -> Parser String Char
char1 c = Parser $ \case
  (x:xs) | x == c -> Just (xs, x)
  _               -> Nothing