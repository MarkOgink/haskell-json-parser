module Main where

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser x) = Parser $ \s -> do
        (x',s') <- x s
        return (f x', s')

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x,s)

    (Parser f) <*> (Parser x) = Parser $ \s -> do
        (f', s1) <- f s
        (x', s2) <- x s1
        return (f' x', s2)

instance Monad Parser where
    (Parser x) >>= f = Parser $ \s -> do
        (x', s') <- x s
        runParser (f x') s'

instance MonadFail Parser where
    fail _ = Parser $ const Nothing

class (Applicative f) => Alternative f where
    empty :: f a
    ( <|> ) :: f a -> f a -> f a
    some :: f a -> f [a]
    some v = some_v
        where   many_v = some_v <|> pure []
                some_v = (:) <$> v <*> many_v
    
    many :: f a -> f [a]
    many v = many_v
        where   many_v = some_v <|> pure []
                some_v = (:) <$> v <*> many_v

instance Alternative Parser where
    empty = fail ""
    (Parser x) <|> (Parser y) = Parser $ \s ->
        case x s of
            Just x -> Just x
            Nothing -> y s
char :: Char -> Parser Char
char c = Parser charP
    where   charP []              = Nothing
            charP (x:xs) | x == c = Just (c,xs)
                         | otherwise = Nothing
string :: String -> Parser String
string = mapM char

space :: Parser Char
space = char ' '<|> char '\n'
spaces = many space

parseIntChar :: Parser Char
parseIntChar = char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4' <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9'

parseHW = (,) <$> (string "Hello" <* spaces) <*> string "World!"

main :: IO()
main = putStrLn "Hello, Haskell"