module Parser (
    Parser(..),
    sat,
    char,
    next
) where

import Control.Applicative (Alternative(..))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f p = Parser $ \input -> case runParser p input of
        Nothing -> Nothing
        Just (x, xs) -> Just (f x, xs)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    pf <*> px = Parser $ \input -> case runParser pf input of
        Nothing -> Nothing
        Just (f, xs) -> case runParser px xs of
            Nothing -> Nothing
            Just (x, xs') -> Just (f x, xs')

instance Alternative Parser where
    empty = Parser $ const Nothing
    p <|> q = Parser $ \input -> case runParser p input of
        Nothing -> runParser q input
        Just (x, xs) -> Just (x, xs)

instance Monad Parser where
    p >>= f = Parser $ \input -> case runParser p input of
        Nothing -> Nothing
        Just (x, xs) -> runParser (f x) xs

sat :: (Char -> Bool) -> Parser Char
sat p = Parser $ \input -> case input of
    []     -> Nothing
    (x:xs) -> if p x then Just (x, xs)
                       else Nothing

char :: Char -> Parser Char
char c = sat (== c)

next :: Parser Char
next = sat (const True)