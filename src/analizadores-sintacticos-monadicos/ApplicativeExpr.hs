module ApplicativeExpr (
    Expr(..),
    Parser(..),
    expr
) where

import Parser ( Parser(..), sat, char )
import Data.Char ( isDigit )
import Control.Applicative ( Alternative((<|>), some) )

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving Show

natural :: Parser Int
natural = read <$> some (sat isDigit)

expr :: Parser Expr
expr = (Add <$> (factor <* char '+') <*> factor) <|> factor

factor :: Parser Expr
factor = (Mul <$> (term <* char '*') <*> term) <|> term

term :: Parser Expr
term = (char '(' *> expr <* char ')') <|> (Val <$> natural)