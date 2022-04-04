module ApplicativeExpr (
    Expr(..),
    Parser(..),
    expr
) where

import Parser ( Parser(..), sat, char )
import Data.Char ( isDigit )
import Control.Applicative ( Alternative((<|>), some) )

data Expr = K Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving Show

natural :: Parser Expr
natural = K . read <$> some (sat isDigit)

expr :: Parser Expr
expr = (Add <$> (factor <* char '+') <*> expr) <|> factor

factor :: Parser Expr
factor = (Mul <$> (term <* char '*') <*> factor) <|> term

term :: Parser Expr
term = (char '(' *> expr <* char ')') <|> natural