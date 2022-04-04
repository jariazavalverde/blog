module MonadExpr (
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
expr = do x <- factor
          expr' x

expr' :: Expr -> Parser Expr
expr' x = (do char '+'
              y <- factor
              expr' (Add x y)) <|> return x

factor :: Parser Expr
factor = do x <- term
            factor' x

factor' :: Expr -> Parser Expr
factor' x = (do char '*'
                y <- term
                factor' (Mul x y)) <|> return x

term :: Parser Expr
term = (do char '('
           x <- expr
           char ')'
           return x) <|> (Val <$> natural)