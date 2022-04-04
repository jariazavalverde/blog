# Analizadores sintácticos monádicos

Un *analizador sintáctico* es un programa que analiza cadenas de símbolos y permite transformarlas en algún tipo de representación más estructurada. Por otra parte, en programación funcional, una *mónada* es una estructura que representa una forma de computación, y favorece la programación *con efectos* de forma genérica. El análisis sintáctico es uno de los muchos problemas que las mónadas ayudan a simplificar.

En este artículo, se proporciona una descripción general de los conceptos fundamentales sobre el análisis sintáctico basado en las interfaces de funtor, funtor aplicativo y mónada. Se asume cierta familiaridad con las clases de Haskell `Functor`, `Applicative` y `Monad`.

## La mónada Parser

Definiremos los analizadores sintácticos como funciones que toman una cadena de caracteres -la entrada que debe ser analizada- y que *posiblemente* devuelven un par formado por el valor analizado -el dato estructurado- y el resto de la cadena que queda por analizar.

```haskell
newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}
```

Es frecuente que las librerías de análisis parametricen el tipo de entrada `s` que se consume, y que se devuelva una lista de resultados `[(a, s)]` en lugar de `Maybe (a, s)`, permitiendo así que el proceso de análisis sea no determinista. De hecho, lo normal es parametrizar además el tipo de la mónada subyacente para agregar distintos efectos al proceso de análisis madiante *transformadores de mónadas*.

No obstante, por simplicidad, en este artículo  nos centraremos en el tipo `String` y usaremos la mónada `Maybe`, que será suficiente para representar un proceso de análisis semi-determinista donde la entrada puede fallar o ser analizada de una única forma.

## Bloques de construcción básicos 

La mayoría de analizadores se construyen combinando otros analizadores más simples. La función más básica para construir analizadores sintácticos es `sat`. Esta función toma como argumento un predicado (en `Char`) y devuelve un analizador que consume el primer carácter de la entrada si este satisface el predicado.

```haskell
sat :: (Char -> Bool) -> Parser Char
sat p = Parser $ \input -> case input of
    []     -> Nothing
    (x:xs) -> if p x then Just (x, xs)
                     else Nothing
```

Por ejemplo, para consumir el siguiente dígito numérico de la entrada, podemos utilizar el predicado `isDigit :: Char -> Bool` del módulo `Data.Char`.

```haskell
ghci> runParser (sat isDigit) "123"
Just ('1', "23")
ghci> runParser (sat isDigit) "a123"
Nothing
```

Partiendo de `sat` podemos construir otras funciones básicas, como `char`, que recibe un carácter `c` y devuelve un analizador que consume el siguiente carácter de la cadena si es `c`, o `next`, que consume el siguiente carácter de la cadena, sea cual sea.

```haskell
char :: Char -> Parser Char
char c = sat (== c)

next :: Parser Char
next = sat (const True)
```

```haskell
ghci> runParser (char 'b') "bar"
Just ('b', "ar")
ghci> runParser (char 'o') "foo"
Nothing
ghci> runParser next "foo"
Just ('f', "oo")
ghci> runParser next ""
Nothing
```

## Análisis aplicativo

En ocasiones, es necesario transformar el posible valor generado por un analizador. Esto es exactamente lo que permite la función `fmap :: (a -> b) -> Parser a -> Parser b` de la declaración de `Functor` de un analizador: aplicar una función al resultado del análisis, si existe.

```haskell
instance Functor Parser where
    fmap f p = Parser $ \input -> case runParser p input of
        Nothing -> Nothing
        Just (x, xs) -> Just (f x, xs)
```

```haskell
ghci> let digit = fmap (read . pure) (sat isDigit) :: Parser Int
ghci> runParser digit "123"
Just (1, "23")
```

De forma más general, estamos interesados en aplicar una función a dos o más posibles valores generados por analizadores. La función `(<*>) :: Parser (a -> b) -> Parser a -> Parser b` de la declaración de `Applicative` de un analizador permite ejecutar secuencialmente dos analizadores, y aplicar la posible función generada por el primero al posible valor generado por el segundo. (Nótese que la entrada que lee el segundo analizador es la cadena restante que deja por analizar el primero).

```haskell
instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    pf <*> px = Parser $ \input -> case runParser pf input of
        Nothing -> Nothing
        Just (f, xs) -> case runParser px xs of
            Nothing -> Nothing
            Just (x, xs') -> Just (f x, xs')
```

Además, la clase `Applicative` cuenta con métodos que nos permiten aplicar secuencialmente dos analizadores, descartando el primer o segundo valor analizado: `*>` y `<*`, respectivamente.

Por ejemplo, la función `pair` recibe dos analizadores sintácticos, uno que analiza datos del tipo `a` y otro que analiza datos del tipo `b`, y devuelve un analizador que consume una tupla del tipo `(a,b)`.

```haskell
pair :: Parser a -> Parser b -> Parser (a,b)
pair p q = (,) <$> (char '(' *> p <* char ',') <*> (q <* char ')') 
```

```haskell
ghci> runParser (pair digit digit) "(1,2)"
Just ((1,2), "")
ghci> runParser (pair digit next) "(1,a)"
Just ((1,'a'), "")
```

## Fallos y alternativas

Es conveniente introducir un combinador para manejar fallos y alternativas, de forma que sea posible lanzar un analizador y, si este no tiene éxito, ejecutar otro en su defecto. Eso es justo lo que hace la función `(<|>) :: Parser a -> Parser a -> Parser a` de la declaración de `Alternative` de un analizador.

```haskell
instance Alternative Parser where
    empty = Parser $ const Nothing
    p <|> q = Parser $ \input -> case runParser p input of
        Nothing -> runParser q input
        Just (x, xs) -> Just (x, xs)
```

Esta clase nos da acceso a dos funciones realmente útiles para el análisis sintáctico:

* `many` toma como argumento un analizador `p` que genera datos del tipo `a` y devuelve un analizador que genera datos del tipo `[a]` aplicando *cero o más veces* el analizador `p`.
* `some` toma como argumento un analizador `p` que genera datos del tipo `a` y devuelve un analizador que genera datos del tipo `[a]` aplicando *una o más veces* el analizador `p`.

La diferencia entre `many` y `some` es que `many` permite analizar una cadena vacía, mientras que `some` tiene que tener éxito al menos una vez.

Por ejemplo, un número natural es una secuencia *no nula* de dígitos. Podemos utilizar el combinador `some` para consumir uno o más dígitos numéricos de la entrada, y transformar la cadena resultante a un entero.

```haskell
natural :: Parser Int
natural = read <$> some (sat isDigit)
```
```haskell
ghci> runParser natural "123foo"
Just (123, "foo")
ghci> runParser natural "bar"
Nothing
```

La interfaz de funtor aplicativo es lo suficientemente expresiva para analizar  gramáticas libres de contexto. Una *gramática libre de contexto* es una gramática formal en la que cada regla de producción es de la forma `A → α`, donde `A` es un símbolo no terminal y `α` es una cadena de símbolos terminales y no terminales.<sup>[[1]](#footnote-1)</sup>

Un ejemplo sencillo de gramática libre de contexto, es una gramática para analizar expresiones aritméticas. Por simplicidad, consideraremos que las expresiones aritméticas contienen únicamente números naturales, sumas `+`, productos `*` y paréntesis `(` y `)` para agrupar expresiones, con la precedencia usual de los operadores, pero asociándolos por la derecha, generando un árbol de análisis que defina la estructura de la expresión aritmética analizada.

```haskell
data Expr = K Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving Show
```

La gramática libre de contexto correspondiente es la siguiente:

* `<Expr> → <Factor> "+" <Expr> | <Factor>`
* `<Factor> → <Term> "*" <Factor> | <Term>`
* `<Term> → "(" <Expr> ")" | <Natural>`

donde `|` es un operador usado para separar múltiples opciones para un mismo no terminal. (De hecho, en este contexto, `|` se corresponde con la función `<|>` en Haskell).

```haskell
natural :: Parser Expr
natural = K . read <$> some (sat isDigit)

expr :: Parser Expr
expr = (Add <$> (factor <* char '+') <*> expr) <|> factor

factor :: Parser Expr
factor = (Mul <$> (term <* char '*') <*> factor) <|> term

term :: Parser Expr
term = (char '(' *> expr <* char ')') <|> natural
```

```haskell
ghci> runParser expr "2*3+4*5"
Just (Add (Mul (K 2) (K 3)) (Mul (K 4) (K 5)), "")

ghci> runParser expr "(1+2)*(3+4)"
Just (Mul (Add (K 1) (K 2)) (Add (K 3) (K 4)), "")

ghci> runParser expr "(1+2)+3"
Just (Add (Add (K 1) (K 2)) (K 3), "")

ghci> runParser expr "1+2+3"  
Just (Add (K 1) (Add (K 2) (K 3)), "")
```

## Análisis monádico

No todos los lenguajes pueden ser expresado mediante gramáticas libres de contexto. Para analizar algunas gramáticas, es necesario que el segundo analizador tenga la información del valor generado por el primero. La función lazo `(>>=) :: Parser a -> (a -> Parser b) -> Parser b` de la declaración de `Monad` de un analizador permite extraer el valor generado por un analizador, y pasarlo como parámetro a una función que devuelva otro analizador en función del valor consumido anteriormente.

```haskell
instance Monad Parser where
    p >>= f = Parser $ \input -> case runParser p input of
        Nothing -> Nothing
        Just (x, xs) -> runParser (f x) xs
```

Ahora podemos reimplementar el analizador de expresiones aritméticas, respetando la asociatividad usual de los operadores por la izquierda. La gramática correspodiente es la siguiente:

* `<Expr> → <Factor> <Expr'>`
* `<Expr'> → "+" <Factor> <Expr'> | ε`
* `<Factor> → <Term> <Expr'>`
* `<Factor'> → "*" <Term> <Factor'> | ε`
* `<Term> → <Natural> | "(" <Expr> ")"`

donde `ε` representa la producción nula.

```haskell
natural :: Parser Expr
natural = K . read <$> some (sat isDigit)

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
           return x) <|> natural
```

```haskell
ghci> runParser expr "1+2+3"      
Just (Add (Add (K 1) (K 2)) (K 3), "")
ghci> runParser expr "1+2*3*4+5"
Just (Add (Add (K 1) (Mul (Mul (K 2) (K 3)) (K 4))) (K 5), "")
```

## Referencias

* Hutton, G., & Meijer, E. (1998). [**Monadic parsing in Haskell**](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/monadic-parsing-in-haskell/E557DFCCE00E0D4B6ED02F3FB0466093). *Journal of functional programming*, *8*(4), 437-444.
* Hutton, G. (2016). Monadic parsing. In [**Programming in Haskell**](https://www.cambridge.org/highereducation/books/programming-in-haskell/8FED82E807EF12D390DE0D16FDE217E4#overview) (pp. 177-195). Cambridge: Cambridge University Press.


## Notas

1. <sub id="footnote-1">El término *&laquo;libre de contexto&raquo;* se refiere al hecho de que el símbolo no terminal `A` siempre puede ser reemplazado por `α` sin tener en cuenta el contexto en el que ocurra.