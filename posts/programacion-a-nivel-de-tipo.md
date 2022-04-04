# Programación a nivel de tipo

La *programación a nivel de tipo* implica codificar cierta lógica en el *sistema de tipo* del lenguaje, que es evaluada y comprobada en tiempo de compilación. Uno de los ejemplos más típicos es el de los *vectores con longitud indexada*, que añaden la longitud de la lista en el sistema de tipo, evitando estáticamente errores de *"fuera de límites"*.

Un *tipo dependiente* es un tipo cuya definición depende del valor. Por ejemplo, un *número entero* es un tipo, mientras que un *número entero impar* es un tipo dependiente, ya que depende del valor del número. A diferencia de otros lenguajes funcionales como Agda, Haskell diferencia entre el *nivel de término* y el *nivel de tipo*. Esta distinción hace imposible el uso de tipos dependientes, pero es posible imitar sus efectos mediante el uso de extensiones de GHC.

## Números naturales a nivel de tipo

Vamos a codificar nuestros primeros números a nivel de tipo. Para ello, utilizaremos la representación de los [*números naturales de Peano*](https://es.wikipedia.org/wiki/Axiomas_de_Peano), donde un número se define como *cero* o como el *sucesor* de otro número natural.

```haskell
{-# LANGUAGE DataKinds #-}
data Nat = Zero | Succ Nat
```

Con esto, acabamos de crear un nuevo tipo de dato llamado `Nat`, que tiene dos constructores de datos:

* `Zero :: Nat`
* `Succ :: Nat -> Nat`

con los que podemos representar los números naturales como:

0. `Zero`
1. `Succ Zero`
2. `Succ (Succ Zero)`
3. `Succ (Succ (Succ Zero))`

Es fácil ver que el tipo (`:type`) de estos datos es `Nat`. También es fácil ver que el tipo (`:kind`) del tipo `Nat` es `*`. Por lo tanto, lo que tenemos son números naturales a nivel de término. Por ejemplo, podemos implementar una función que reciba dos números naturales y nos devuelva su suma:

```haskell
add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)
```

Pero nosotros queremos números naturales a nivel de tipo, no de término, así que tenemos que subir GHC de nivel. La extensión `DataKinds` nos permite exactamente esto, *promover* los constructores de datos a constructores de tipos, así como los constructores de tipos a tipos de tipos. Para diferenciarlos, Haskell precede el nombre de estos nuevos constructores con una comilla simple (`'`).<sup>[[1]](#footnote-1)</sup>

Ahora, del mismo modo que el tipo del tipo `Int` es `*`, y el tipo del constructor de tipos `Maybe` es `* -> *`, podemos observar que el tipo (`:kind`) del tipo `'Zero` es `Nat`, y el tipo del constructor de tipos `'Succ` es `Nat -> Nat`.

## Vectores con longitud indexada

Como ya dijimos, los *vectores con longitud indexada* son listas que añaden su longitud en el sistema de tipo. Para definirlos, necesitamos introducir los *tipos de datos algebraicos generalizados* (*GADTs*).

Los [*GADTs*](https://wiki.haskell.org/Generalised_algebraic_datatype) son tipos de datos para los cuales un constructor tiene un tipo no estándar. Esto nos permite proporcionar información adicional del tipo al ajustar los constructores:

```haskell
{-# LANGUAGE DataKinds, KindSignatures, GADTs, StandaloneDeriving #-}
data Nat = Zero | Succ Nat

data Vector (n :: Nat) (a :: *) where
    VNil :: Vector 'Zero a
    VCons :: a -> Vector n a -> Vector ('Succ n) a

deriving instance Show a => Show (Vector n a)
```

Lo primero que observamos es que además de la extensión `GADTs` hemos añadido la extensión `KindSignatures`. Esta extensión nos permite anotar el tipo de los tipos en las declaraciones, tal y como anotaríamos el tipo de una expresión. Haskell puede deducirlo perfectamente, así que no es obligatorio, pero así queda explícito que nuestro nuevo constructor de tipos `Vector` toma un tipo `n` del tipo `Nat` (representando la longitud de la lista) y un tipo `a` del tipo `*` (representando el tipo de dato almacenado en la lista).

`Vector` tiene dos constructores de datos:

* `VNil`, que devuelve un vector de longitud cero.
* `VCons`, que toma un dato y un vector de longitud `n`, y devuelve un vector de longitud `n+1`. No sabemos qué longitud tiene el vector que recibe, pero sí sabemos que el vector resultante será una unidad más grande.

```haskell
gchi> :t VNil
VNil :: Vector 'Zero a

gchi> :t VCons 'a' (VCons 'b' (VCons 'c' VNil))
VCons 'a' (VCons 'b' (VCons 'c' VNil)) :: Vector ('Succ ('Succ ('Succ 'Zero))) Char

gchi> :t VCons True (VCons False VNil)
VCons True (VCons False VNil) :: Vector ('Succ ('Succ 'Zero)) Bool
```

Ya tenemos nuestros vectores, así que ahora deberíamos definir algunas operaciones. Por ejemplo, vamos a crear una función que dado un vector nos devuelva su cola. El tipo de esta función sería:

```haskell
tailv :: Vector (‘Succ n) a -> Vector n a
```

Estamos especificando mediante el tipo que el resultado de quitar el primer elemento a un vector debe ser un vector con un elemento menos. Tiene sentido. Además, ya de paso estamos descartando los vectores nulos como entrada. Es decir, esta función sólo acepta vectores con longitud mayor que cero. Parece lógico.

```haskell
tailv :: Vector ('Succ n) a -> Vector n a
tailv (VCons _ xs) = xs
```

```haskell
ghci> tailv (VCons 1 (VCons 2 (VCons 3 VNil)))
VCons 2 (VCons 3 VNil)

ghci> tailv VNil

<interactive>:5:7: error:
    * Couldn't match type 'Zero with 'Succ n
      Expected type: Vector ('Succ n) a
        Actual type: Vector 'Zero a
    * In the first argument of `tailv', namely `VNil'
      In the expression: tailv VNil
      In an equation for `it': it = tailv VNil
    * Relevant bindings include
        it :: Vector n a (bound at <interactive>:5:1)
```

Si se nos hubiese pasado por la cabeza la —malévola— idea de descartar dos elementos de la lista en lugar de uno, es decir, `tailv (VCons _ (VCons _ xs)) = xs`, habríamos obtenido un error de tipo al tratar de compilar el programa. Este tipo de comprobaciones en tiempo de compilación son imposibles con las listas predefinidas de Haskell, ya que no hay información de su longitud en el tipo.

Ahora vamos a crear una función que dados dos vectores de longitud arbitraria `n` y `m`, respectivamente, nos devuelva la concatenación de ambos. El tipo de esta función sería:

```haskell
appendv :: Vector n a -> Vector m a -> Vector (?) a
```

No obstante, aquí nos encontramos con un problema: ¿cuál es el tipo del vector resultante? Sabemos que debería tener longitud `n+m`, pero ¿cómo expresamos esto en el tipo? A estas operaciones a nivel de tipo se las denomina [*familias de tipos*](https://wiki.haskell.org/GHC/Type_families). Al aplicar una función a los parámetros se produce un tipo. Para utilizarlas, tenemos que incluir la extensión `TypeFamilies`.

> Type families are to vanilla data types what type class methods are to regular functions

Por lo tanto, vamos a crear una familia que dados dos tipos del tipo `Nat`, nos devuelva un tipo del tipo `Nat` que represente la suma de ambos tipos. La definición de esta familia es análoga a la definición de la función `add` que hemos proporcionado antes para sumar números naturales a nivel de término.

```haskell
{-# LANGUAGE TypeFamilies #-}
type family Add (n :: Nat) (m :: Nat) where
    Add 'Zero n = n
    Add ('Succ n) m = 'Succ (Add n m)
```

Ahora sí, podemos definir la función `appendv :: Vector n a -> Vector m a -> Vector (Add n m) a` de forma similar a como se define la concatenación de dos listas en Haskell:

```haskell
appendv :: Vector n a -> Vector m a -> Vector (Add n m) a
appendv VNil ys = ys
appendv (VCons x xs) ys = VCons x (appendv xs ys)
```

```haskell
ghci> appendv (VCons 1 (VCons 2 (VCons 3 VNil))) (VCons 4 (VCons 5 VNil))
VCons 1 (VCons 2 (VCons 3 (VCons 4 (VCons 5 VNil))))

ghci> :t appendv (VCons 1 (VCons 2 VNil)) (VCons 3 (VCons 4 VNil))
appendv (VCons 1 (VCons 2 VNil)) (VCons 3 (VCons 4 VNil)) :: Num a => Vector ('Succ ('Succ ('Succ ('Succ 'Zero)))) a
```

La principal diferencia entre nuestros vectores con longitud indexada y las listas predefinidas en Haskell es que el *sistema de tipo* puede verificar, en tiempo de compilación, que la lista resultante de la operación `appendv` tiene la longitud correcta.

## Referencias

* [Generalised algebraic datatype - HaskellWiki](https://wiki.haskell.org/Generalised_algebraic_datatype)
* [GHC/Type families - HaskellWiki](https://wiki.haskell.org/GHC/Type_families)

## Notas

1. <sub id="footnote-1">Para utilizar `DataKinds` en GHCi de forma interactiva, hay que establecer la opción `:seti -XDataKinds`.</sub>