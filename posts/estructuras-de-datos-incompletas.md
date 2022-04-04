# Estructuras de datos incompletas

Las *estructuras de datos incompletas* proporcionan una técnica de programación propia de Prolog que permite incrementar la eficiencia de los programas lógicos y simplificar su diseño. Estas estructuras se apoyan en el uso de *variables lógicas* para representar *huecos*, los cuales simbolizan partes de las estructuras que todavía no han sido computadas. La estructura incompleta más utilizada es la *lista diferencia*.

## El problema de la concatenación de listas

La definición de la concatenación de listas usual no tiene ningún misterio: el predicado `append/3` es cierto cuando la tercera lista es la concatenación de las dos primeras.

```prolog
append([], X, X).
append([H|T], X, [H|S]) :-
    append(T, X, S).
```

```prolog
?- append([1,2,3], [4,5,6], X).
X = [1,2,3,4,5,6].
```

Mediante esta definición, para concatenar dos listas se deben recorrer todos los elementos de la primera hasta llegar al final, donde se inserta la segunda. Por lo tanto, este predicado tiene *complejidad lineal* respecto al tamaño de la primera lista.

La concatenación es la base para la implementación de muchas de las operaciones sobre listas: permutar, rotar, aplanar, etcétera; donde la eficiencia se ve afectada por esta operación.

```prolog
permutation([], []).
permutation([H|T], S) :-
    permutation(T, P),
    append(X, Y, P),
    append(X, [H|Y], S).
```

```prolog
?- permutation([1,2,3], X).
X = [1,2,3] ;
X = [2,1,3] ;
X = [2,3,1] ;
X = [1,3,2] ;
X = [3,1,2] ;
X = [3,2,1].
```

## Listas abiertas y listas diferencia

Una *lista abierta* es una lista que tiene como cola una *variable libre*, la cual es llamada *hueco*. Esta variable puede unificar con otra lista, resultando así en una concatenación de listas.

```prolog
?- X = [1,2,3|H], H = [].
X = [1,2,3], H = [].

?- X = [1,2,3|H], H = [4,5,6].
X = [1,2,3,4,5,6], H = [4,5,6].
```

Una *lista diferencia* es un par formado por una lista abierta y su hueco. En Prolog, es usual representar este par como `L-H`, donde `L` es una lista abierta que tiene como hueco a la variable `H`. Por ejemplo: `[1,2,3|X]-X`.<sup>[[1]](#footnote-1)</sup>

```prolog
append_diff(Xs-X, Ys-Y, Zs-Z) :-
    X = Ys,
    Zs = Xs,
    Z = Y.
```

```prolog
?- append_diff([1,2,3|X]-X, [4,5,6|Y]-Y, Z).
X = [4,5,6|Y], Z = [1,2,3,4,5,6|Y]-Y.
```

Utilizando esta representación, la concatenación de las listas `[1,2,3]` y `[4,5,6]` se corresponde con la concatenación de las listas diferencia `[1,2,3|X]-X` y `[4,5,6|Y]-Y`, obteniendo como resultado otra lista diferencia `[1,2,3,4,5,6|Y]-Y`.

Podemos reescribir el predicado `append_diff/3` anterior de forma más directa.

```prolog
append_diff(A-B, B-C, A-C).
```

Ahora es posible concatenar cualquier par de listas diferencia en tiempo constante, aunque este predicado no es tan versátil como `append/3`. Por ejemplo, el modo `append(-Xs, -Ys, +Zs)` permite obtener, por reevaluación, todos los pares de listas `(Xs, Ys)` cuya concatenación produce la lista `Zs`. Esto no es posible con el predicado `append_diff/3`.

## Manipulando listas diferencia

Cuando trabajamos con listas diferencia, debemos manipular siempre la estructura adecuada `L-H` en todos los predicados involucrados. La lista diferencia vacía se representa como `X-X`. Para transformar una lista diferencia en una *lista estándar* simplemente hay que unificar el hueco de la lista diferencia con la lista vacía `[]`.

Por ejemplo, el predicado `flatten/2` es cierto cuando la segunda lista es la concatenación de todas las sublistas contenidas en la primera lista (análogamente el predicado `flatten_diff/2` con listas diferencia).

```prolog
flatten([],[]).
flatten([H|T], L) :-
    flatten(T, S),
    append(H, S, L).

flatten_diff([], X-X).
flatten_diff([A-B|T], A-C) :-
    flatten_diff(T, B-C).
```

```prolog
?- flatten([[1,2], [3,4], [5,6]],W).
W = [1,2,3,4,5,6].

?- flatten_diff([[1,2|X]-X, [3,4|Y]-Y, [5,6|Z]-Z], W).
X = [3,4,5,6|Z], Y = [5,6|Z], W = [1,2,3,4,5,6|Z]-Z.
```

La complejidad del predicado `flatten_diff/2` es lineal respecto al número de sublistas diferencia que contiene la primera lista, independientemente de la longitud de dichas sublistas. Sin embargo, el predicado `flatten/2` concatena las sublistas con `append/3` desde la última sublista hasta la primera, teniendo que recorrer así una vez todos los elementos de todas las sublistas excepto de la última. Por lo tanto, `flatten/2` tiene una complejidad lineal en el número de sublistas más el número de elementos de todas las sublistas excepto de la última.

## Notas

1. <sub id="footnote-1">Aunque `(-)/2` tiene connotaciones de resta, no hay porqué preocuparse, puesto que en este contexto es simplemente un operador infijo que no será interpretado.</sub>
