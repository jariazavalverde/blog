# Prolog - Transformación de programas
> Expansión de términos y objetivos

Aunque el estándar **ISO Prolog** no define ningún mecanismo de transformación de programas tales como la expansión de macros o la compilación condicional, todos los sistemas Prolog ampliamente utilizados proporcionan predicados que permiten reescribir código en tiempo de compilación. Estos predicados permiten realizar una traducción de los términos Prolog leídos desde una fuente a los términos Prolog manejados por el intérprete.

## Expansión de términos

Cuando el predicado `term_expansion/2` es definido por el usuario, todos los términos leídos durante la consulta de un programa son pasados por este predicado. Si `term_expansion(+Term1, -Term2)` tiene éxito, Prolog asertará en la base de datos el término `Term2` proporcionado por este predicado en lugar del término `Term1` leído originalmente.

Supongamos que queremos definir predicados con un estilo ecuacional, donde el resultado de la operación será la última expresión de la parte derecha de la ecuación. Por ejemplo:

```prolog
% head :: [a] -> a
head([H|_]) := H.

% add :: Int -> Int -> Int
add(X, Y) := Z is X+Y, Z.
```

Podemos utilizar la expansión de términos para transformar estas reglas a predicados Prolog estándar. Para ello, primero declaramos el operador `:=` (no asociativo) con prioridad `1200`, y definimos un predicado `result/3` que dada una conjunción de objetivos `A0, A1, ..., An` nos devuelva el último objetivo `An` y el resto de la expresión `A0, A1, ..., A(n-1)`. Por último, declaramos la expansión del término `(A := B)` a un término de la forma `(A' :- B')`, donde `A'` es la parte izquierda `A` con un parámetro adicional -el último objetivo de la parte derecha `B`- y `B'` es la parte derecha `B` sin el último objetivo.

```prolog
:- use_module(library(lists)).
:- op(1200, xfx, :=).

result(X, (A,C), D) :-
	nonvar(X),
    X = (A,B), !,
    result(B, C, D).
result(X, true, X).

term_expansion((Head1 := Expr), (Head2 :- Body)) :-
	Head1 =.. Xs,
    result(Expr, Body, Result),
    append(Xs, [Result], Ys),
    Head2 =.. Ys.
```

Ahora podemos comprobar que al introducir las dos reglas anteriores, el compilador las transforma a los siguientes predicados Prolog:

```prolog
head([H|_], H) :- true.
add(X, Y, Z) :- Z is X+Y, true.
```

```prolog
?- head([1,2,3], X).
X = 1.

?- add(1, 2, X).
X = 3.
```

## Expansión de objetivos

Como `term_expansion/2`, el predicado `goal_expansion/2` se utiliza a modo de expansión de macros de código Prolog. Entre la fase de expansión de términos y la fase de compilación, el cuerpo de las cláusulas analizadas y los objetivos son manejados por el predicado `goal_expansion/2`, que permite al usuario definir sus propias macros.

Supongamos que queremos definir bucles donde podamos realizar una acción repetidas veces, dando distintos valores a una variable en un intervalo. Por ejemplo, el siguiente predicado `squares/0` imprimiría por la salida estándar los números comprendidos en el intervalo `[1, 10]` junto al valor de sus cuadrados:

```prolog
squares :- for x from 1 to 10 do (
    Y is x*x,
    write((x, Y))
).
```

Para que este tipo de construcciones sean válidas, primero debemos declarar algunos operadores (`do`, `for`, `from` y `to`). El predicado `replace/4` reemplaza todas las ocurrencias de un término por otro en un término dado. El predicado `for/5` recibe el inicio y el fin del intervalo, el término utilizado como variable de iteración y el cuerpo del bucle, y devuelve una conjunción de cuerpos donde la variable se ha reemplazado en cada caso por el valor correspondiente de la iteración. Finalmente declaramos la expansión de los objetivos de la forma `for Var from From to To do Body`.

```prolog
:- use_module(library(lists)).
:- op(850, xfx, do).
:- op(800, fx, for).
:- op(700, xfx, from).
:- op(600, xfx, to).

replace(Old, New, X, New) :- Old == X, !.
replace(Old, New, F, F_) :-
    nonvar(F), F =.. [P|Args],
    maplist(replace(Old, New), Args, Args_),
    F_ =.. [P|Args_].
replace(_, _, X, X).

for(From, To, _, _, true) :- From > To.
for(From, To, Var, Body, (X,Xs)) :-
    From =< To,
    succ(From, From_),
    copy_term(Body, Body_),
    replace(Var, From, Body_, X),
    for(From_, To, Var, Body, Xs).

goal_expansion(for Var from From to To do Body, For) :-
    nonvar(Var), integer(From), integer(To), callable(Body),
    for(From, To, Var, Body, For).
```

Ahora podemos observar que el predicado `squares/0` es transformado de la siguiente forma:

```prolog
squares :-
    (_573 is 1*1, write((1, _573))),
    (_800 is 2*2, write((2, _800))),
    (_1027 is 3*3, write((3, _1027))),
    (_1254 is 4*4, write((4, _1254))),
    (_1481 is 5*5, write((5, _1481))),
    (_1708 is 6*6, write((6, _1708))),
    (_1935 is 7*7, write((7, _1935))),
    (_2162 is 8*8, write((8, _2162))),
    (_2389 is 9*9, write((9, _2389))),
    (_2616 is 10*10, write((10, _2616))),
    true.
```

## Referencias

* [The Power of Prolog: Prolog Macros](https://www.metalevel.at/prolog/macros)
* [SWI-Prolog Manual: Conditional compilation and program transformation](https://www.swi-prolog.org/pldoc/man?section=progtransform)
* [SICStus Prolog: Term and Goal Expansion](https://sicstus.sics.se/sicstus/docs/3.12.9/html/sicstus/Term-and-Goal-Expansion.html)
