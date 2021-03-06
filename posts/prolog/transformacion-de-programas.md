# Prolog - Transformación de programas
> Expansión de términos y objetivos

Toda cláusula Prolog es un término Prolog válido, lo cual implica que podemos analizar y procesar código Prolog utilizando predicados y características incorporadas del propio lenguaje, y es especialmente conveniente a la hora de definir transformaciones automáticas de programas. Aunque el estándar **ISO Prolog** no define ningún mecanismo de transformación de programas tales como la expansión de macros o la compilación condicional, todos los sistemas Prolog ampliamente utilizados proporcionan predicados que permiten reescribir código en tiempo de compilación.

## Expansión de términos

Cuando el predicado `term_expansion/2` es definido por el usuario, todos los términos leídos durante la consulta de un programa son pasados por este predicado. Si `term_expansion(+Term1, -Term2)` tiene éxito, Prolog añadirá a la base de datos el término `Term2` proporcionado por este predicado en lugar del término `Term1` leído originalmente.

___

**Ejemplo 1.** Queremos definir predicados con un estilo *ecuacional*, donde el resultado de la operación será la última expresión de la parte derecha de la ecuación. Por ejemplo:

```prolog
head([H|_]) := H.
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

___

Las [gramáticas de cláusulas definidas](https://github.com/jariazavalverde/blog/blob/master/posts/prolog/gramaticas-de-clausulas-definidas.md) son otro clásico ejemplo de la expansión de términos.

## Expansión de objetivos

Como `term_expansion/2`, el predicado `goal_expansion/2` se utiliza a modo de expansión de macros de código Prolog. Entre la fase de expansión de términos y la fase de compilación, el cuerpo de las cláusulas analizadas y los objetivos son manejados por el predicado `goal_expansion/2`, que permite al usuario definir sus propias transformaciones. La fase de expansión de objetivos calcula un punto fijo aplicando transformaciones hasta que no hay más cambios.

___

**Ejemplo 2.** Queremos definir bucles donde podamos realizar una acción repetidas veces, asignando distintos valores a una variable en un intervalo. Por ejemplo, el siguiente predicado `squares/0` imprime por la salida estándar los números comprendidos en el intervalo `[1, 10]` junto al valor de sus cuadrados:

```prolog
squares :- for X from 1 to 10 do (
    Y is X*X,
    write((X, Y)),
    nl
).
```

Podemos utilizar la expansión de objetivos para desplegar los bucles en tiempo de compilación. Para que este tipo de construcciones sean válidas, primero debemos declarar algunos operadores (`for`, `from`, `to` y `do`). A continuación declaramos la expansión del objetivo de la forma `(for Var from N to M do Body)` a una conjunción de objetivos `(Bn, B(n+1), ..., Bm)`, donde la variable `Var` se ha reemplazado en cada objetivo `Bi` por el valor correspondiente de la `i`-ésima iteración desde `N` hasta `M`.

```prolog
:- use_module(library(lists)).
:- op(850, xfx, do).
:- op(800, fx, for).
:- op(700, xfx, from).
:- op(600, xfx, to).

goal_expansion(for _ from N to M do _, true) :- N > M.
goal_expansion(for Var from N to M do Body, (BodyN,BodyM)) :-
    N =< M,
    succ(N, N2),
    copy_term((Var,Body), (N,BodyN)),
    goal_expansion(for Var from N2 to M do Body, BodyM).
```

Nótese que el predicado `copy_term/2` crea copias idénticas del cuerpo original `Body`, pero con variables renombradas (frescas), ya que cada iteración debe utilizar sus propias variables sin entrar en conflicto con las variables de otras iteraciones. Ahora podemos comprobar que el compilador ha desplegado el bucle del predicado `squares/0` de la siguiente forma:

```prolog
squares :-
    (_573 is 1*1, write((1, _573)), nl),
    (_800 is 2*2, write((2, _800)), nl),
    (_1027 is 3*3, write((3, _1027)), nl),
    (_1254 is 4*4, write((4, _1254)), nl),
    (_1481 is 5*5, write((5, _1481)), nl),
    (_1708 is 6*6, write((6, _1708)), nl),
    (_1935 is 7*7, write((7, _1935)), nl),
    (_2162 is 8*8, write((8, _2162)), nl),
    (_2389 is 9*9, write((9, _2389)), nl),
    (_2616 is 10*10, write((10, _2616)), nl),
    true.
```

```prolog
?- squares.
1,1
2,4
3,9
4,16
5,25
6,36
7,49
8,64
9,81
10,100
```

___

**Ejemplo 3.** Queremos definir predicados *al vuelo*, como si de funciones anónimas se tratasen, para ser utilizadas como argumento en otros predicados de orden superior. Por ejemplo:

```prolog
?- F@([X,Y] => Y is X+1), maplist(F, [1,2,3,4,5,6], Xs).
F = '_lambda0', Xs = [2,3,4,5,6,7].

?- F@([X] => 0 is X mod 2), include(F, [1,2,3,4,5,6], Xs).
F = '_lambda1', Xs = [2,4,6].
```

Podemos utilizar la expansión de objetivos para añadir a la base de datos estos predicados *anónimos* en tiempo de compilación. Como siempre, primero definimos algunos operadores (`@` y `=>`). A continuación declaramos la expansión del objetivo `(V@([A1, A2, ..., An] => Body))` a un término `(V = P)`, que unifica la variable `V` con el nombre asignado al predicado anónimo `_lambdai/n` que se ha añadido a la base de datos -definido como `(_lambdai(A1, A2, ..., An) :- Body)`-, siendo `i` un identificador automático que se incrementa cada vez que se expande uno de estos predicados anónimos.

```prolog
:- use_module(library(lists)).
:- op(1150, xfx, =>).
:- op(100, xfx, @).

:- dynamic(current_lambda/1).
current_lambda(0).

goal_expansion(V@(Head => Body), V = P) :-
    current_lambda(Id),
    retract(current_lambda(_)),
    succ(Id, Id_),
    asserta(current_lambda(Id_)),
    number_chars(Id, Chars),
    atom_chars(Atom, Chars),
    atom_concat('_lambda', Atom, P),
    F =.. [P|Head],
    assertz((F :- Body)).
```

Con esta expansión, el siguiente predicado `double_me/2`:

```prolog
double_me(Xs, Ys) :-
    F@([X,Y] => Y is X*2),
    maplist(F, Xs, Ys).
```

es compilado de la siguiente forma:

```prolog
'_lambda0'(X, Y) :- Y is X*2.

double_me(Xs, Ys) :-
    F = '_lambda0',
    maplist(F, Xs, Ys).
```

```prolog
?- double_me([1,2,3], Xs).
Xs = [2,4,6].
```

Nótese que también se pueden definir estos predicados anónimos al lanzar directamente un objetivo, no sólo en el cuerpo de una regla.

___

## Referencias

* [The Power of Prolog: Prolog Macros](https://www.metalevel.at/prolog/macros)
* [SWI-Prolog Manual: Conditional compilation and program transformation](https://www.swi-prolog.org/pldoc/man?section=progtransform)
* [SICStus Prolog: Term and Goal Expansion](https://sicstus.sics.se/sicstus/docs/3.12.9/html/sicstus/Term-and-Goal-Expansion.html)
