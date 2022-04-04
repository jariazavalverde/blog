# Transformación automática de programas

Toda cláusula Prolog es un término Prolog válido, lo cual implica que podemos analizar y procesar código Prolog utilizando predicados y características incorporadas del propio lenguaje. Esto es especialmente conveniente a la hora de definir transformaciones automáticas de programas. Aunque el estándar *ISO Prolog* no define ningún mecanismo de transformación de programas tales como la *expansión de macros* o la *compilación condicional*, todos los sistemas Prolog ampliamente utilizados proporcionan predicados que permiten reescribir código en tiempo de compilación.

Normalmente, en Prolog, estos mecanismos se proveen mediante la *expansión de términos* y la *expansión de objetivos*, en forma de predicados dinámicos: `term_expansion/2` y `goal_expansion/2`, respectivamente.

## Expansión de términos

Cuando el predicado `term_expansion/2` es definido por el usuario, todos los términos leídos durante la consulta de un programa son pasados por este predicado. Si `term_expansion(+Term1, -Term2)` tiene éxito, Prolog añadirá a la base de datos el término `Term2` proporcionado por este predicado en lugar del término `Term1` leído originalmente.

La notación de las [gramáticas de cláusulas definidas](https://jariaza.es/blog/gramaticas-de-clausulas-definidas) es uno de los principales usos de la expansión de términos, permitiendo compilar las producciones de una gramática de la forma `(Head --> Body)` a cláusulas Prolog estándar.

### Notación ecuacional

Vamos a definir una notación que nos permita expresar predicados con un estilo *ecuacional*, donde el resultado de la operación será la última expresión de la parte derecha de la ecuación. Por ejemplo:

```prolog
head([H|_]) := H.
add(X, Y) := Z is X+Y, Z.
```

Podemos utilizar la expansión de términos para transformar estas ecuaciones en predicados Prolog estándar. Para ello, primero declaramos el operador `:=` (no asociativo) con prioridad `1200`, y definimos un predicado `result/3` que dada una conjunción de objetivos `(A0, A1, ..., An)` nos devuelva el último objetivo `An` y el resto de la expresión `(A0, A1, ..., A(n-1))`. 

```prolog
:- op(1200, xfx, :=).

result(X, (A,C), D) :-
    nonvar(X),
    X = (A,B), !,
    result(B, C, D).
result(X, true, X).
```

Entonces, declaramos la expansión de un término de la forma `(A := B)` en un término de la forma `(A' :- B')`, donde `A'` es la parte izquierda `A` con un parámetro adicional -el último objetivo de la parte derecha `B`- y `B'` es la parte derecha `B` sin el último objetivo.

```prolog
term_expansion((Head1 := Expr), (Head2 :- Body)) :-
    Head1 =.. Xs,
    result(Expr, Body, Result),
    append(Xs, [Result], Ys),
    Head2 =.. Ys.
```

Ahora podemos comprobar que al introducir las dos ecuaciones anteriores, el compilador las transforma en los predicados Prolog `head/2` y `add/3`:

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

Como `term_expansion/2`, el predicado `goal_expansion/2` se utiliza a modo de expansión de macros de código Prolog. Entre la fase de expansión de términos y la fase de compilación, el cuerpo de las cláusulas analizadas y los objetivos son manejados por el predicado `goal_expansion/2`, que permite al usuario definir sus propias transformaciones. La fase de expansión de objetivos calcula un punto fijo aplicando transformaciones hasta que no hay más cambios.

### Predicados anónimos

Vamos a definir una notación que nos permita expresar predicados *al vuelo*, como si de funciones anónimas se tratasen, para ser utilizadas como argumento en otros predicados de orden superior. Por ejemplo:

```prolog
?- F@([X,Y] => Y is X+1), maplist(F, [1,2,3,4,5,6], Xs).
F = '_lambda0', Xs = [2,3,4,5,6,7].

?- F@([X] => 0 is X mod 2), include(F, [1,2,3,4,5,6], Xs).
F = '_lambda1', Xs = [2,4,6].
```

Podemos utilizar la expansión de objetivos para añadir a la base de datos estos predicados *anónimos* en tiempo de compilación. Primero, definimos algunos operadores (`@` y `=>`).

```prolog
:- op(1150, xfx, =>).
:- op(100, xfx, @).
```

A continuación, declaramos la expansión de un objetivo de la forma `(V@([A1, A2, ..., An] => Body))` en un término de la forma `(V = P)`, que unifica la variable `V` con el nombre asignado automáticamente al predicado anónimo `_lambdai/n`, cuya cláusula `(_lambdai(A1, A2, ..., An) :- Body)` se añadirá a la base de datos dinámicamente.

```prolog
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

Nótese que también es posible definir estos predicados anónimos al lanzar directamente un objetivo, no sólo en el cuerpo de una regla.

### Bucles

Como último ejemplo, vamos a definir una notación que nos permita expresar bucles, donde podamos realizar una acción repetidas veces, asignando distintos valores a una variable en un intervalo. Por ejemplo, el siguiente predicado `squares/0` imprime por la salida estándar los números comprendidos en el intervalo `[1, 10]` junto al valor de sus cuadrados:

```prolog
squares :- for X from 1 to 10 do (
    Y is X*X,
    write((X, Y)),
    nl
).
```

Podemos utilizar la expansión de objetivos para desplegar los bucles en tiempo de compilación. Para que este tipo de construcciones sean válidas, primero debemos declarar algunos operadores (`for`, `from`, `to` y `do`).

```prolog
:- op(850, xfx, do).
:- op(800, fx, for).
:- op(700, xfx, from).
:- op(600, xfx, to).
```

A continuación, declaramos la expansión de un objetivo de la forma `(for Var from N to M do Body)` a una conjunción de objetivos `(Bn, B(n+1), ..., Bm)`, donde la variable `Var` se ha reemplazado en cada objetivo `Bi` por el valor correspondiente de la `i`-ésima iteración desde `N` hasta `M`.

```prolog
goal_expansion(for _ from N to M do _, true) :- N > M.
goal_expansion(for Var from N to M do Body, (BodyN,BodyM)) :-
    N =< M,
    succ(N, N2),
    copy_term((Var,Body), (N,BodyN)),
    goal_expansion(for Var from N2 to M do Body, BodyM).
```

Nótese que el predicado `copy_term/2` crea copias idénticas del cuerpo original `Body`, pero con variables renombradas (frescas), ya que cada iteración debe utilizar sus propias variables sin entrar en conflicto con las variables de otras iteraciones. Ahora, podemos comprobar que el compilador ha desplegado el bucle del predicado `squares/0` de la siguiente forma:

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

## Referencias

* [The Power of Prolog: Prolog Macros](https://www.metalevel.at/prolog/macros)
* [SWI-Prolog Manual: Conditional compilation and program transformation](https://www.swi-prolog.org/pldoc/man?section=progtransform)
* [SICStus Prolog: Term and Goal Expansion](https://sicstus.sics.se/sicstus/docs/3.12.9/html/sicstus/Term-and-Goal-Expansion.html)
