# Prolog - Gramáticas de cláusulas definidas
> Procesamiento de gramáticas formales

Las **gramáticas de cláusulas definidas** (**DCG's**) permiten representar una gramática formal como un conjunto de cláusulas definidas en una lógica de primer orden. Estas DCG's proporcionan a los lenguajes de programación lógicos como Prolog una forma conveniente y efectiva de expresar gramáticas, resultando especialmente útiles en el **procesamiento** de **lenguajes naturales** y de **lenguajes formales**.

***Nota.*** *Como veremos a continuación, las DCG's están muy relacionadas con las **listas abiertas** y las **listas diferencia**. Puedes leer el artículo [Prolog - Estructuras de datos incompletas](https://github.com/jariazavalverde/blog/blob/master/posts/prolog/estructuras-de-datos-incompletas.md) para obtener más información sobre estas.*

## Sintaxis

Una **gramática formal** es una estructura matemática con un conjunto de reglas de producción que definen las cadenas admisibles en un determinado lenguaje formal o lenguaje natural.

***Ejemplo.*** *Un sencillo ejemplo ayuda a ilustrar qué son estas gramáticas y para qué sirven. Aquí, decimos que una oración está formada por un sujeto y un predicado, tal y como aprendimos en el colegio. Un sujeto está formado a su vez por un determinante y un sustantivo, y un predicado está formado por un verbo seguido de un sustantivo. Declaramos los determinantes `el` y `un`, los sustantivos `gato` y `pescado`, y el verbo `come`. Ahora, esta gramática nos permite comprobar si determinadas cadenas son correctas o no, como por ejemplo, `el gato come pescado` o `come gato pescado el`. Más aún, esta gramática puede ser utilizada para derivar, por reevaluación, todas las cadenas permitidas por el lenguaje descrito.*

```prolog
oracion --> sujeto, predicado.
sujeto --> determinante, sustantivo.
predicado --> verbo, sustantivo.
determinante --> [el].
determinante --> [un].
sustantivo --> [gato].
sustantivo --> [pescado].
verbo --> [come].

% ?- oracion([el, gato, come, pescado], []).
% true.

% ?- oracion([come, gato, pescado, el], []).
% false.

% ?- oracion(X, []).
% X = [el, gato, come, gato] ;
% X = [el, gato, come, pescado] ;
% X = [el, pescado, come, gato] ;
% X = [el, pescado, come, pescado] ;
% X = [un, gato, come, gato] ;
% X = [un, gato, come, pescado] ;
% X = [un, pescado, come, gato] ;
% X = [un, pescado, come, pescado].
```

Como vemos en el ejemplo anterior, las producciones de la DCG se definen en Prolog mediante reglas de la forma `Cabeza --> Cuerpo`, donde la cabeza representa un símbolo no terminal de la gramática y el cuerpo es una cadena de símbolos terminales y no terminales. Un símbolo **terminal** se expresa en Prolog como una lista, que representa los elementos que contiene. Un símbolo **no terminal** se refiere a otra construcción de la gramática, que representa los elementos que ella misma describe.

En realidad, esta notación de DCG es simplemente azúcar sintáctico para las cláusulas definidas normalmente en Prolog. Aquí es donde entran en juego las listas abiertas y las listas diferencia.

***Ejemplo.*** *Podemos expresar la gramática anterior como un conjunto de cláusulas Prolog estándar de la siguiente forma. En estos predicados, la primera lista contiene los elementos que se deben analizar, y cada uno busca lo que necesita al frente de la primera lista, unificando la segunda lista con lo que queda por analizar. Por ejemplo, cuando `oracion/2` busca un sujeto en `[el, gato, come, pescado]`, `sujeto/2` lo encuentra (`[el, gato]`) y deja el resto de la lista (`[come, pescado]`) para que ahora `predicado/2` busque un predicado. Como el predicado es `[come, pescado]`, ahora `predicado/2` deja una lista vacía (`[]`) como resto por analizar, y como `predicado/2` era el último símbolo de la regla `oracion/2`, `oracion/2` deja por analizar esa misma lista vacía. Por lo tanto, cuando lanzamos el objetivo `oracion([...], [])` estamos preguntando si es posible analizar por completo todos los símbolos de la primera lista mediante esa regla.*

```prolog
oracion(L1, L3) :- sujeto(L1, L2), predicado(L2, L3).
sujeto(L1, L3) :- determinante(L1, L2), sustantivo(L2, L3).
predicado(L1, L3) :- verbo(L1, L2), sustantivo(L2, L3).
determinante([el|L], L).
determinante([un|L], L).
sustantivo([gato|L], L).
sustantivo([pescado|L], L).
verbo([come|L], L).

% ?- oracion([el, gato, come, pescado], []).
% true.

% ?- oracion([come, gato, pescado, el], []).
% false.

% ?- oracion(X, []).
% X = [el, gato, come, gato] ;
% X = [el, gato, come, pescado] ;
% X = [el, pescado, come, gato] ;
% X = [el, pescado, come, pescado] ;
% X = [un, gato, come, gato] ;
% X = [un, gato, come, pescado] ;
% X = [un, pescado, come, gato] ;
% X = [un, pescado, come, pescado].
```

Cada vez que introducimos una regla de producción `Cabeza --> Cuerpo` en un intérprete de Prolog, este transforma la sintaxis de DCG a una cláusula Prolog estándar de la forma que acabamos de ver.

## Gramáticas libres de contexto

Una **gramática libre de contexto** es una gramática formal en la que cada regla de producción es de la forma `A → α`, donde `A` es un símbolo no terminal y `α` es una cadena de símbolos terminales y no terminales. El término libre de contexto se refiere al hecho de que el símbolo no terminal `A` siempre puede ser reemplazado por `α` sin tener en cuenta el contexto en el que ocurra.

¿Te suena esta definición? Son prácticamente las mismas palabras que acabamos de utilizar para describir las gramáticas de cláusulas definidas de Prolog. Esto es porque las DCG's tal y como las hemos visto hasta el momento, sólo permiten analizar lenguajes libres de contexto.

***Ejemplo.*** *Una simple gramática libre de contexto formada por las siguientes reglas de producción:*

* *`A → aA | B`*
* *`B → bB | C`*
* *`C → cC | ε`*

*genera el lenguaje* `{a*b*c*}`*, donde `|` es un operador usado para separar múltiples opciones para un mismo no terminal, los símbolos `A`, `B` y `C` son no terminales, los símbolos `a`, `b` y `c` son terminales, y `ε` indica una cadena vacía.*

*Podemos expresar esta gramática en Prolog con DCG's tal y como hicimos anteriormente. Vemos que en Prolog la producción vacía `ε` se corresponde con una lista vacía. Tal y como esperábamos, podemos generar cadenas con cualquier número de símbolos `a`, `b` y `c`, en ese orden. Por eso, una vez que se ha encontrado una `b` no es posible que aparezca una `a` y la cadena `[b,a,c]` no puede ser derivada en este lenguaje.*

```prolog
a --> [a], a.
a --> b.
b --> [b], b.
b --> c.
c --> [c], c.
c --> [].

% ?- a([a,b,c], []).
% true.

% ?- a([a,a,a,b,b,c], []).
% true.

% ?- a([b,a,c], []).
% false.
```

Las gramáticas libres de contexto son simples y permiten describir la sintaxis de la mayoría de los lenguajes de programación. No obstante, no son lo suficientemente expresivas para describir cualquier lenguaje formal. Supongamos que en lugar de generar el lenguaje `{a*b*c*}` estuviésemos interesados en generar cadenas con el mismo número de terminales `a`, `b` y `c`, es decir, el lenguaje `{aⁿbⁿcⁿ : n>0}`. Para ello, debemos recurrir a las gramáticas sensibles al contexto.

## Gramáticas sensibles al contexto

Una **gramática sensible al contexto** es una gramática formal en la que cada regla de producción es de la forma `α → β`, donde `α` y `β` son cadenas de símbolos terminales y no terminales que no permiten producciones vacías (`ε`).

***Ejemplo.*** *La gramática sensible al contexto formada por las siguientes reglas de producción:*

* *`A → abc | aABc`*
* *`cB → Bc`*
* *`bB → bb`*

*genera el lenguaje `{aⁿbⁿcⁿ : n>0}`, donde los símbolos `A`, `B` y `C` son no terminales y los símbolos `a`, `b` y `c` son terminales.*

En Prolog, no podemos trasladar directamente estas gramáticas sensibles al contexto a la sintaxis de DCG's, pero sí podemos conseguir una expresividad similar añadiendo parámetros adicionales a las reglas de producción de las gramáticas.

***Ejemplo.*** *En la siguiente DCG hemos añadido un parámetro adicional a las reglas de producción, para indicar el número de símbolos a analizar. Si bien antes estas reglas eran traducidas a cláusulas Prolog estándar con 2 argumentos, ahora son traducidas a cláusulas con 3 argumentos (2 argumentos para las listas abiertas y 1 para nuestro parámetro adicional). También es importante observar que podemos invocar predicados Prolog arbitrarios en medio de las reglas de producción, encerrando estas llamadas en el término `{}/1`. De esta forma, le indicamos al intérprete que esas regiones de código no forman parte de la gramática y por lo tanto no deben ser traducidas a la sintaxis de listas abiertas.*

```prolog
s(N) --> a(N), {N > 0}, b(N), c(N).
a(N) --> [a], a(M), {succ(M, N)}.
a(0) --> [].
b(N) --> [b], b(M), {succ(M, N)}.
b(0) --> [].
c(N) --> [c], c(M), {succ(M, N)}.
c(0) --> [].

% ?- s(X, [a,a,b,b,c,c], []).
% X = 2.

% ?- s(X, [a,a,a,b,b,b,c,c,c], []).
% X = 3.

% ?- s(X, [a,b,b,c,c,c], []).
% false.
```

## Árboles de análisis

El uso más práctico y común de los parámetros adicionales en las gramáticas de cláusulas definidas es el de capturar información sobre el proceso de análisis de una cadena para construir su árbol de análisis.

***Ejemplo.*** *La siguiente gramática analiza expresiones aritméticas formadas por números naturales, paréntesis y los operadores binarios `(+)/2` y `(-)/2`.*

* *`Expresion → Termino + Termino | Termino - Termino | Termino`*
* *`Termino → Natural | ( Expresion )`*
* *`Natural → Digito Natural | Digito`*
* *`Digito → 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9`*

*A continuación se muestra una posible implementación de esta gramática en Prolog, que permite analizar cadenas generadas por este lenguaje, proporcionando además un árbol de análisis de la expresión analizada. La regla `digito/3` analiza un carácter y comprueba que es un dígito. La regla `digitos/3` obtiene una secuencia de dígitos. La regla `natural/3` obtiene una secuencia no vacía de dígitos y la convierte en un número. La regla `termino/3` permite analizar un número natural o una expresión entre paréntesis. Y por último, la regla `expresion/3` permite analizar términos, así como sumas y restas de términos.*

```prolog
digito(X) --> [X], {char_code(X, C), C >= 48, C =< 57}.

digitos([X|Xs]) --> digito(X), !, digitos(Xs).
digitos([]) --> [].

natural(X) --> digitos(Xs), {Xs \= [], number_chars(X, Xs)}.

expresion(add(X,Y)) --> termino(X), [+], termino(Y).
expresion(sub(X,Y)) --> termino(X), [-], termino(Y).
expresion(X) --> termino(X).

termino(X) --> natural(X).
termino(X) --> ['('], expresion(X), [')'].

% ?- atom_chars('3098', C), expresion(X, C, []).
% X = 3098.

% ?- atom_chars('102+(22-35)', C), expresion(X, C, []).
% X = add(102, sub(22, 35)).

% ?- atom_chars('(55-21)+(73-2)', C), expresion(X, C, []).
% X = add(sub(55, 21), sub(73, 2)).
```
