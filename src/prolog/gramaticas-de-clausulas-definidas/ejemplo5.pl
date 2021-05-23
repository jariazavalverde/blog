digito(X) --> [X], {char_code(X, C), C >= 48, C =< 57}.

digitos([X|Xs]) --> digito(X), !, digitos(Xs).
digitos([]) --> [].

natural(X) --> digitos(Xs), {Xs \= [], number_chars(X, Xs)}.

termino(X) --> natural(X).
termino(X) --> ['('], expresion(X), [')'].

factor(mul(X,Y)) --> termino(X), [*], termino(Y).
factor(X) --> termino(X).

expresion(add(X,Y)) --> factor(X), [+], factor(Y).
expresion(X) --> factor(X).