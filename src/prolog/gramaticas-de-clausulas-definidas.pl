oracion --> sujeto, predicado.       % oracion(L1, L3) :- sujeto(L1, L2), predicado(L2, L3).
sujeto --> determinante, sustantivo. % sujeto(L1, L3) :- determinante(L1, L2), sustantivo(L2, L3).
predicado --> verbo, sustantivo.     % predicado(L1, L3) :- verbo(L1, L2), sustantivo(L2, L3).
determinante --> [el].               % determinante([el|L], L).
determinante --> [un].               % determinante([un|L], L).
sustantivo --> [gato].               % sustantivo([gato|L], L).
sustantivo --> [pescado].            % sustantivo([pescado|L], L).
verbo --> [come].                    % verbo([come|L], L).



a --> [a], a.
a --> b.
b --> [b], b.
b --> c.
c --> [c], c.
c --> [].



s(N) --> a(N), {N > 0}, b(N), c(N).
a(N) --> [a], a(M), {succ(M, N)}.
a(0) --> [].
b(N) --> [b], b(M), {succ(M, N)}.
b(0) --> [].
c(N) --> [c], c(M), {succ(M, N)}.
c(0) --> [].



digito(X) --> [X], {char_code(X, C), C >= 48, C =< 57}.

digitos([X|Xs]) --> digito(X), !, digitos(Xs).
digitos([]) --> [].

numero(X) --> digitos(Xs), {Xs \= [], number_chars(X, Xs)}.

expresion(add(X,Y)) --> termino(X), [+], termino(Y).
expresion(sub(X,Y)) --> termino(X), [-], termino(Y).
expresion(X) --> termino(X).

termino(X) --> numero(X).
termino(X) --> ['('], expresion(X), [')'].