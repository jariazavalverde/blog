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

% head :: [a] -> a
head([H|_]) := H.

% add :: Int -> Int -> Int
add(X, Y) := Z is X+Y, Z.
