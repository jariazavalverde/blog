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

double_me(Xs, Ys) :-
    F@([X,Y] => Y is X*2),
    maplist(F, Xs, Ys).