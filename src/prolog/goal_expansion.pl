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

squares :- for x from 1 to 10 do (
    Y is x*x,
    write((x, Y))
).
