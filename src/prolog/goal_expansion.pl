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

goal_expansion(for _ from N to M do _, true) :- N > M.
goal_expansion(for Var from N to M do Body, (BodyN,BodyM)) :-
    N =< M,
    succ(N, N2),
    copy_term(Body, Body2),
    replace(Var, N, Body2, BodyN),
    goal_expansion(for Var from N2 to M do Body, BodyM).

squares :- for x from 1 to 10 do (
    Y is x*x,
    write((x, Y)),
    nl
).