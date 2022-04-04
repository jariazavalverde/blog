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

/*
squares :- for X from 1 to 10 do (
    Y is X*X,
    write((X, Y)),
    nl
).
*/