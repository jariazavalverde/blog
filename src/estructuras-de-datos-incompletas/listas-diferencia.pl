% append/3
append([], X, X).
append([H|T], X, [H|S]) :- append(T, X, S).

% permutation/2
permutation([], []).
permutation([H|T], S) :- permutation(T, P), append(X, Y, P), append(X, [H|Y], S).

% flatten/2
flatten([],[]).
flatten([H|T], L) :- flatten(T, S), append(H, S, L).

% append_diff/3
append_diff(A-B, B-C, A-C). % append_diff(A-Ha, B-Hb, C-Hc) :- Ha = B, C = A, Hc = Hb.

% flatten_diff/2
flatten_diff([], X-X).
flatten_diff([A-B|T], A-C) :- flatten_diff(T, B-C).