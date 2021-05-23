oracion(L1, L3) :-
    sintagma_nominal(L1, L2),
    sintagma_verbal(L2, L3).

sintagma_nominal(L1, L3) :-
    determinante(L1, L2),
    sustantivo(L2, L3).
sintagma_nominal(L1, L2) :-
    sustantivo(L1, L2).

sintagma_verbal(L1, L3) :-
    verbo(L1, L2),
    sintagma_nominal(L2, L3).

determinante([los|L], L).
determinante([un|L], L).

sustantivo([belmont|L], L).
sustantivo([vampiros|L], L).
sustantivo([colmillos|L], L).

verbo([matan|L], L).
verbo([tienen|L], L).