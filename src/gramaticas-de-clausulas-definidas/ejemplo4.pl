s(N) --> a(N), {N > 0}, b(N), c(N).

a(N) --> [a], a(M), {succ(M, N)}.
a(0) --> [].

b(N) --> [b], b(M), {succ(M, N)}.
b(0) --> [].

c(N) --> [c], c(M), {succ(M, N)}.
c(0) --> [].