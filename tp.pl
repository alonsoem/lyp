arbol(nil).
arbol(ab(_, T1, T2)) :- arbol(T1), arbol(T2).

maximo(ab(X, nil, nil), X).
maximo(ab(X, T1, nil), Y) :- maximo(T1, Z), Y is max(X, Z), !.
maximo(ab(X, nil, T2), Y) :- maximo(T2, Z), Y is max(X, Z), !.
maximo(ab(X, T1, T2), Y)  :- maximo(T1, Z1), maximo(T2, Z2), Y is max(max(X, Z1), Z2), !.

minimo(ab(X, nil, nil), X).
minimo(ab(X, T1, nil), Y) :- minimo(T1, Z), Y is min(X, Z), !.
minimo(ab(X, nil, T2), Y) :- minimo(T2, Z), Y is min(X, Z), !.
minimo(ab(X, T1, T2), Y)  :- minimo(T1, Z1), minimo(T2, Z2), Y is max(max(X, Z1), Z2), !.

abb(nil).
abb(ab(_, nil, nil)).
abb(ab(X, T1, nil)) :- maximo(T1, Mx), X >= Mx, abb(T1), !.
abb(ab(X, nil, T2)) :- minimo(T2, Mn), Mn >= X, abb(T2), !.
abb(ab(X, T1, T2))  :- maximo(T1, Mx), X >= Mx, minimo(T2, Mn), Mn >= X, abb(T1), abb(T2), !.

contains(ab(X, _, _), X).
contains(ab(_, T1, _), X) :- contains(T1, X).
contains(ab(_, _, T2), X) :- contains(T2, X).


/* Cambie la definicion de esta regla, fijate si te va! */
list_to_abb([], nil).
list_to_abb(XS, ab(X, T1, T2)) :-
    append(T1S, [X|T2S], XS),
    list_to_abb(T1S, T1),
    list_to_abb(T2S, T2).


max(X, Y, X):- X >= Y,!.
max(X, Y, Y):- X < Y, !.

altura(nil, 0).
altura(ab(_, I, D), H) :- altura(I, HI), altura(D, HD), max(HI, HD, X), H is 1+X.

sumas(0, X, X).
sumas(X, 0, X).
sumas(X, Y, Z) :- X > 0, sumas(X-1, Y+1, Z).

toN(zero, 0).
toN(suc(M), V):- toN(M, N), V is N+1.

add(zero, M, M).
add(suc(N), M, suc(S)):- add(N, M, S).


abbal(nil, 0).
abbal(ab(_, nil, nil), 1).
abbal(ab(_, T1, T2), H):- 
    H > 1,
    X is H-1,
    abbal(T1, X), abbal(T2, X).
abbal(ab(_, T1, T2), H):- 
    H > 1,
    X is H-1,
    H2 is X-1,
    abbal(T1, X), abbal(T2, H2).
abbal(ab(_, T1, T2), H):- 
    H > 1,
    X is H-1,
    H1 is X-1,
    abbal(T1, H1), abbal(T2, X).

/* Esta esta no funca ahora.*/
list_to_avl([], nil).
list_to_avl(XS, T) :- list_to_abb(XS, T), abbal(T, _).


/* Esto es para analizar lo que dice el punto 5*/
list_to_avl2([], nil).
list_to_avl2(XS, T) :- abbal(T, _), list_to_abb(XS, T).



