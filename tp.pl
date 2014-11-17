%--------------------------Ejercicio 1 -----------------------------------

maximo(ab(X, nil, nil), X).
maximo(ab(X, T1, nil), Y) :- maximo(T1, Z), Y is max(X, Z).
maximo(ab(X, nil, T2), Y) :- maximo(T2, Z), Y is max(X, Z).
maximo(ab(X, T1, T2), Y)  :- maximo(T1, Z1), maximo(T2, Z2), Y is max(max(X, Z1), Z2).

minimo(ab(X, nil, nil), X).
minimo(ab(X, T1, nil), Y) :- minimo(T1, Z), Y is min(X, Z).
minimo(ab(X, nil, T2), Y) :- minimo(T2, Z), Y is min(X, Z).
minimo(ab(X, T1, T2), Y)  :- minimo(T1, Z1), minimo(T2, Z2), Y is max(max(X, Z1), Z2).

abb(nil).
abb(ab(_, nil, nil)).
abb(ab(X, T1, nil)) :- maximo(T1, Mx), X >= Mx, abb(T1).
abb(ab(X, nil, T2)) :- minimo(T2, Mn), Mn >= X, abb(T2).
abb(ab(X, T1, T2))  :- maximo(T1, Mx), X >= Mx, minimo(T2, Mn), Mn >= X, abb(T1), abb(T2).



%--------------------------Ejercicio 2 -----------------------------------

list_to_abb([], nil).
list_to_abb(UXS, ab(X, T1, T2)) :-
    sort(UXS, XS),              
    append(T1S, [X|T2S], XS),
    list_to_abb(T1S, T1),
    list_to_abb(T2S, T2). 



%--------------------------Ejercicio 3 -----------------------------------

diff(1, [0, 0]).
diff(H, [H1, H2]) :- H>1, H1 is H-1, H2 is H-1.
diff(H, [H1, H2]) :- H>1, H1 is H-1, H2 is H-2.
diff(H, [H1, H2]) :- H>1, H1 is H-2, H2 is H-1.

abbal(nil, 0).
abbal(ab(_, T1, T2), H) :- diff(H, [H1,H2]), abbal(T1, H1), abbal(T2, H2). 


%--------------------------Ejercicio 4 -----------------------------------


altura(nil, 0).
altura(ab(_,I, D), H):-  altura(I, HI), altura(D, HD), H is 1+max(HI,HD).

list_to_avl([], nil).
list_to_avl(XS, T) :- list_to_abb(XS, T), altura(T, H), abbal(T, H).
	

%--------------------------Ejercicio 5 -----------------------------------
/*
 * La conclucion que sacamos al evaluar ambas soluciones, es que el predicado list_to_abb 
 * genera mucho menos árboles que abbal. Sin embargo las restricciones que usamos en el 
 * segundo caso hace que la brecha se reduzca considerablemente.
 * 
 *  
 */

list_to_avl2([], nil).
list_to_avl2(XS, T) :- 
    length(XS, LX),
    X is integer(ceil(log10(LX)/log10(2))),  %minima altura posible para un avl%
    Y is X+1,  				     %altura maxima para el avl%
    between(X, Y, H), 
    abbal(T, H), list_to_abb(XS, T).



