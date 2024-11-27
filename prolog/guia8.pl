% 1

padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

hermano(X,Y) :- padre(Z,X), padre(Z,Y), X \= Y.

descendiente(X,Y) :- padre(X,Y).
descendiente(X,Y) :- padre(X,Z), descendiente(Z,Y).

ancestro(X, X).
ancestro(X, Y) :- padre(Z, X), ancestro(Z, Y) .

% 3

natural(cero).
natural(suc(X)) :- natural(X).
mayorOIgual(X,X) :- natural(X).

mayorOIgual(suc(X),Y) :- mayorOIgual(X, Y).


% 4
juntar(L1, L2, L3) :- append(L1, L2, L3).

% 5
ultimo([X], X).
ultimo([_|T], X) :- ultimo(T, X).

reversa([], []).
reversa([H|T], R) :- reversa(T, R1), append(R1, [H], R).

prefijo([], _).
prefijo([H|T1], [H|T2]) :- prefijo(T1, T2).

sufijo([], _).
sufijo(L1,L2) :- reversa(L1, L1R), reversa(L2, L2R), prefijo(L1R,L2R).

sublista([], []).

sublista([X|XS], [X|YS]) :-
    sublista(XS, YS).

sublista([_|XS], YS) :-
    sublista(XS, YS).

pertenencia(X, [X|_]).
pertenencia(X, [_|T]) :- pertenencia(X, T).


sacarRepetidos([],[]).
sacarRepetidos([X|Resto],Resultado) :- member(X, Resto), sacarRepetidos(Resto,Resultado).
sacarRepetidos([X|Resto],[X|Resultado]) :- not(member(X, Resto)), sacarRepetidos(Resto,Resultado).

%% 9

desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).

%%11 arbol binario

vacio(nil).
raiz(bin(_,V,_),V).
altura(nil,0).
altura(bin(Izq,_,Der),A) :- altura(Izq,A1), altura(Der,A2), A is max(A1,A2)+1.
%%bin(bin(nil,1,nil),2,bin(nil,3,nil))

cantidadDeNodos(nil,0).
cantidadDeNodos(bin(Izq,_,Der),A) :- altura(Izq,A1), altura(Der,A2), A is A1+A2+1.

inorder(nil,[]).
inorder(bin(Izq,V,Der),A) :- inorder(Izq,Rizq),inorder(Der,Rder), append(Rizq,[V|Rder],A).

%%13

coprimos(X,Y):- desde(1,N),N2 is N,between(1,N2,X), Y is N2-X, sonCoprimos(X,Y), X\=1, Y \= 1.

sonCoprimos(X, Y) :- Z is gcd(X, Y), Z =:= 1.

%%14

cuadradoSemiMagico(N, XS) :-
    desde(0,Sum),
    generarMatriz(N,N,Sum,XS). 

generarMatriz(_,0,_,[]).
generarMatriz(N,C,Sum,[X|XS]):- 
    C>0,
    crearLista(N,Sum,X),
    RestC is C-1,
    generarMatriz(N,RestC,Sum,XS).

crearLista(0, 0, []).
crearLista(N,Sum,[X|Lista]):-
    N>0,
    between(0,Sum,X),
    RestSum is Sum-X,
    RestN is N-1,
    crearLista(RestN,RestSum,Lista).


%%15
maximoTri(A,B,A):- A > B.
maximoTri(A,B,B):- A =< B.

esTriangulo(A,B,C) :- maximoTri(A,B,M1), maximoTri(C,M1,Maxi),
    Suma is A + B + C - Maxi, Suma > Maxi.


perimetro(tri(L1, L2, L3), P) :-
    nonvar(L1), nonvar(L2), nonvar(L3), %% aca se podria usar ground(tri(L1, L2, L3))
    esTriangulo(L1, L2, L3),
    P is L1 + L2 + L3.

perimetro(tri(L1, L2, L3), P) :-
    not(ground(tri(L1,L2,L3))), nonvar(P), 
    between(1,P,L1), P2 is P-L1,
    between(1,P2,L2), L3 is P2 -L2, L3 >0, esTriangulo(L1, L2, L3). 

perimetro(tri(L1, L2, L3), P) :-
    not(ground(tri(L1,L2,L3))), var(P), desde(1,N), between(1,N,L1),
    N2 is N-L1, between(1,N2,L2), L3 is N2-L2, L3>0, 
    esTriangulo(L1, L2, L3),P is L1 + L2 + L3.

%%18

primerosN(0,_,[]).
primerosN(N,[X|L],[X|L1]):-
    N>0,
    N2 is N-1,
    primerosN(N2,L,L1).

ultimosN(N,L,L2) :- reverse(L,LRev), primerosN(N,LRev,L2).

calcularSumaArray([],0).
calcularSumaArray([X|XS],Sum) :-calcularSumaArray(XS, RestSum), Sum is X + RestSum.

corteMásParejo(L,L1,L2) :- 
    unCorte(L,L1,L2,MinDiff),
    not((unCorte(L,_,_,MinDiff2), MinDiff2 < MinDiff)).
    
unCorte(L,L1,L2,MinDiff):-
    length(L,Largo), 
    between(1,Largo,N),
    primerosN(N,L,L1),
    RestL is Largo-N, 
    ultimosN(RestL,L,L2),
    calcularSumaArray(L1,SumaL1),
    calcularSumaArray(L2,SumaL2),
    MinDiff is abs(SumaL1-SumaL2).


corteMásParejo2(L, L1, L2) :-
    unCorte2(L, L1, L2, D1),
    not((unCorte2(L, _, _, D2), D2 < D1)).

unCorte2(L, L1, L2, D) :-
    append(L1, L2, L), %%recordar que apend te deja trabajar con dos listas no instanciadas
    sum_list(L1, S1),
    sum_list(L2, S2),
    D is abs(S1 - S2).


%%20
%%próximoNumPoderoso(+X,-Y)

próximoNumPoderoso(X,Y) :- unNumPodresoso(X,Y), not((unNumPodresoso(X,Z), Z<Y)).
unNumPodresoso(X,Y) :- factoral(X,ResX), between(X,ResX,N), raizCuadrada(N,P), esPrimo(P), Y is N. 

raizCuadrada(N, Raiz) :- raizCuadradaAux(N,Raiz,1).

raizCuadradaAux(N,Raiz,I) :- Cuadrado is I*I, N =:= Cuadrado, Raiz is I.

raizCuadradaAux(N,Raiz,I) :- 
    Cuadrado is I*I, N > Cuadrado, I2 is I+1, raizCuadradaAux(N,Raiz,I2).

esPrimo(X) :- esPrimoAux(X,2).
esPrimoAux(X,I) :- Z is gcd(X, I), Z =:= 1, I2 is I+1, esPrimoAux(X,I2).
esPrimoAux(X,X). 

factoral(1,1).
factoral(X,ResFact):- X>1, X2 is X-1, factoral(X2,ResFact2), ResFact is ResFact2*X.

primoDesde(X,P):- desde(X,P), esPrimo(P),!.

primoDesde2(X,X):- esPrimo(X).
primoDesde2(X,P):- not(esPrimo(X)), X2 is X+1, primoDesde2(X2,P).

%%parcialRecu

generarLista(0,[]).
generarLista(N,[X|Resto]) :- between(1,N,X), N2 is N-X, generarLista(N2,Resto).
esCapicua(L) :-reverse(L,LR), L == LR. 
generarCapicuas(L) :- desde(1,X), generarLista(X,L), esCapicua(L).

estaEnD([P|_],P).
estaEnD([_|Resto],Q):- estaEnD(Resto,Q).

tokenizar(_,[],[]).
tokenizar(D,F,[L1|TResto]) :- length(F,LargoFrase), between(1,LargoFrase,N), 
        append(L1,L2,F),length(L1,N), LargoRestante is LargoFrase-N,
        length(L2, LargoRestante), estaEnD(D,L1) , tokenizar(D,L2,TResto).

mayorCantPalabras(D,F,T) :- tokenizar(D, F, T), 
    length(T, LargoT),
    not((tokenizar(D, F, R), length(R, LargoR), LargoR > LargoT)).

/*
Diferencia entre =, == y =:=
=: El operador = se utiliza para unificar dos términos. Puede unificar variables con valores o estructuras.
==: El operador == se utiliza para verificar si dos términos son idénticos sin intentar unificarlos.
=:=: El operador =:= se utiliza para comparar el valor numérico de dos expresiones aritméticas después de evaluarlas. No unifica variables.
*/
reversa1([], []).
reversa1([X|Resto], R) :- reversa1(Resto, R2), append(R2, [X], R).

arbol(a, bin(bin(bin(nil,1,nil),2,nil),3,bin(nil,4,nil))).
obtenerArbol(A) :-
    arbol(a, A).

%%camino(A,C) :- caminoAux(A,C,[]).

camino(bin(nil, Num, nil), [Num]).
camino(bin(Izq, Num, _), [Num|Camino]) :-
    Izq \= nil,
    camino(Izq, Camino).
camino(bin(_, Num, Der), [Num|Camino]) :-
    Der \= nil,
    camino(Der, Camino).

caminoMasLargo(A,C) :- camino(A,C), length(C,LargoC), 
        not((camino(A,B), length(B,LargoB), LargoB > LargoC)).

caminoUnicoDeLong(A,N,C) :- camino(A,C), length(C,N), 
        not((camino(A,B), B \= C, length(B,N))).

sublistaConsecutiva(Lista, Sublista) :-
    sublistaDesde(Lista, Sublista).  
sublistaConsecutiva([_|Resto], Sublista) :-
    sublistaConsecutiva(Resto, Sublista). 

sublistaDesde(_, []).
sublistaDesde([X|XS], [X|YS]) :-
    sublistaDesde(XS, YS).

verTodosPrimos([]).
verTodosPrimos([X|Resto]):- esPrimo(X), verTodosPrimos(Resto).

subListaMasLargaDePrimos(L,SL) :-sublistaConsecutiva(L,SL), verTodosPrimos(SL),
    length(SL,Largo), not((sublistaConsecutiva(L,SL2),verTodosPrimos(SL2), 
    length(SL2,Largo2), Largo2 > Largo)).

pertenece(X, [X|_]).
pertenece(X, [M|Resto]) :- X \= M, pertenece(X, Resto).

conjuntoNaturales(E) :- not( (pertenece(E,X), not(natural(X))) ).

cantAparciones(_, [], 0).
cantAparciones(X,[X|Resto],R) :- cantAparciones(X,Resto,R2), R is R2 + 1.
cantAparciones(X,[Y|Resto],R) :-  X \= Y, cantAparciones(X,Resto,R2), R is R2.

masRepetido(L,X) :- cantAparciones(X,L,Cant), not((cantAparciones(_,L,Cant2),Cant2 > Cant)).

calcularSuma(_,0,[]).
calcularSuma([L1|RestoL],S,[L1|RestoP]):- S2 is S - L1, S2 >= 0, calcularSuma(RestoL, S2, RestoP).

partesQueSumen(L,S,P) :- subset2(L,SubConjunto), calcularSuma(SubConjunto,S,P).

subset2([], []).
subset2([X|Resto], [X|SubResto]) :- subset2(Resto, SubResto).
subset2([_|Resto], SubResto) :- subset2(Resto, SubResto).
