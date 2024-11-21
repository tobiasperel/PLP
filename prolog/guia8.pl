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

sublista([], _).
sublista([H|T1], [H|T2]) :- sublista(T1, T2).
sublista([H|T1], [_|T2]) :- sublista([H|T1], T2).

pertenencia(X, [X|_]).
pertenencia(X, [_|T]) :- pertenencia(X, T).


% 6


/*
7.
intersección(+L1, +L2, -L3), tal que L3 es la intersección sin repeticiones de las listas L1 y L2, respe-
tando en L3 el orden en que aparecen los elementos en L1.
partir(N, L, L1, L2), donde L1 tiene los N primeros elementos de L, y L2 el resto. Si L tiene menos de N
elementos el predicado debe fallar. ¾Cuán reversible es este predicado? Es decir, ¾qué parámetros pueden
estar indenidos al momento de la invocación?
*/

sacarRepetidos([],[]).
sacarRepetidos([X|Resto],Resultado) :- member(X, Resto), sacarRepetidos(Resto,Resultado).
sacarRepetidos([X|Resto],[X|Resultado]) :- not(member(X, Resto)), sacarRepetidos(Resto,Resultado).

%% 9

desde(X,X).
desde(X,Y) :- N is X+1, desde(N,Y).


entre(X,X,X).
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