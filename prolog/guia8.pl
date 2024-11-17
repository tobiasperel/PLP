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

natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X,X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).

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
coprimos(X, Y) :- gcd(X, Y) =:= 1.  

esPrimo()
esPrimo(X,)
esPrimo(X,V) :-

esPrimoAux(X,X) .
esPrimoAux()