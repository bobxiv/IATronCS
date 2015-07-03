%Tron
%Autores:
%  Emmanuel Rojas Fredini
%  Walter Bedrij

%Hechos
:- dynamic ocupado/3, celda/2.
%inicial ocupado informacion tron hero
ocupado(X , 0, 1) :- between(0,11,X).
ocupado(X ,11, 1) :- between(0,11,X).
ocupado(0 , X, 1) :- between(0,11,X).
ocupado(11, X, 1) :- between(0,11,X).

%inicial celda informacion ambiente
celda([1,1]  , hero  ). %agentes
celda([10,10], dragon).
celda([X,Y]  , wall   ) :- not(inside(X,Y)).
celda([X,Y]  , libre  ) :-
	inside(X,Y),
	not(celda([X,Y], hero);celda([X,Y], dragon);celda([X,Y], wall)).



:- dynamic pos/3.
pos(1,1,1).

%Acciones
irDerecha(1,S)  :- pos(X,Y,S), Aux is X+1, not(ocupado(Aux,Y  ,S)).%, write('entro en irDerecha'), nl.
irIzquierda(2,S):- pos(X,Y,S), Aux is X-1, not(ocupado(Aux,Y  ,S)).%, write('entro en irIzquierda'), nl.
irArriba(3,S)   :- pos(X,Y,S), Aux is Y+1, not(ocupado(X  ,Aux,S)).%, write('entro en irArriba'), nl.
irAbajo(4,S)    :- pos(X,Y,S), Aux is Y-1, not(ocupado(X  ,Aux,S)).%, write('entro en irAbajo'), nl.

% A=1 -> irDerecha
% A=2 -> irIzquierda
% A=3 -> irArriba
% A=4 -> irAbajo


%Reglas de Diagnostico
p([1, B, C, D], S):- pos(X,Y,S), Aux is X+1, assert(ocupado(Aux,Y  ,S)), !, (p([0,B,C,D],S);true).
p([_, 1, C, D], S):- pos(X,Y,S), Aux is X-1, assert(ocupado(Aux,Y  ,S)), !, (p([0,0,C,D],S);true).
p([_, _, 1, D], S):- pos(X,Y,S), Aux is Y+1, assert(ocupado(X  ,Aux,S)), !, (p([0,0,0,D],S);true).
p([_, _, _, 1], S):- pos(X,Y,S), Aux is Y-1, assert(ocupado(X  ,Aux,S)), !.


%Reglas Estado Sucesor
%regla para pos
es1(1,S) :- pos(X,Y,S), Snext is S+1, Aux is X+1, assert(pos(Aux,Y  ,Snext)), assert(ocupado(X,Y,Snext)).
es1(2,S) :- pos(X,Y,S), Snext is S+1, Aux is X-1, assert(pos(Aux,Y  ,Snext)), assert(ocupado(X,Y,Snext)).
es1(3,S) :- pos(X,Y,S), Snext is S+1, Aux is Y+1, assert(pos(X  ,Aux,Snext)), assert(ocupado(X,Y,Snext)).
es1(4,S) :- pos(X,Y,S), Snext is S+1, Aux is Y-1, assert(pos(X  ,Aux,Snext)), assert(ocupado(X,Y,Snext)).

%regla para ocupado
es2(S) :-
	Snext is S+1,
	forall(
		ocupado(X,Y,S),
		assert(ocupado(X,Y,Snext))
	).

esta_ocupado([X,Y], 1, S) :- ocupado(X,Y,S), !.
esta_ocupado(_, 0, _).

convert([X,Y],0) :- celda([X,Y], libre), !.
convert(_,1).
%Funcion principal
%%   P es un vector de 4 elementos con las percepciones del ambiente
%%   S es el numero de situacion a simular
main(S, A):- pos(X,Y,S), Xmas is X+1, Ymas is Y+1, Xmenos is X-1, Ymenos is Y-1,
	convert([Xmas  ,Y     ],Est), %percepciones
	convert([Xmenos,Y     ],Oes),
	convert([X     ,Ymas  ],Nor),
	convert([X     ,Ymenos],Sur),

	p([Est,Oes,Nor,Sur],S),
	huida_del_rey(A,S),
	es1(A,S),
	es2(S),
	write('Tron: '), displayAccion(A).

%Muestra la accion a efectuar
displayAccion(A):-
	(
		(A=1) -> ( write('Accion: IrDerecha'  ), nl );
		(A=2) -> ( write('Accion: IrIzquierda'), nl );
		(A=3) -> ( write('Accion: IrArriba'   ), nl );
		(A=4) -> ( write('Accion: IrAbajo'	), nl )
	).



%Codigo sacado de internet, saca el nth de una lista
buscarNth([],_,_) :-
	true.

buscarNth([Element|_],1,Out) :-
        Out is Element.

buscarNth([_|List],N,Out) :-
        N1 is N-1,
        buscarNth(List,N1,Out).

%Funcion Principal, inicializa y corre el juego
runTron :-
	print_board,
	forall(
		between(1,100,S),
		(
		main(S, Aheroe),
      		doit(Aheroe , hero),!,
		celda([Xd,Yd], dragon),
		random_bot([Xd,Yd], Adragon),
		doit(Adragon, dragon),

		print_board,
		write('Tron    '), displayAccion(Aheroe),
		write('Enemigo '), displayAccion(Adragon),nl,
		get_single_char(_)
		)
	).

%Resetea el esatdo del juego para que pueda volver a ejecutarse runTron
restartTron :-
	retractall(ocupado(_,_,_)),
	retractall(    pos(_,_,_)),
	assert(	ocupado(X ,0 ,1) :- between(0,11,X) ),
	assert(	ocupado(X ,11,1) :- between(0,11,X) ),
	assert(	ocupado(0 ,X ,1) :- between(0,11,X) ),
	assert(	ocupado(11,X ,1) :- between(0,11,X) ),
	assert( pos(1,1,1) ),
	retractall(celda(_,_)),
	assert(celda([1,1]  , hero  )),
	assert(celda([10,10], dragon)),
	assert(celda([X,Y]  , wall   ) :- not(inside(X,Y))),
	assert( (celda([X,Y]  , libre  ) :- inside(X,Y),
	       not(celda([X,Y], hero);celda([X,Y], dragon);celda([X,Y], wall))) ).


inside(X,Y):-
	between(1,10,X),
	between(1,10,Y).

%Sale por [X2,Y] la posicion luego de hacer un movimiento hacia la Derecha
adyacente([X1,Y],[X2,Y], 1):-
	inside(X2,Y),
	plus(X1,1,X2).

%Sale por [X2,Y] la posicion luego de hacer un movimiento hacia la Izquierda
adyacente([X1,Y],[X2,Y], 2 ):-
	inside(X2,Y),
	plus(X1,-1,X2).

%Sale por [X,Y2] la posicion luego de hacer un movimiento hacia la Arriba
adyacente([X,Y1],[X,Y2], 3 ):-
	inside(X,Y2),
	plus(Y1,1,Y2).

%Sale por [X,Y2] la posicion luego de hacer un movimiento hacia la Abajo
adyacente([X,Y1],[X,Y2], 4 ):-
	inside(X,Y2),
	plus(Y1,-1,Y2).

avanzar([Xi,Yi], [Xf,Yf], Dir):-
	adyacente([Xi,Yi],[Xf,Yf], Dir),
	not(ocupado(Xf,Yf, _)).

%Se le pasa la accion a efectuar y a que agente y la realiza
doit(Action, Agent) :-
	!, celda([X,Y], Agent),
	avanzar([X,Y], [Xf,Yf], Action),
	celda([Xf,Yf], libre),
	retract( celda([X,Y]  , Agent) ), asserta( celda([X,Y]  , wall) ), %actualizando la grilla
	asserta( celda([Xf,Yf], Agent) ).

sim(Action, Action) :- between(1,4,Action), !.
sim(_, 1).


random_bot([X,Y], Action) :-
	findall(A, (adyacente([X,Y], [Xf,Yf], A), celda([Xf,Yf], libre)), P),
	length(P,CANT),
	( CANT = 0 -> (nl, write('Tron ha ganado! El programa ahora cree en el usuario'), nl, nl, !, fail);true),
	N is random(CANT), 	N1 is N+1,
	buscarNth(P,N1,Action).
%	write('Lista '), write(P), nl,
%	displayAccion(A),nl.


%Codigo de fillFlood, ingresa una posicion del ambiente y dice cuantas casillas se pueden pintar
%solo pinta las casillas vacias
:- dynamic pintado/2.

fillFlood(X,Y,COUNT,S):- retractall(pintado(_,_)), fillFloodAux(X,Y,S), contar(COUNT).

contar(COUNT):- findall([X,Y], pintado(X,Y), L), length(L,COUNT).

fillFloodAux(X,Y,S):- not(ocupado(X,Y,S)), not(pintado(X,Y)), assert(pintado(X,Y)),
       Xmas is X+1, Ymas is Y+1, Xmenos is X-1, Ymenos is Y-1,
       (   fillFloodAux(Xmas  ,Y     ,S) ; true),
       (   fillFloodAux(Xmenos,Y     ,S) ; true),
       (   fillFloodAux(X     ,Ymas  ,S) ; true),
       (   fillFloodAux(X     ,Ymenos,S) ; true).


%moverse hacia el lado que tiene mas casillas libres
%si no se puede, tratar de ir al centro
%si no se puede, tirar la moneda
huida_del_rey(Action,S) :-
	pos(X,Y,S),
	((maximizar([X,Y], Aux1, S), avanzar([X,Y], _, Aux1)) ->
		(Action = Aux1, write('maximales maximizar'));
		((centrar([X,Y], Aux2), avanzar([X,Y], _, Aux2)) ->
			(Action = Aux2, write('center centre'));
			(moneda([X,Y], Action), avanzar([X,Y], _, Action), write('Random Dent'))
		)
	), nl.

maximizar([X,Y],Action, S) :-
%	write('avispaneitor'),
	assert(ocupado(X,Y,S)), %para considerar la cabeza como inválida
	Xmas is X+1, Ymas is Y+1, Xmenos is X-1, Ymenos is Y-1,
	computar_libertad(
		[[Xmas, Y],[Xmenos, Y],[X, Ymas],[X, Ymenos]], %el orden es lo que importa
		S,
		Libertad
	),
	retract(ocupado(X,Y,S)),
	max_list(Libertad, Maximum),
	findall(A, posicion(Libertad, Maximum, A), L), %asegurandose de que solo exista 1
	(length(L,1) -> L = [Action]; fail).

%computar_libertad([[X,Y]|L], S, Result)
computar_libertad([], _, []).

computar_libertad([[X,Y]|Cdr], S, [Lib|Result]) :-
	(fillFlood(X,Y,Libaux,S) -> Lib=Libaux; Lib=0),
	computar_libertad(Cdr, S, Result).

%computar_libertad(L,_,_) :-
%	nl,write('default '), write(L), fail.

%posicion(List, Elem, Index). base 1
posicion([], _, _) :- fail.
posicion([X|_], X, 1).
posicion([_|L], X, Pos) :- posicion(L,X,Aux), plus(Aux, 1, Pos).

diff(X,X,0).
diff(Xi,Xf,1) :- Xf > Xi.
diff(Xi, Xf, -1) :- Xf < Xi.

diff_list([],[],[]).
diff_list([A|Ca], [B|Cb], [R|Cr]) :- diff(A,B,R), diff_list(Ca,Cb,Cr).

%redondeo, el centro esta es 5,6
centrar([X,Y], Action) :-
	diff_list([X,Y], [5,6], [Dx, Dy]),
	((Dx = 0, Dy = 0) ->
		fail;
	%	fail;
		((Dx = 1; Dx = -1) ->
			can_move_h([X,Y], Dx, Action);
			((Dy = 1;Dy = -1) ->
				can_move_v([X,Y], Dy, Action);
				fail
			)
		)
	).


%can_move_h(Inic, Delta, Action).
can_move_h(Inic, 1, 1) :- avanzar(Inic, _, 1).
can_move_h(Inic, -1, 2) :- avanzar(Inic, _, 2).
can_move_v(Inic, 1, 3) :- avanzar(Inic, _, 3).
can_move_v(Inic, -1, 4) :- avanzar(Inic, _, 4).



%aleatorio (como random_bot, pero no se mete en el ambiente)
moneda([X,Y], Action) :-
	findall(A, avanzar([X,Y], _, A), P),
	length(P,CANT),
	( CANT = 0 -> (nl, write('Tron ha perdido! El programa ya no cree en el usuario'), nl, nl, !, fail);true),
	N is random(CANT), 	N1 is N+1,
	buscarNth(P,N1,Action).

%Dibuja el ambiente
%%    T es el agente tron
%%    X es el tron maquina (dragon)
%%    * es una pared
%%    . es un espacio libre
print_board :-
	nl,
	forall(
		between(1,10,Y),
		(forall(
			between(1,10,X),
			(Y2 is 11-Y, celda([X,Y2], Estado),
			(Estado=libre ->
				 write('.');
				 (Estado=hero ->
				 	write('T');
				 	(Estado=dragon ->
				 		write('X');
				 		write('*')
				 	)
				 )
			)
			)),nl
		)
	).
