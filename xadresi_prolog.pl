%	Implementação Xadresi em Prolog
%	As peças são referenciadas em todo o codigo da seguinte maneira:
%		0 - Casa vazia;
%		1 - Rei Branco;		 R B 
%		2 - Rainha Branca;	 RaB 
%		3 - Cavalo Branco;	 C B 
%		4 - Torre Branca;	 T B 
%		5 - Bispo Branco;	 B B 
%		6 - Rei Preto;		 R P 
%		7 - Rainha Preta;	 RaP 
%		8 - Cavalo Preto;	 C P 
%		9 - Torre Preta;	 B P 
%		10 - Bispo Preto;	 T P 
%	Todas as declarações ":- dynamic xxx/Y" indicam predicados que são retracted, ou asserted ao longo da execução.

% Modulos externos
:- use_module(library(lists)).

% Estado inicial do jogo
% Turno
:- dynamic turno/1.
turno(brancas).

% Tabuleiro (inicialmente encontra-se vazio)
:- dynamic tabuleiro/1.
tabuleiro([[0, 0, 0, 0, 0, 0, 0, 0],
		   [0, 0, 0, 0, 0, 0, 0, 0],
		   [0, 0, 0, 0, 0, 0, 0, 0],
		   [0, 0, 0, 0, 0, 0, 0, 0],
		   [0, 0, 0, 0, 0, 0, 0, 0],
		   [0, 0, 0, 0, 0, 0, 0, 0],
		   [0, 0, 0, 0, 0, 0, 0, 0],
		   [0, 0, 0, 0, 0, 0, 0, 0]]).
		   
% Passagem de turno
passar_turno :- turno(brancas), retract(turno(brancas)), assert(turno(pretas)).
passar_turno :- turno(pretas), retract(turno(pretas)), assert(turno(brancas)).

% Quantidade de peças de cada equipa
:- dynamic quantidade_de_pecas/3.
quantidade_de_pecas(rei, brancas, 1).
quantidade_de_pecas(rainha, brancas, 1).
quantidade_de_pecas(cavalo, brancas, 2).
quantidade_de_pecas(torre, brancas, 2).
quantidade_de_pecas(bispo, brancas, 2).
quantidade_de_pecas(rei, pretas, 1).
quantidade_de_pecas(rainha, pretas, 1).
quantidade_de_pecas(cavalo, pretas, 2).
quantidade_de_pecas(torre, pretas, 2).
quantidade_de_pecas(bispo, pretas, 2).

% Posicionamento das peças
posicionar_peca(P, _, _) :- quantidade_de_pecas(P, _, X), X = 0, write("No more pieces left of that type.");
posicionar_peca(rei, X, Y) :- turno(E), replaceOnChessboard(1, X, Y), retract(quantidade_de_pecas(rei, E, 1)), assert(quantidade_de_pecas(rei, E, 0)).
posicionar_peca(rainha, X, Y) :- turno(E), replaceOnChessboard(2, X, Y), retract(quantidade_de_pecas(rainha, E, 1)), assert(quantidade_de_pecas(rainha, E, 0)).
posicionar_peca(cavalo, X, Y) :- turno(E), replaceOnChessboard(3, X, Y), retract(quantidade_de_pecas(cavalo, E, Q)), assert(quantidade_de_pecas(rainha, E, Q - 1)).
posicionar_peca(torre, X, Y) :- turno(E), replaceOnChessboard(4, X, Y), retract(quantidade_de_pecas(torre, E, Q)), assert(quantidade_de_pecas(rainha, E, Q - 1)).
posicionar_peca(bispo, X, Y) :- turno(E), replaceOnChessboard(5, X, Y), retract(quantidade_de_pecas(bispo, E, Q)), assert(quantidade_de_pecas(rainha, E, Q - 1)).

% Regras de movimentação das peças

% Torre
mover(torre, Xini, Yini, Xfin, Yfin) :- (Xini \= Xfin; Yini \= Yfin),
										write("Pelo menos uma das coordenadas finais tem de ser igual às iniciais. (Só se pode mover a torre horizontal ou verticalmente)"), !.

mover(torre, Xini, Yini, Xfin, Yfin) :- posicao_atual(torre1, E, Xini, Yini),
										posicao_dentro_tabuleiro(Xfin, Yfin),
										(Xini =:= Xfin; Yini =:= Yfin),
										turno(E),
										retract(posicao_atual(torre1, E, Xini, Yini)),
										assert(posicao_atual(torre1, E, Xfin, Yfin)).

mover(torre, Xini, Yini, Xfin, Yfin) :- posicao_atual(torre2, E, Xini, Yini),
										posicao_dentro_tabuleiro(Xfin, Yfin),
										(Xini =:= Xfin; Yini =:= Yfin),
										turno(E),
										retract(posicao_atual(torre2, E, Xini, Yini)),
										assert(posicao_atual(torre2, E, Xfin, Yfin)).

% Bispo
mover(bispo, Xini, Yini, DirHorizontal, DirVertical, Adder) :- (DirHorizontal \= 1; DirHorizontal \= -1),
															   (DirVertical \= 1; DirVertical \= -1),
															   write("Ambos a direção horizontal e vertical precisam de ter um valor de 1 ou -1."), !.
															   
mover(bispo, Xini, Yini, DirHorizontal, DirVertical, Adder) :- Adder = 0,
															   write("O fator de multiplicação tem de ser diferente de zero."), !.

mover(bispo, Xini, Yini, DirHorizontal, DirVertical, Adder) :- posicao_atual(bispo1, E, Xini, Yini),
															   posicao_dentro_tabuleiro(Xini + (DirHorizontal * Adder), Yini + (DirVertical * Adder)),
															   (DirHorizontal = 1; DirHorizontal = -1),
															   (DirVertical = 1; DirVertical = -1),
															   turno(E),
															   retract(posicao_atual(bispo1, E, Xini, Yini)),
															   assert(posicao_atual(bispo1, E, Xini + (DirHrizontal * Adder), Yini + (DirVertical * Adder)).

mover(bispo, Xini, Yini, DirHorizontal, DirVertical, Adder) :- posicao_atual(bispo2, E, Xini, Yini),
															   posicao_dentro_tabuleiro(Xini + (DirHorizontal * Adder), Yini + (DirVertical * Adder)),
															   (DirHorizontal = 1; DirHorizontal = -1),
															   (DirVertical = 1; DirVertical = -1),
															   turno(E),
															   retract(posicao_atual(bispo2, E, Xini, Yini)),
															   assert(posicao_atual(bispo2, E, Xini + (DirHorizontal * Adder), Yini + (DirVertical * Adder)).
															   
% Rainha
mover(rainha, Xini, Yini, DirHorizontal, DirVertical, Adder) :- (DirHorizontal > 1; DirHorizontal < -1),
																(DirVertical > 1; DirVertical < -1),
																write("Ambos a direção horizontal e vertical precisam de ter um valor de 1 ou -1."), !.
																
mover(rainha, Xini, Yini, DirHorizontal, DirVertical, Adder) :- Adder = 0,
																write("O fator de multiplicação tem de ser diferente de zero."), !.

mover(rainha, Xini, Yini, DirHorizontal, DirVertical, Adder) :- posicao_atual(rainha, E, Xini, Yini),
																posicao_dentro_tabuleiro(Xini + (DirHorizontal * Adder), Yini + (DirVertical * Adder)),
																turno(E),
																(DirHorizontal <= 1; DirHorizontal >= -1),
																(DirVertical <= 1; DirVertical >= -1),
																retract(posicao_atual(rainha, E, Xini, Yini)),
																assert(posicao_atual(rainha, E, Xini + (DirHorizontal * Adder), Yini + (DirVertical * Adder))).

% Rei
mover(rainha, Xini, Yini, DirHorizontal, DirVertical) :- (DirHorizontal > 1; DirHorizontal < -1),
														 (DirVertical > 1; DirVertical < -1),
														  write("Ambos a direção horizontal e vertical precisam de ter um valor de 1 ou -1."), !.

mover(rainha, Xini, Yini, DirHorizontal, DirVertical) :- posicao_atual(rei, E, Xini, Yini),
														 posicao_dentro_tabuleiro(Xini + DirHorizontal, Yini + DirVertical),
														 turno(E),
														 (DirHorizontal <= 1; DirHorizontal >= -1),
														 (DirVertical <= 1; DirVertical >= -1),
														 retract(posicao_atual(rei, E, Xini, Yini)),
														 assert(posicao_atual(rei, E, Xini + DirHorizontal, Yini + DirVertical)).
														 
% Cavalo
mover(cavalo, Xini, Yini, HorizontalMult, VerticalMult) :- posicao_atual(cavalo1, E, Xini, Yini),
														   posicao_dentro_tabuleiro(Xini + DirHorizontal, Yini + DirVertical),
														   turno(E),
														   (abs(HorizontalMult) =:= 2, abs(VerticalMult); 

% Verificação da posicao dentro do tabuleiro
posicao_dentro_tabuleiro(X, Y) :- X > 0, Y > 0, X <= 8, Y <= 8.

% Verificação da cor da casa
cor_da_casa(X, Y, branca) :- Y mod 2 =:= 1, (X - 1) mod 2 =:= 0.
cor_da_casa(X, Y, preta) :- \+ cor_da_casa(X, Y, branca).

% Code utilities

% Substitui um elemento numa matriz dadas as suas coordenadas X, Y: replace/5
replace( [L|Ls] , 1 , Y , Z , [R|Ls] ) :- % once we find the desired row,
  replace_column(L,Y,Z,R), !.			  % - we replace specified column, and we're done.
replace( [L|Ls] , X , Y , Z , [L|Rs] ) :- % if we haven't found the desired row yet
  X > 1,                                  % - and the row offset is positive,
  X1 is X-1,                              % - we decrement the row offset
  replace( Ls , X1 , Y , Z , Rs ).		  % - and recurse down

replace_column( [_|Cs] , 1 , Z , [Z|Cs] ) .  % once we find the specified offset, just make the substitution and finish up.
replace_column( [C|Cs] , Y , Z , [C|Rs] ) :- % otherwise,
  Y > 1,                                     % - assuming that the column offset is positive,
  Y1 is Y-1,                                 % - we decrement it
  replace_column( Cs , Y1 , Z , Rs ).		 % - and recurse down.
  
% Substitui um elemento no tabuleiro dadas as coordenadas X, Y: replaceOnChessboard/3
replaceOnChessboard(P, X, Y) :- tabuleiro(T), replace(T, X, Y, P, T2), retract(tabuleiro(_)), assert(tabuleiro(T2)).

% Escreve o tabuleiro no ecrã: writeChessboard/0
writeChessboard :- writef(" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ \n"),
				   tabuleiro(T),
				   writeChessboardLines(T, 1).

writeChessboardLines(_, 9) :- !.
writeChessboardLines([ChessHead|ChessTail], Count) :- writef("|     |     |     |     |     |     |     |     |\n"),
													  writef("|"), writeChessElements(ChessHead, 1),
													  writef("\n|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|\n"),
													  Count1 is Count + 1,
													  writeChessboardLines(ChessTail, Count1).

writeChessElements([0|ChessTail]) :- writef("     |").
writeChessElements([1|ChessTail]) :- writef(" R B |").
writeChessElements([2|ChessTail]) :- writef(" RaB |").
writeChessElements([3|ChessTail]) :- writef(" C B |").
writeChessElements([4|ChessTail]) :- writef(" T B |").
writeChessElements([5|ChessTail]) :- writef(" B B |").
writeChessElements([6|ChessTail]) :- writef(" R P |").
writeChessElements([7|ChessTail]) :- writef(" RaP |").
writeChessElements([8|ChessTail]) :- writef(" C P |").
writeChessElements([9|ChessTail]) :- writef(" T P |").
writeChessElements([10|ChessTail]) :- writef(" B P |").
writeChessElements(_, 9) :- !.
writeChessElements([ChessHead|ChessTail], Count) :- writeChessElements([ChessHead|ChessTail]), Count1 is Count + 1, writeChessElements(ChessTail, Count1).