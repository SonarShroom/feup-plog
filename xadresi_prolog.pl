%	Implementação Xadresi em Prolog
%	As peças são referenciadas em todo o codigo da seguinte maneira:
%		0 - Casa vazia
%		1 - Rei Branco;
%		2 - Rainha Branca;
%		3 - Cavalo Branco;
%		4 - Torre Branca;
%		5 - Bispo Branco;
%		6 - Rei Preto;
%		7 - Rainha Preta;
%		8 - Cavalo Preto;
%		9 - Torre Preta;
%		10 - Bispo Preto;
%	Todas as declarações ":- dynamic xxx/Y" indicam predicados que são retracted, ou asserted ao longo da execução.

% Modulos externos
:- use_module(library(lists)).

% Estado inicial do jogo
% Turno
:- dynamic turno/1.
turno(brancas).

% Tabuleiro (inicialmente encontra-se vazio)
:- dynamic tabuleiro/1.
tabuleiro([0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0,
		   0, 0, 0, 0, 0, 0, 0, 0]).
		   
% Passagem de turno
passar_turno :- turno(brancas), retract(turno(brancas)), assert(turno(pretas)).
passar_turno :- turno(pretas), retract(turno(pretas)), assert(turno(brancas)).

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


% Posicionamento atual das peças (inicialmente não estão posicionados)
:- dynamic posicao_atual/4.

posicao_atual(torre1, branca, -1, -1).
posicao_atual(torre2, branca, -1, -1).
posicao_atual(bispo1, branca, -1, -1).
posicao_atual(bispo2, branca, -1, -1).
posicao_atual(cavalo1, branca, -1, -1).
posicao_atual(cavalo2, branca, -1, -1).
posicao_atual(rei, branca, -1, -1).
posicao_atual(rainha, branca, -1, -1).

posicao_atual(torre1, preta, -1, -1).
posicao_atual(torre2, preta, -1, -1).
posicao_atual(bispo1, preta, -1, -1).
posicao_atual(bispo2, preta, -1, -1).
posicao_atual(cavalo1, preta, -1, -1).
posicao_atual(cavalo2, preta, -1, -1).
posicao_atual(rei, preta, -1, -1).
posicao_atual(rainha, preta, -1, -1).

% Code utilities

% Substitui um elemento no tabuleiro dadas as suas coordenadas X, Y: replaceOnChessboard/3
replaceOnChessboard(X, Y, R) :- Index is X + (Y - 1) * 8, tabuleiro(Tabuleiro), write(Index), replaceOnChessboard(Tabuleiro, Index, R, NovoTabuleiro).
replaceOnChessboard([_|T], 0, R, [R|T]) :- retract(tabuleiro(_)), assert(tabuleiro([R|T])).
replaceOnChessboard([H|T], Index, R, [H|T2]) :- write(Index), Index > 0, NIndex is Index - 1, replaceOnChessboard(T, NIndex, R, T2).

% Escreve o tabuleiro no ecrã: writeChessboard/0

writeChessboard :- writef(" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ \n"),
				   tabuleiro(T),
				   writeChessboardLines(T).

writeChessboardLines([]) :- !.				   
writeChessboardLines(Chessboard) :- writef("|     |     |     |     |     |     |     |     |\n"),
								writef("|"), writeChessElements(Chessboard, 1), writef("\n"),
								writef("|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|\n").

writeChessElements([], Count) :- !.
writeChessElements([0|_]) :- writef("     |").
writeChessElements([_|_], 9) :- !.
writeChessElements([ChessHead]ChessTail], Count) :- write("Entry"), writeChessElements([ChessHead|ChessTail]), Count1 is Count + 1, writeChessElements(ChessTail, Count1).