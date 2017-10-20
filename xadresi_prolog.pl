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

% Pontuação
:- dynamic pontos/2.
pontos(brancas, 0).
pontos(pretas, 0).

% Modos de jogo
% Modo de jogo atual
:- dynamic modo_de_jogo_atual/1.
modo_de_jogo_atual(humano_vs_humano).

% Mudar modo de jogo
% mudar_modo_de_jogo(X) :- jogo_iniciado(t), writef("O jogo já foi iniciado e não é permitido mudar o modo."), !.
mudar_modo_de_jogo(humano_vs_humano) :- retract(modo_de_jogo_atual(_)), assert(modo_de_jogo_atual(humano_vs_humano)), !.
mudar_modo_de_jogo(humano_vs_computador) :- retract(modo_de_jogo_atual(_)), assert(modo_de_jogo_atual(humano_vs_computador)), !.
mudar_modo_de_jogo(computador_vs_computador) :- retract(modo_de_jogo_atual(_)), assert(modo_de_jogo_atual(computador_vs_computador)), !.
mudar_modo_de_jogo(_) :- writef("Este modo de jogo não existe/não é suportado: \nOs seguintes modos de jogo estão implementados: \n\n humano_vs_humano \n\n humano_vs_computador \n\n computador_vs_computador").


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
posicionar_peca(P, _, _) :- turno(E), quantidade_de_pecas(P, E, X), X =:= 0, write("Não existem mais peças deste tipo."), !.
posicionar_peca(rei, X, Y) :- turno(E), replaceOnChessboard(1, X, Y), retract(quantidade_de_pecas(rei, E, 1)), assert(quantidade_de_pecas(rei, E, 0)).
posicionar_peca(rainha, X, Y) :- turno(E), replaceOnChessboard(2, X, Y), retract(quantidade_de_pecas(rainha, E, 1)), assert(quantidade_de_pecas(rainha, E, 0)).
posicionar_peca(cavalo, X, Y) :- turno(E), replaceOnChessboard(3, X, Y), retract(quantidade_de_pecas(cavalo, E, Q)), assert(quantidade_de_pecas(rainha, E, Q - 1)).
posicionar_peca(torre, X, Y) :- turno(E), replaceOnChessboard(4, X, Y), retract(quantidade_de_pecas(torre, E, Q)), assert(quantidade_de_pecas(rainha, E, Q - 1)).
posicionar_peca(bispo, X, Y) :- turno(E), replaceOnChessboard(5, X, Y), retract(quantidade_de_pecas(bispo, E, Q)), assert(quantidade_de_pecas(rainha, E, Q - 1)).
														   
% Ciclo de jogo
ciclo_de_jogo(Tabuleiro) :- modo_de_jogo_atual(humano_vs_humano), !,
							retract(tabuleiro(_)), assert(tabuleiro(Tabuleiro)),
							passar_turno.
							
% Rotina de cálculo de ataque
executar_ataques :- tabuleiro(T), verificar_ataques_linha(T, 1, T).
verificar_ataques_linha([], _, _) :- writef("Verificacao do tabuleiro concluida.\n"), !.
verificar_ataques_linha([H|T], Y, Tabuleiro) :- verificar_ataques_coluna(H, 1, Y, Tabuleiro), Y1 is Y + 1, verificar_ataques_linha(T, Y1, Tabuleiro).
verificar_ataques_coluna([], _, _, _) :- writef("Verificacao de linha concluida.\n"), !.
verificar_ataques_coluna([H|T], X, Y, Tabuleiro) :- ataque(H, X, Y, Tabuleiro), X1 is X + 1, verificar_ataques_coluna(T, X1, Y, Tabuleiro).

verificar_peca_ataca(_, 0) :- !.
verificar_peca_ataca(PAtacante, PDefensora) :- ((PAtacante > 5, PDefensora > 5); (PAtacante < 6, PDefensora < 6)), !.
verificar_peca_ataca(PAtacante, PDefensora) :- PAtacante > 5, PDefensora < 6, retract(pontos(pretas, P)), P1 is P + 1, assert(pontos(pretas, P1)).
verificar_peca_ataca(PAtacante, PDefensora) :- PAtacante < 6, PDefensora > 5, retract(pontos(brancas, P)), P1 is P + 1, assert(pontos(brancas, P1)).

% Regras de ataque

% Verificação de casa vazia
ataque(0, _, _, _) :- !.

% Rei
ataque(1, X, Y, T) :- atk_all_directions(1, X, Y, 1, T).
ataque(6, X, Y, T) :- atk_all_directions(6, X, Y, 1, T).

% Rainha
ataque(2, X, Y, T) :- atk_all_directions(2, X, Y, 7, T).
ataque(7, X, Y, T) :- atk_all_directions(7, X, Y, 7, T).

% Cavalo
ataque(3, X, Y, T) :- atk_horse_positions(3, X, Y, T).
ataque(8, X, Y, T) :- atk_horse_positions(8, X, Y, T).

% Torre
ataque(4, X, Y, T) :- atk_cross_positions(4, X, Y, 7, T).
ataque(9, X, Y, T) :- atk_cross_positions(9, X, Y, 7, T).

% Bispo
ataque(5, X, Y, T) :- atk_cross_bishop_positions(5, X, Y, 7, T).
ataque(10, X, Y, T) :- atk_cross_bishop_positions(10, X, Y, 7, T).

% Utilidade para obter todas as posições a atacar num padrão equivalente à rainha (mesmo as fora do tabuleiro), no formato [Y, X], num raio equivalente a Adder
% Representação das peças num raio de 1 (equivalente ao rei):
%	1	2	3
%	4	R	5
%	6	7	8
atk_all_directions(_, _, _, 0, _) :- writef("Finished atacking all directions."), !.
atk_all_directions(P, X, Y, Adder, T) :- PrevCol is X - Adder, NextCol is X + Adder, PrevLine is Y - Adder, NextLine is Y + Adder,
										 atk_list_positions(P, [[PrevLine, PrevCol], [PrevLine, X], [PrevLine, NextCol], [Y, PrevCol], [Y, NextCol], [NextLine, PrevCol], [NextLine, X], [NextLine, NextCol]], T),
										 Adder1 is Adder - 1,
										 atk_all_directions(P, X, Y, Adder1, T).
										 
atk_horse_positions(P, X, Y, T) :- PrevLine1 is Y - 1, NextLine1 is Y + 1, PrevLine2 is Y - 2, NextLine2 is Y + 2, PrevCol1 is X - 1, NextCol1 is X + 1, PrevCol2 is X - 2, NextCol2 is X + 2, 
								   atk_list_positions(P, [[PrevLine2, PrevCol1], [PrevLine2, NextCol1], [NextLine2, PrevCol1], [NextLine2, NextCol1], [PrevLine1, PrevCol2], [PrevLine1, NextCol2], [NextLine1, PrevCol2], [NextLine1, NextCol2]], T).

% Utilidade para obter todas as posições a atacar num padrão equivalente à torre (mesmo as fora do tabuleiro), no formato [Y, X], num raio equivalente a Adder
% Representação das peças num raio de 1 (equivalente à torre):
%	-	1	-
%	2	T	3
%	-	4	-
atk_cross_positions(_, _, _, 0, _) :- writef("Finished atacking all cross directions."), !.
atk_cross_positions(P, X, Y, Adder, T) :- PrevCol is X - Adder, NextCol is X + Adder, PrevLine is Y - Adder, NextLine is Y + Adder,
										  atk_list_positions(P, [[PrevLine, X], [NextLine, X], [Y, PrevCol], [Y, NextCol]], T),
										  Adder1 is Adder - 1,
										  atk_cross_positions(P, X, Y, Adder1, T).
										  
% Utilidade para obter todas as posições a atacar num padrão equivalente ao bispo (mesmo as fora do tabuleiro), no formato [Y, X], num raio equivalente a Adder
% Representação das peças num raio de 1 (equivalente ao bispo):
%	1	-	2
%	-	B	-
%	3	-	4
atk_cross_bishop_positions(_, _, _, 0, _) :- writef("Finished atacking all cross directions."), !.
atk_cross_bishop_positions(P, X, Y, Adder, T) :- PrevCol is X - Adder, NextCol is X + Adder, PrevLine is Y - Adder, NextLine is Y + Adder,
										  atk_list_positions(P, [[PrevLine, PrevCol], [PrevLine, NextCol], [NextLine, PrevCol], [NextLine, NextCol]], T),
										  Adder1 is Adder - 1,
										  atk_cross_positions(P, X, Y, Adder1, T).

% Ataca uma lista de posições no formato [Y, X].
atk_list_positions(_, [], _) :- !.
atk_list_positions(P, [Pos|Positions], T) :- atk_at_position(P, Pos, T), atk_list_positions(P, Positions, T).

% Dada uma lista [Y, X], obtem a linha Y do tabuleiro e ataca uma pos X dessa linha.
atk_at_position(_, [Y|_], _) :- (Y < 1; Y > 8), writef("Y invalido.\n"), !.
atk_at_position(P, [Y|X], T) :- Y > 0, Y < 9, !, nth1(Y, T, Line), atk_at_line_pos(P, X, Line).

% Dada uma linha do tabuleiro, e uma pos X, verifica o ataque entre a peça P e a peça defensora PD.
atk_at_line_pos(_, [X|_], _) :- (X < 1; X > 8), writef("X invalido."), !.
atk_at_line_pos(P, [X|_], Line) :- X > 0, X < 9, !, nth1(X, Line, PD), verificar_peca_ataca(P, PD).


% Verificação da cor da casa
cor_da_casa(X, Y, branca) :- Y mod 2 =:= 1, (X - 1) mod 2 =:= 0, !.
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
replaceOnChessboard(P, X, Y) :- tabuleiro(T), replace(T, X, Y, P, T2), ciclo_de_jogo(T2).

% Escreve o tabuleiro no ecrã: writeChessboard/0
writeChessboard :- writef(" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ \n"),
				   tabuleiro(T),
				   writeChessboardLines(T).

writeChessboardLines([]) :- !.
writeChessboardLines([ChessHead|ChessTail]) :- writef("|     |     |     |     |     |     |     |     |\n"),
													  writef("|"), writeChessElements(ChessHead),
													  writef("\n|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|\n"),
													  writeChessboardLines(ChessTail).

writeChessElements([]) :- !.
writeChessElements([0|ChessTail]) :- writef("     |"), writeChessElements(ChessTail).
writeChessElements([1|ChessTail]) :- writef(" R B |"), writeChessElements(ChessTail).
writeChessElements([2|ChessTail]) :- writef(" RaB |"), writeChessElements(ChessTail).
writeChessElements([3|ChessTail]) :- writef(" C B |"), writeChessElements(ChessTail).
writeChessElements([4|ChessTail]) :- writef(" T B |"), writeChessElements(ChessTail).
writeChessElements([5|ChessTail]) :- writef(" B B |"), writeChessElements(ChessTail).
writeChessElements([6|ChessTail]) :- writef(" R P |"), writeChessElements(ChessTail).
writeChessElements([7|ChessTail]) :- writef(" RaP |"), writeChessElements(ChessTail).
writeChessElements([8|ChessTail]) :- writef(" C P |"), writeChessElements(ChessTail).
writeChessElements([9|ChessTail]) :- writef(" T P |"), writeChessElements(ChessTail).
writeChessElements([10|ChessTail]) :- writef(" B P |"), writeChessElements(ChessTail).