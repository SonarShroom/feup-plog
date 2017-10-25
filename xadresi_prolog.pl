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

% Pontuação
:- dynamic pontos/2.
pontos(brancas, 0).
pontos(pretas, 0).

% Jogadas possíveis
% Quando jogadas_possiveis(todas) isto significa todas exceto o rei, visto que os reis têm de ser ambos a primeira e a ultima peça a ser posicionados.
% Estados possíveis: rei, todas, rainha
:- dynamic jogadas_possiveis/1.
jogadas_possiveis(rei).
% Lista de posições de jogadas possíveis (quando vazia, é porque estamos no 1º turno logo todas são possíveis.)
:- dynamic pos_jogadas_possiveis/1.
pos_jogadas_possiveis(todas).

% Construção de lista das posições das jogadas possíveis:
build_list_possible_plays(T, E) :- retract(pos_jogadas_possiveis(_)), assert(pos_jogadas_possiveis([])), build_list_possible_plays_line(T, 1, E).

build_list_possible_plays_line([], _, _) :- !.
build_list_possible_plays_line([THead|TTail], Y, E) :- build_list_possible_plays_col(THead, 1, Y, E), Y1 is Y + 1, build_list_possible_plays_line(TTail, Y1, E).

build_list_possible_plays_col([], _, _, _) :- !.
build_list_possible_plays_col([0|T], X, Y, E) :- X1 is X + 1, build_list_possible_plays_col(T, X1, Y, E), !.
%TODO: construir lista das casas vizinhas (ou refazer predicados de ataque para retornar lista de casas vizinhas), e inserir casos em que exista casa que nao satisfaça condição.
build_list_possible_plays_col([H|T], X, Y, brancas) :- H > 5, retract(pos_jogadas_possiveis(L)), append(L, [[Y - 1, X - 1], [Y - 1, X], [Y - 1, X + 1], [Y, X - 1], [Y, X + 1], [Y + 1, X - 1], [Y + 1, X], [Y + 1, X + 1]], L2), assert(pos_jogadas_possiveis(L2)), X1 is X + 1, build_list_possible_plays_col(T, X1, Y, brancas).
build_list_possible_plays_col([H|T], X, Y, pretas) :- H < 6, retract(pos_jogadas_possiveis(L)), append(L, [[Y - 1, X - 1], [Y - 1, X], [Y - 1, X + 1], [Y, X - 1], [Y, X + 1], [Y + 1, X - 1], [Y + 1, X], [Y + 1, X + 1]], L2), assert(pos_jogadas_possiveis(L2)), X1 is X + 1, build_list_possible_plays_col(T, X1, Y, pretas).

% Modos de jogo
% Modo de jogo atual
:- dynamic modo_de_jogo_atual/1.
modo_de_jogo_atual(humano_vs_humano).

% Mudar modo de jogo
% mudar_modo_de_jogo(X) :- jogo_iniciado(t), write("O jogo já foi iniciado e não é permitido mudar o modo."), !.
mudar_modo_de_jogo(humano_vs_humano) :- retract(modo_de_jogo_atual(_)), assert(modo_de_jogo_atual(humano_vs_humano)), !.
mudar_modo_de_jogo(humano_vs_computador) :- retract(modo_de_jogo_atual(_)), assert(modo_de_jogo_atual(humano_vs_computador)), !.
mudar_modo_de_jogo(computador_vs_computador) :- retract(modo_de_jogo_atual(_)), assert(modo_de_jogo_atual(computador_vs_computador)), !.
mudar_modo_de_jogo(_) :- write("Este modo de jogo não existe/não é suportado: \nOs seguintes modos de jogo estão implementados: \n\n humano_vs_humano \n\n humano_vs_computador \n\n computador_vs_computador").


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

% Casa do primeiro bispo
:- dynamic casa_primeiro_bispo/2.
casa_primeiro_bispo(brancas, nao_existe).
casa_primeiro_bispo(pretas, nao_existe).

% Posicionamento das peças
% Movimentos não permitidos
posicionar_peca(_, X, Y, T, T, _) :- ((X < 1; X > 8; Y < 1; Y > 8), !, write("Posicao fora do tabuleiro. Refaca a jogada."), nl; nth1(Y, T, Line), nth1(X, Line, P), P \= 0, write("Casa nao esta vazia. Refaca a jogada."), nl; \+ pos_jogadas_possiveis(todas), pos_jogadas_possiveis(L), \+ member([X, Y], L), write("Essa jogada nao e possivel. Refaca a jogada."), nl), !.
posicionar_peca(P, _, _, T, T, E) :- jogadas_possiveis(P2), (quantidade_de_pecas(P, E, X), X = 0, write("Nao existem mais pecas deste tipo. Refaca a jogada."), nl; P \= P2, P2 = rei, write("So pode posicionar o rei nesta jogada. Refaca a jogada."), nl; P \= P2, P2 = rainha, write("So pode posicionar a rainha nesta jogada. Refaca a jogada."), nl; P2 = todas, P = rei, write("Os reis so podem ser posicionados na primeira e ultima jogadas. Refaca a jogada."), nl), !.
posicionar_peca(bispo, X, Y, T, T, E) :- casa_primeiro_bispo(E, Cor), cor_da_casa(X, Y, Cor), write("Não se pode colocar bispos em casas da mesma cor. Refaca a jogada.\n"), !.

% Brancas
posicionar_peca(rei, X, Y, T, T2, brancas) :- jogadas_possiveis(rei), retract(jogadas_possiveis(_)), assert(jogadas_possiveis(todas)), retract(quantidade_de_pecas(rei, brancas, 1)), assert(quantidade_de_pecas(rei, brancas, 0)), replace(T, X, Y, 1, T2), !.
posicionar_peca(rainha, X, Y, T, T2, brancas) :- (jogadas_possiveis(rainha), retract(jogadas_possiveis(_)), assert(jogadas_possiveis(todas)); jogadas_possiveis(todas), retract(jogadas_possiveis(_)), assert(jogadas_possiveis(rainha))), retract(quantidade_de_pecas(rainha, brancas, 1)), assert(quantidade_de_pecas(rainha, brancas, 0)), replace(T, X, Y, 2, T2), !.
posicionar_peca(cavalo, X, Y, T, T2, brancas) :- jogadas_possiveis(todas), retract(quantidade_de_pecas(cavalo, brancas, Q)), assert(quantidade_de_pecas(cavalo, brancas, Q - 1)), replace(T, X, Y, 3, T2), !.
posicionar_peca(torre, X, Y, T, T2, brancas) :- jogadas_possiveis(todas), retract(quantidade_de_pecas(torre, brancas, Q)), assert(quantidade_de_pecas(torre, brancas, Q - 1)), replace(T, X, Y, 4, T2), !.
posicionar_peca(bispo, X, Y, T, T2, brancas) :- jogadas_possiveis(todas), casa_primeiro_bispo(brancas, nao_existe), cor_da_casa(X, Y, Cor), retract(casa_primeiro_bispo(brancas, _)), assert(casa_primeiro_bispo(brancas, Cor)), retract(quantidade_de_pecas(bispo, brancas, Q)), assert(quantidade_de_pecas(bispo, brancas, Q - 1)), replace(T, X, Y, 5, T2), !.
posicionar_peca(bispo, X, Y, T, T2, brancas) :- jogadas_possiveis(todas), casa_primeiro_bispo(brancas, _), cor_da_casa(X, Y, _), retract(quantidade_de_pecas(bispo, brancas, Q)), assert(quantidade_de_pecas(bispo, brancas, Q - 1)), replace(T, X, Y, 5, T2), !.

% Pretas
posicionar_peca(rei, X, Y, T, T2, pretas) :- jogadas_possiveis(rei), retract(jogadas_possiveis(_)), assert(jogadas_possiveis(todas)), retract(quantidade_de_pecas(rei, pretas, 1)), assert(quantidade_de_pecas(rei, pretas, 0)), replace(T, X, Y, 6, T2), !.
posicionar_peca(rainha, X, Y, T, T2, pretas) :- (jogadas_possiveis(rainha), retract(jogadas_possiveis(_)), assert(jogadas_possiveis(todas)); jogadas_possiveis(todas), retract(jogadas_possiveis(_)), assert(jogadas_possiveis(rainha))), retract(quantidade_de_pecas(rainha, pretas, 1)), assert(quantidade_de_pecas(rainha, pretas, 0)), replace(T, X, Y, 7, T2), !.
posicionar_peca(cavalo, X, Y, T, T2, pretas) :- jogadas_possiveis(todas), retract(quantidade_de_pecas(cavalo, pretas, Q)), assert(quantidade_de_pecas(cavalo, pretas, Q - 1)), replace(T, X, Y, 8, T2), !.
posicionar_peca(torre, X, Y, T, T2, pretas) :- jogadas_possiveis(todas), retract(quantidade_de_pecas(torre, pretas, Q)), assert(quantidade_de_pecas(torre, pretas, Q - 1)), replace(T, X, Y, 9, T2), !.
posicionar_peca(bispo, X, Y, T, T2, pretas) :- jogadas_possiveis(todas), casa_primeiro_bispo(pretas, nao_existe), cor_da_casa(X, Y, Cor), retract(casa_primeiro_bispo(pretas, _)), assert(casa_primeiro_bispo(pretas, Cor)), retract(quantidade_de_pecas(bispo, pretas, Q)), assert(quantidade_de_pecas(bispo, pretas, Q - 1)), replace(T, X, Y, 10, T2), !.
posicionar_peca(bispo, X, Y, T, T2, pretas) :- jogadas_possiveis(todas), casa_primeiro_bispo(pretas, _), cor_da_casa(X, Y, _), retract(quantidade_de_pecas(bispo, pretas, Q)), assert(quantidade_de_pecas(bispo, pretas, Q - 1)), replace(T, X, Y, 10, T2), !.
														   
% Ciclo de jogo
xadrersi :- tabuleiro(T), xadrersi(T, brancas).

xadrersi(Tabuleiro, E) :- modo_de_jogo_atual(humano_vs_humano), !,
						  writeChessboard(Tabuleiro),
						  write("Peca: "), read(P), nl,
						  write("Coordenadas: \nX: "), read(X), nl,
						  write("Y: "), read(Y),
						  posicionar_peca(P, X, Y, Tabuleiro, T2, E),
						  (Tabuleiro \= T2, game_end(T2); Tabuleiro \= T2, E = brancas, build_list_possible_plays(T2, pretas), xadrersi(T2, pretas); Tabuleiro \= T2, build_list_possible_plays(T2, brancas), xadrersi(T2, brancas); xadrersi(Tabuleiro, E)).
							
% Verficação de término de jogo
game_end(Tabuleiro) :- quantidade_de_pecas(rei, brancas, 0),
					   quantidade_de_pecas(rainha, brancas, 0),
					   quantidade_de_pecas(cavalo, brancas, 0),
					   quantidade_de_pecas(torre, brancas, 0),
					   quantidade_de_pecas(bispo, brancas, 0),
					   quantidade_de_pecas(rei, pretas, 0),
					   quantidade_de_pecas(rainha, pretas, 0),
					   quantidade_de_pecas(cavalo, pretas, 0),
					   quantidade_de_pecas(torre, pretas, 0),
					   quantidade_de_pecas(bispo, pretas, 0),
					   executar_ataques(Tabuleiro),
					   writeChessboard(Tabuleiro),
					   pontos(pretas, PP), pontos(brancas, PB),
					   (PP > PB, write("As pretas venceram o jogo!"), nl; PB > PP, write("As brancas venceram o jogo!"), nl; PP = PB, write("O jogo terminou num empate!"), nl), 
					   make.

% Rotina de cálculo de ataque
executar_ataques(T) :- verificar_ataques_linha(T, 1, T).
verificar_ataques_linha([], _, _) :- write("Verificacao do tabuleiro concluida.\n"), !.
verificar_ataques_linha([H|T], Y, Tabuleiro) :- verificar_ataques_coluna(H, 1, Y, Tabuleiro), Y1 is Y + 1, verificar_ataques_linha(T, Y1, Tabuleiro).
verificar_ataques_coluna([], _, _, _) :- write("Verificacao de linha concluida.\n"), !.
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
atk_all_directions(_, _, _, 0, _) :- write("Finished atacking all directions."), !.
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
atk_cross_positions(_, _, _, 0, _) :- write("Finished atacking all cross directions."), !.
atk_cross_positions(P, X, Y, Adder, T) :- PrevCol is X - Adder, NextCol is X + Adder, PrevLine is Y - Adder, NextLine is Y + Adder,
										  atk_list_positions(P, [[PrevLine, X], [NextLine, X], [Y, PrevCol], [Y, NextCol]], T),
										  Adder1 is Adder - 1,
										  atk_cross_positions(P, X, Y, Adder1, T).
										  
% Utilidade para obter todas as posições a atacar num padrão equivalente ao bispo (mesmo as fora do tabuleiro), no formato [Y, X], num raio equivalente a Adder
% Representação das peças num raio de 1 (equivalente ao bispo):
%	1	-	2
%	-	B	-
%	3	-	4
atk_cross_bishop_positions(_, _, _, 0, _) :- write("Finished atacking all cross directions."), !.
atk_cross_bishop_positions(P, X, Y, Adder, T) :- PrevCol is X - Adder, NextCol is X + Adder, PrevLine is Y - Adder, NextLine is Y + Adder,
												 atk_list_positions(P, [[PrevLine, PrevCol], [PrevLine, NextCol], [NextLine, PrevCol], [NextLine, NextCol]], T),
												 Adder1 is Adder - 1,
												 atk_cross_positions(P, X, Y, Adder1, T).

% Ataca uma lista de posições no formato [Y, X].
atk_list_positions(_, [], _) :- !.
atk_list_positions(P, [Pos|Positions], T) :- atk_at_position(P, Pos, T), atk_list_positions(P, Positions, T).

% Dada uma lista [Y, X], obtem a linha Y do tabuleiro e ataca uma pos X dessa linha.
atk_at_position(_, [Y|_], _) :- (Y < 1; Y > 8), write("Y invalido.\n"), !.
atk_at_position(P, [Y|X], T) :- Y > 0, Y < 9, !, nth1(Y, T, Line), atk_at_line_pos(P, X, Line).

% Dada uma linha do tabuleiro, e uma pos X, verifica o ataque entre a peça P e a peça defensora PD.
atk_at_line_pos(_, [X|_], _) :- (X < 1; X > 8), write("X invalido."), !.
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

% Escreve o tabuleiro no ecrã: writeChessboard/1
writeChessboard(Tabuleiro) :- write(" _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ "), nl,
							  writeChessboardLines(Tabuleiro).

writeChessboardLines([]) :- !.
writeChessboardLines([ChessHead|ChessTail]) :- write("|     |     |     |     |     |     |     |     |"), nl,
											   write("|"), writeChessElements(ChessHead), nl,
											   write("|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|_ _ _|"), nl,
											   writeChessboardLines(ChessTail).

writeChessElements([]) :- !.
writeChessElements([0|ChessTail]) :- write("     |"), writeChessElements(ChessTail).
writeChessElements([1|ChessTail]) :- write(" R B |"), writeChessElements(ChessTail).
writeChessElements([2|ChessTail]) :- write(" RaB |"), writeChessElements(ChessTail).
writeChessElements([3|ChessTail]) :- write(" C B |"), writeChessElements(ChessTail).
writeChessElements([4|ChessTail]) :- write(" T B |"), writeChessElements(ChessTail).
writeChessElements([5|ChessTail]) :- write(" B B |"), writeChessElements(ChessTail).
writeChessElements([6|ChessTail]) :- write(" R P |"), writeChessElements(ChessTail).
writeChessElements([7|ChessTail]) :- write(" RaP |"), writeChessElements(ChessTail).
writeChessElements([8|ChessTail]) :- write(" C P |"), writeChessElements(ChessTail).
writeChessElements([9|ChessTail]) :- write(" T P |"), writeChessElements(ChessTail).
writeChessElements([10|ChessTail]) :- write(" B P |"), writeChessElements(ChessTail).