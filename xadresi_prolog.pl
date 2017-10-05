%	Implementação Xadresi em Prolog
%	As peças são referenciadas pelo seu nome em todo o codigo.

% Estado inicial do jogo
turno(brancas).

% Passar turno
passar_turno :- turno(brancas), retract(turno(brancas)), assert(turno(pretas)).
passar_turno :- turno(pretas), retract(turno(pretas)), assert(turno(brancas)).

% Regras de movimentação das peças
mover(torre, Xini, Yini, Xfin, Yfin) :- Xini =:= Xfin,
										posicao_dentro_tabuleiro(Xini, Yini),
										posicao_atual(torre1, E, Xini, Yini),
										posicao_dentro_tabuleiro(Xfin, Yfin),
										turno(E),
										retract(posicao_atual(torre1, E, Xini, Yini)),
										assert(posicao_atual(torre1, E, Xfin, Yfin)).

mover(torre, Xini, Yini, Xfin, Yfin) :- Yini =:= Yfin,
										posicao_dentro_tabuleiro(Xini, Yini),
										posicao_atual(torre1, E, Xini, Yini),
										posicao_dentro_tabuleiro(Xfin, Yfin),
										turno(E),
										retract(posicao_atual(torre1, E, Xini, Yini)),
										assert(posicao_atual(torre1, E, Xfin, Yfin)).
										
mover(torre, Xini, Yini, Xfin, Yfin) :- Xini =:= Xfin,
										posicao_dentro_tabuleiro(Xini, Yini),
										posicao_atual(torre2, E, Xini, Yini),
										posicao_dentro_tabuleiro(Xfin, Yfin),
										turno(E),
										retract(posicao_atual(torre2, E, Xini, Yini)),
										assert(posicao_atual(torre2, E, Xfin, Yfin)).
										
mover(torre, Xini, Yini, Xfin, Yfin) :- Yini =:= Yfin,
										posicao_dentro_tabuleiro(Xini, Yini),
										posicao_atual(torre2, E, Xini, Yini),
										posicao_dentro_tabuleiro(Xfin, Yfin),
										turno(E),
										retract(posicao_atual(torre2, E, Xini, Yini)),
										assert(posicao_atual(torre2, E, Xfin, Yfin)).
										
mover(bispo, Xini, Yini, HorizontalDir, VerticalDir, Adder) :- .

posicao_dentro_tabuleiro(X, Y) :- X > 0, Y > 0, X <= 8, Y <= 8.

% Posicionamento atual das peças (inicialmente não estão posicionados)
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

% 
