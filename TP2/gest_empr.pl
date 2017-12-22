% Implementação do problema de gestão empresarial em PLR
:- use_module(library(clpfd)).
:- use_module(library(lists)).

/** %SET 1
% Orçamento disponivel
orcamento_disponivel(30).

% Criterios a respeitar: criterio(ID, Peso). (SOMA DOS PESOS TEM DE SER =1)
criterio(1, 0.1).
criterio(2, 0.4).
criterio(3, 0.5).

% Medidas a tomar: medida(ID, Custo, [AfetaçõesAosCritérios])
medida(1, 20, [4, -2, 1]).
medida(2, 5, [-5, 3, 0]).
medida(3, 7, [0, 0, 5]).
*/


% SET 2
orcamento_disponivel(50).

criterio(1, 0.34).
criterio(2, 0.10).
criterio(3, 0.26).
criterio(4, 0.30).

medida(1, 30, [6, -5, 5, 3]).
medida(2, 10, [-10, 1, 5, 1]).
medida(3, 5, [1, -2, -2, 3]).
medida(4, 6, [5, -2, 0, 0]).
medida(5, 3, [-1, 3, -1, 3]).


% Verificar quantas medidas existem (o maximo id que existir equivale ao numero de medidas)
numero_medidas_existentes(MedidasExistentes) :- findall(X, medida(X, _, _), ListaIds), length(ListaIds, MedidasExistentes).

% Obtenção de uma lista com todos os custos das medidas por ordem.
lista_medidas_custos(CustoMedidas) :- findall(CustoMedida, medida(_, CustoMedida, _), CustoMedidas).

lista_medidas_afetacoes(AfectaMedidas) :- findall(Afetacao, medida(_, _, Afetacao), AfectaMedidas).

lista_prioridades_criterios(PrioridadesCrits) :- findall(Prioridade, criterio(_, Prioridade), Prioridades), obter_percentagens_de_criterios(Prioridades, PrioridadesCrits).

obter_percentagens_de_criterios([], []).
obter_percentagens_de_criterios([Prioridade|T], [PrioridadePercentagem|TP]) :- PrioridadePercentagem is integer(Prioridade * 100), obter_percentagens_de_criterios(T, TP).

gest_empr :-	numero_medidas_existentes(MedidasExistentes),
				length(ListaMedidas, MedidasExistentes),
				domain(ListaMedidas, 0, 1),
				orcamento_disponivel(O),															%OBTENÇÃO DADOS DO PROBLEMA
				lista_medidas_custos(CustoMedidas),
				lista_prioridades_criterios(Prioridades),
				lista_medidas_afetacoes(Afetacoes),
				obter_custo_relativo_das_medidas(Prioridades, Afetacoes, ListaCustosRelativos),
				sum(Prioridades, #=, 100),															%RESTRIÇÕES AOS DADOS DO PROBLEMA
				assegurar_custo_medidas_menor_que_orcamento(ListaMedidas, CustoMedidas, O),
				custo_relativo_medidas(ListaMedidas, ListaCustosRelativos, CustoRelativo),
				labeling([maximize(CustoRelativo)], ListaMedidas),
				escrever_medidas_adotar(ListaMedidas, CustoRelativo).
							
% Assegurar que o custo das medidas escolhidas é menor que o orçamento total
assegurar_custo_medidas_menor_que_orcamento(ListaMedidas, CustoMedidas, Orcamento) :-	scalar_product(CustoMedidas, ListaMedidas, #=, CustoTotal), 
																						Orcamento #>= CustoTotal.
																						
obter_custo_relativo_das_medidas(_, [], []).
obter_custo_relativo_das_medidas(Prioridades, [Afetacao|TailAfetacoes], [CustoRelativo|TailCustos]) :-
																	scalar_product(Prioridades, Afetacao, #=, CustoRelativo),
																	obter_custo_relativo_das_medidas(Prioridades, TailAfetacoes, TailCustos).
																	
custo_relativo_medidas(ListaMedidas, ListaCustosRelativos, CustoRelativo) :- scalar_product(ListaCustosRelativos, ListaMedidas, #=, CustoRelativo).

escrever_medidas_adotar(ListaMedidas, CustoRelativo) :- write('As medidas a adotar sao as seguintes: '), nl, write('|'), escrever_medidas_adotar(ListaMedidas, 1, CustoRelativo).
escrever_medidas_adotar([Medida|Tail], CurrentMedidaID, CustoRelativo) :-	((Medida = 1, write(CurrentMedidaID), write('|')); (Medida = 0)),
																			CurrentMedidaID1 is CurrentMedidaID + 1,
																			escrever_medidas_adotar(Tail, CurrentMedidaID1, CustoRelativo).
escrever_medidas_adotar([], _, CustoRelativo) :- nl, write('Estas medidas tem um custo relativo de: '), CustoRelativoDec is CustoRelativo / 100, write(CustoRelativoDec).