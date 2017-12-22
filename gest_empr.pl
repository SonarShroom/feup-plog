% Implementação do problema de gestão empresarial em PLR
:- use_module(library(clpfd)).
:- use_module(library(lists)).

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

% Verificar quantas medidas existem (o maximo id que existir equivale ao numero de medidas)
numero_medidas_existentes(MedidasExistentes) :- findall(X, medida(X, _, _), ListaIds), max_member(MedidasExistentes, ListaIds).

% Obtenção de uma lista com todos os custos das medidas por ordem.
lista_medidas_custos(CustoMedidas) :- findall(CustoMedida, medida(_, CustoMedida, _), CustoMedidas).

lista_medidas_afetacoes(AfectaMedidas) :- findall(Afetacao, medida(_, _, Afetacao), AfectaMedidas).

lista_prioridades_criterios(PrioridadesCrits) :- findall(Prioridade, criterio(_, Prioridade), Prioridades), obter_percentagens_de_criterios(Prioridades, PrioridadesCrits).

obter_percentagens_de_criterios([], []).
obter_percentagens_de_criterios([Prioridade|T], [PrioridadePercentagem|TP]) :- PrioridadePercentagem is integer(Prioridade * 100), obter_percentagens_de_criterios(T, TP).

gest_empr(ListaMedidas) :-	numero_medidas_existentes(MedidasExistentes),
							length(ListaMedidas, MedidasExistentes),
							domain(ListaMedidas, 0, 1),
							orcamento_disponivel(O),
							lista_medidas_custos(CustoMedidas),
							lista_prioridades_criterios(Prioridades),
							lista_medidas_afetacoes(Afetacoes),
							obter_custo_relativo_das_medidas(Prioridades, Afetacoes, ListaCustosRelativos),
							assegurar_custo_medidas_menor_que_orcamento(ListaMedidas, CustoMedidas, O),
							custo_relativo_medidas(ListaMedidas, ListaCustosRelativos, CustoRelativo),
							labeling([maximize(CustoRelativo)], ListaMedidas),
							write(ListaMedidas).
							
% Assegurar que o custo das medidas escolhidas é menor que o orçamento total
assegurar_custo_medidas_menor_que_orcamento(ListaMedidas, CustoMedidas, Orcamento) :-	scalar_product(CustoMedidas, ListaMedidas, #=, CustoTotal), 
																						Orcamento #>= CustoTotal.
																						
obter_custo_relativo_das_medidas(_, [], []).
obter_custo_relativo_das_medidas(Prioridades, [Afetacao|TailAfetacoes], [CustoRelativo|TailCustos]) :-
																	scalar_product(Prioridades, Afetacao, #=, CustoRelativo),
																	obter_custo_relativo_das_medidas(Prioridades, TailAfetacoes, TailCustos).
																	
custo_relativo_medidas(ListaMedidas, ListaCustosRelativos, CustoRelativo) :- scalar_product(ListaCustosRelativos, ListaMedidas, #=, CustoRelativo).