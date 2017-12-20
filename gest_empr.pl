% Implementação do problema de gestão empresarial em PLR
:- use_module(library(clpfd)).
:- use_module(library(lists)).

% Orçamento disponivel
orcamento_disponivel(30).

% Criterios a respeitar: criterio(ID, Peso). (SOMA DOS PESOS TEM DE SER =1)
criterio(1, 3).
criterio(2, -4).
criterio(3, 2).

% Medidas a tomar: medida(ID, Custo, CriteriosPos, CriteriosNeg)
medida(1, 20, [1, 3], [2]).
medida(2, 5, [2], [1]).
medida(3, 7, [3], []).

% Verificar quantas medidas existem (o maximo id que existir equivale ao numero de medidas)
numero_medidas_existentes(MedidasExistentes) :- findall(X, medida(X, _, _, _), ListaIds), max_list(ListaIds, MedidasExistentes).

gest_empr(Orcamento, NumMedidas, ListaMedidas) :- 	CurrentMedidas in 1..NumMedidas, length(ListaMedidas, CurrentMedidas),
													all_distinct(ListaMedidas),
													ListaMedidas ins 1..NumMedidas,
													medidas_ordem_crescente(ListaMedidas),
													verificar_custo_menor_que_orcamento(ListaMedidas, Orcamento),
													verificar_custo_relativo_total_medidas(ListaMedidas, CustoRel),
													labeling([min(CustoRel), enum], ListaMedidas),
													write(CustoRel), nl, write(ListaMedidas).

medidas_ordem_crescente([_]).
medidas_ordem_crescente([A, B|T]) :- B #> A, medidas_ordem_crescente([B|T]).

verificar_custo_menor_que_orcamento(ListaMedidas, Orcamento) :- verificar_custo_menor_que_orcamento(ListaMedidas, 0, Orcamento).

verificar_custo_menor_que_orcamento([], _, _).
verificar_custo_menor_que_orcamento([MedidaID|Tail], Sum, Orcamento) :- medida(MedidaID, Custo, _, _), Sum1 #= Sum + Custo, Orcamento #>= Sum1, verificar_custo_menor_que_orcamento(Tail, Sum1, Orcamento).

verificar_custo_relativo_total_medidas([], 0).
verificar_custo_relativo_total_medidas([MedidaID|Tail], CustoRel) :- obter_custo_relativo_medida(MedidaID, CustoRelMedida), verificar_custo_relativo_total_medidas(Tail, CustoRel1), CustoRel1 #= CustoRel + CustoRelMedida.

obter_custo_relativo_medida(MedidaID, CustoRelativoMedida) :-	medida(MedidaID, _, CriteriosPos, CriteriosNeg),
																calcular_peso_criterios(CriteriosPos, PesoPos),
																calcular_peso_criterios(CriteriosNeg, PesoNeg),
																CustoRelativoMedida #= PesoPos-PesoNeg.
																
calcular_peso_criterios([], 0).
calcular_peso_criterios([Criterio|Tail], PesoRelativo) :- criterio(Criterio, PesoCriterio), calcular_peso_criterios(Tail, PesoRel), PesoRelativo #= PesoRel + PesoCriterio.

