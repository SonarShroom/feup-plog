% Implementação do problema de gestão empresarial em PLR
:- use_module(library(clpfd)).
:- use_module(library(lists)).

% Orçamento disponivel
orcamento_disponivel(30).

% Criterios a respeitar: criterio(ID, Peso). (SOMA DOS PESOS TEM DE SER =1)
criterio(1, 4).
criterio(2, -5).
criterio(3, 2).

% Medidas a tomar: medida(ID, Custo, CriteriosPos, MultCriteriosPositivos, CriteriosNeg, MultCriteriosNegativos)
medida(1, 20, [1, 3], [4, 1], [2], [2]).
medida(2, 5, [2], [3], [1], [5]).
medida(3, 7, [3], [5], [], []).

% Verificar quantas medidas existem (o maximo id que existir equivale ao numero de medidas)
numero_medidas_existentes(MedidasExistentes) :- findall(X, medida(X, _, _, _, _, _), ListaIds), max_member(MedidasExistentes, ListaIds).

gest_empr(NumMedidas) :-	NumMedidas =< 0, write('O numero de medidas nao pode ser menor ou igual a 0!').

gest_empr(NumMedidas) :-	length(ListaMedidas, NumMedidas),
							numero_medidas_existentes(MedidasExistentes),
							all_distinct(ListaMedidas),
							domain(ListaMedidas, 1, MedidasExistentes),
							orcamento_disponivel(O),
							%medidas_ordem_crescente(ListaMedidas),
							verificar_custo_menor_que_orcamento(ListaMedidas, O),
							verificar_custo_relativo_total_medidas(ListaMedidas, CustoRel),
							labeling([maximize(CustoRel)], ListaMedidas),
							write(CustoRel), nl, write(ListaMedidas).

% Provavelmente pode ser descartado: assegura que as medidas sao mostradas por ordem crescente.
medidas_ordem_crescente([_]).
medidas_ordem_crescente([A, B|T]) :- B #> A, medidas_ordem_crescente([B|T]).

verificar_custo_menor_que_orcamento(ListaMedidas, Orcamento) :- verificar_custo_menor_que_orcamento(ListaMedidas, 0, Orcamento).

% Verifica se o conjunto de medidas escolhidas não excedem o orçamento disponivel.
verificar_custo_menor_que_orcamento([], _, _).
verificar_custo_menor_que_orcamento([MedidaID|Tail], Sum, Orcamento) :- medida(MedidaID, Custo, _, _, _, _), Sum1 #= Sum + Custo, Orcamento #>= Sum1, verificar_custo_menor_que_orcamento(Tail, Sum1, Orcamento).

verificar_custo_relativo_total_medidas([], 0).
verificar_custo_relativo_total_medidas([MedidaID|Tail], CustoRel) :- obter_custo_relativo_medida(MedidaID, CustoRelMedida), verificar_custo_relativo_total_medidas(Tail, CustoRel1), CustoRel is CustoRel1 + CustoRelMedida.

obter_custo_relativo_medida(MedidaID, CustoRelativoMedida) :-	medida(MedidaID, _, CriteriosPos, PesoCritsPos, CriteriosNeg, PesoCritsNeg),
																calcular_peso_criterios(CriteriosPos, PesoCritsPos, PesoPos),
																calcular_peso_criterios(CriteriosNeg, PesoCritsNeg, PesoNeg),
																CustoRelativoMedida is PesoPos - PesoNeg.
																
calcular_peso_criterios([], _, 0).
calcular_peso_criterios([Criterio|Tail], [CritPesoMult|TailPeso], PesoRelativo) :- criterio(Criterio, PesoCriterio), calcular_peso_criterios(Tail, TailPeso, PesoRel), PesoRelativo is PesoRel + PesoCriterio * CritPesoMult.