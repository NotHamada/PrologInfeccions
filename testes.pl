:- begin_tests(trabalho).
:- include('trabalho.pl').

% Testes para remover_repetidos/2
test(remover_repetidos_normal, true(L == [1,2,3])) :-
    remover_repetidos([1,2,2,3,3], L).

test(remover_repetidos_vazio, true(L == [])) :-
    remover_repetidos([], L).

test(remover_repetidos_unico, true(L == [1])) :-
    remover_repetidos([1,1,1], L).

% Testes para ordenar_decrescente/2
test(ordenar_decrescente_normal, true(L == [3-abc, 2-def, 1-ghi])) :-
    ordenar_decrescente([1-ghi, 2-def, 3-abc], L).

test(ordenar_decrescente_vazio, true(L == [])) :-
    ordenar_decrescente([], L).

test(ordenar_decrescente_unico, true(L == [1-abc])) :-
    ordenar_decrescente([1-abc], L).

% Testes para probabilidade_doenca/3 
:- dynamic doenca/3.
test(probabilidade_doenca_base, setup(assertz(doenca(doenca_teste, [sintoma1, sintoma2], _)))) :-
    probabilidade_doenca([sintoma1], Prob, doenca_teste),
    Prob > 0.

test(probabilidade_doenca_nenhum_sintoma, [nondet]) :-
    probabilidade_doenca([], Prob, doenca_teste),
    Prob == 0.

test(probabilidade_doenca_completa, [nondet]) :-
    probabilidade_doenca([sintoma1, sintoma2], Prob, doenca_teste),
    Prob > 0.

:- end_tests(trabalho).
