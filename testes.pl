:- begin_tests(extended).

:- include('trabalho.pl').

%% Testes para manipulação e atualização de sintomas escolhidos
test(atualizar_sintomas_add, [cleanup(retractall(sintomas_escolhidos_paciente(_)))]) :-
    atualizar_sintomas_escolhidos([4, 5]),
    sintomas_escolhidos_paciente(Sintomas),
    assertion(Sintomas == [4, 5]).

%% Testes para lógica e manipulação de lista
test(remover_repetidos_variados, true(Lista == [1, 2, 3])) :-
    remover_repetidos([1, 1, 2, 3, 3], Lista).

test(sintomas_por_tipos_infeccao_combine, [cleanup(retractall(sintomas_escolhidos_paciente(_)))]) :-
    atualizar_sintomas_escolhidos([1, 2, 3, 4, 5]),
    sintomas_por_tipos_infeccao([1, 2], Sintomas),
    remover_repetidos(Sintomas, SintomasUnicos),
    length(SintomasUnicos, Length),
    assertion(Length > 0).

%% Testes para lógica de diagnóstico (excluindo impressão)
test(diagnostico_simple, [cleanup(retractall(sintomas_escolhidos_paciente(_))), cleanup(retractall(tipos_infeccao_escolhidos(_)))]) :-
    atualizar_sintomas_escolhidos([1, 2]),
    assert(tipos_infeccao_escolhidos([1])),
    diagnostico([1, 2], [1]),
    true.

test(string_para_lista_numero_composto, true(Lista == [10, 20, 30])) :-
    string_para_lista_numero("10,20,30", Lista).

test(probabilidade_doenca_simple, [true(Probabilidade > 0)]) :-
    probabilidade_doenca([1, 2, 3], Probabilidade, "Colera").

%% Testes para ordenação e manipulação de dados
test(ordenar_decrescente_numeros, true(L == [10, 5, 2, 1])) :-
    ordenar_decrescente([2, 1, 5, 10], L).

:- end_tests(extended).

