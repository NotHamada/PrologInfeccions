read_file(File, Content) :-
    open(File, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).

read_lines(Stream, [Line|Rest]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Rest).

append_to_file(File, Line) :-
    open(File, append, Stream),
    write(Stream, Line),
    write(Stream, '\n'),
    close(Stream).

% Definição de sintomas e suas doenças correspondentes
sintoma(1, febre).
sintoma(2, tosse).
sintoma(3, dor_de_cabeca).
sintoma(4, fadiga).

doenca(gripe, [1, 2, 4]).
doenca(resfriado, [2, 3]).
doenca(enxaqueca, [3, 4]).

% Predicado para verificar se uma doença é possível com base nos sintomas escolhidos
possivel_doenca(SintomasEscolhidos, Doenca) :-
    doenca(Doenca, SintomasDaDoenca),
    subset(SintomasEscolhidos, SintomasDaDoenca).

% Verifica se todos os elementos de Lista1 estão contidos em Lista2
subset([], _).
subset([H|T], List) :-
    member(H, List),
    subset(T, List).

% Predicado principal para fazer o diagnóstico
diagnostico(SintomasEscolhidos) :-
    write('Possíveis doenças com base nos sintomas escolhidos:'), nl,
    findall(Doenca-Probabilidade, (doenca(Doenca, _), probabilidade_doenca(SintomasEscolhidos, Doenca, Probabilidade)), DoencasProbabilidades),
    print_doencas_probabilidades(DoencasProbabilidades).

% Predicado para imprimir as doenças possíveis com suas probabilidades
print_doencas_probabilidades([]).
print_doencas_probabilidades([Doenca-Probabilidade|T]) :-
    format('~w: ~2f%~n', [Doenca, Probabilidade]),
    print_doencas_probabilidades(T).

% Predicado para converter uma string de números separados por vírgula em uma lista de números
string_para_lista_numero(String, Lista) :-
    atomic_list_concat(StringList, ',', String),
    maplist(atom_number, StringList, Lista).

probabilidade_doenca(SintomasEscolhidos, Doenca, Probabilidade) :-
    doenca(Doenca, SintomasDaDoenca),
    intersection(SintomasDaDoenca, SintomasEscolhidos, SintomasCorrespondentes),
    length(SintomasCorrespondentes, NumCorrespondentes),
    length(SintomasEscolhidos, NumTotal),
    (NumTotal > 0 -> Probabilidade is NumCorrespondentes / NumTotal * 100; Probabilidade is 0).

% Predicado inicial
:- initialization(main).

main :-
    write('Lista de sintomas disponíveis:'), nl,
    write('1. Febre'), nl,
    write('2. Tosse'), nl,
    write('3. Dor de cabeça'), nl,
    write('4. Fadiga'), nl,
    write('Escolha os sintomas que possui (digite os números separados por vírgula): '),
    read_string(user_input, "\n", "\r", _, SintomasString),
    string_para_lista_numero(SintomasString, SintomasEscolhidos), 
    diagnostico(SintomasEscolhidos),
    write('Fim do diagnóstico.'), nl,
    halt.