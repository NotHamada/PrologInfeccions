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
sintoma(1, "Diarreia").
sintoma(2, "Vomito").
sintoma(3, "Desidratacao").
sintoma(4, "Febre").
sintoma(5, "Dor abdominal").
sintoma(6, "Demencia").
sintoma(7, "Perda de coordenacao").
sintoma(8, "Mudancas de comportamento").
sintoma(9, "Perda de apetite").
sintoma(10, "Dor de cabeca").
sintoma(11, "Fadiga").
sintoma(12, "Nausea").
sintoma(13, "Ictericia").
sintoma(14, "Dor muscular").
sintoma(15, "Rigidez na nuca").
sintoma(16, "Fraqueza muscular").
sintoma(17, "Inchaco dos linfonodos").
sintoma(18, "Tosse").
sintoma(19, "Sangue na urina").
sintoma(20, "Hidrofobia").
sintoma(21, "Rash cutaneo").
sintoma(22, "Rigidez muscular").
sintoma(23, "Espasmos musculares").
sintoma(24, "Dificuldade para engolir").
sintoma(25, "Confusao").
sintoma(26, "Hemorragia").
sintoma(27, "Calafrios").
sintoma(28, "Suor").
sintoma(29, "Inchaco das parotidas").
sintoma(30, "Dificuldade para respirar").
sintoma(31, "Perda de peso").
sintoma(32, "Suores noturnos").
sintoma(33, "Ferida genital dolorosa").
sintoma(34, "Corrimento vaginal").
sintoma(35, "Ferida genital indolor").
sintoma(36, "Feridas na pele").
sintoma(37, "Verrugas genitais").
sintoma(38, "Ferida genital").
sintoma(39, "Dor ao urinar").

tipo_infeccao(1, "Contato com agua contaminada").
tipo_infeccao(2, "Contato com solo contaminado").
tipo_infeccao(3, "Contato com alimento contaminado").
tipo_infeccao(4, "Mordida de animal infectado").
tipo_infeccao(5, "Contato com fezes de gato infectado").
tipo_infeccao(6, "Contato com urina de rato infectado").
tipo_infeccao(7, "Contato com ferida contaminada").
tipo_infeccao(8, "Picada de mosquito infectado").
tipo_infeccao(9, "Contato gotículas respiratórias").
tipo_infeccao(10, "Contato com saliva contaminada").
tipo_infeccao(11, "Contato sexual").

doenca("Colera", [1, 2, 3], [1, 2]).
doenca("Doenca dos viajantes", [1, 2, 4, 5], [1, 2]).
doenca("Vaca louca", [6, 7, 8], [3]).
doenca("Febre tifoide", [4, 10, 5  ], [1, 2]).
doenca("Hepatite A", [11, 12, 2, 13], [1, 3]).
doenca("Hepatite E", [4, 11, 12, 2, 5], [1, 3]).
doenca("Leptospirose", [4, 10, 14, 13], [1, 2]).
doenca("Poliomelite", [4, 10, 15, 16], [1, 2]).
doenca("Toxoplasmose", [4, 10, 17, 11], [3]).
doenca("Esquitossomose", [4, 5, 18, 19], [1]).
doenca("Amebas", [1, 2, 5, 4], [1]).

doenca("Raiva", [4, 10, 25, 20], [4]).
doenca("Doenca do gato", [4, 10, 17, 11], [5]).
doenca("Doenca do rato", [4, 10, 14, 21], [6]).
doenca("Tetano", [22, 23, 24], [7]).

doenca("Doenca de chagas", [4, 10, 17, 11], [1, 8]).
doenca("Dengue", [4, 10, 14, 21], [8]).
doenca("Encefalite", [4, 10, 15, 25], [8]).
doenca("Febre amarela", [4, 10, 14, 13, 26], [8]).
doenca("Malaria", [4, 10, 27, 28], [8]).

doenca("Caxumba", [4, 10, 29], [10]).
doenca("Difteria", [4, dor_de_garganta, 30], [9]).
doenca("Gripe", [4, 10, dor_de_garganta, 18], [9]).
doenca("Pneumonia", [4, 18, 30, dor_no_peito], [9]).
doenca("Sarampo", [4, 10, 18, coriza, 21], [9]).
doenca("Sars", [4, 10, 18, 30], [9]).
doenca("Rubeola", [4, 10, 17, 21], [9]).
doenca("Tuberculose", [18, 31, 11, 4], [9]).
doenca("Varicela", [4, 10, 11, 21], [9]).

doenca("Aids", [11, 31, 4, 32], [11]).
doenca("Cancro mole", [33], [11]).
doenca("Corrimento", [34], [11]).
doenca("Donovanose", [35], [11]).
doenca("Hepatite B", [11, 12, 2, 13], [11]).
doenca("Herpes", [36], [11]).
doenca("Linfogranuloma", [17], [11]).
doenca("Papilomavirus", [37], [11]).
doenca("Sifilis", [38, 21], [11]).
doenca("Uretrite", [39], [11]).

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