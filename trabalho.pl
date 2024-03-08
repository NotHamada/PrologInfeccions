:- dynamic sintomas_escolhidos_paciente/1.
:- dynamic tipos_infeccao_escolhidos/1.

create_file(Texto, File) :-
    open(File, write, Stream),
    write(Stream, Texto),
    close(Stream).

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
sintoma(40, "Dor de garganta").
sintoma(41, "Dor no peito").
sintoma(42, "Coriza").

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
doenca("Difteria", [4, 40, 30], [9]).
doenca("Gripe", [4, 10, 40, 18], [9]).
doenca("Pneumonia", [4, 18, 30, 41], [9]).
doenca("Sarampo", [4, 10, 18, 42, 21], [9]).
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

% Predicado principal para fazer o diagnóstico
diagnostico(SintomasEscolhidos, []).
diagnostico(SintomasEscolhidos, [InfeccaoEscolhido|T]) :-
    tipo_infeccao(InfeccaoEscolhido, TipoInfeccao),
    write(TipoInfeccao),
    nl,
    findall(Probabilidade-Doenca, (doenca(Doenca, _, Tipos), member(InfeccaoEscolhido, Tipos), probabilidade_doenca(SintomasEscolhidos, Probabilidade, Doenca)), DoencasProbabilidades),
    ordenar_decrescente(DoencasProbabilidades, DoencasProbabilidadesOrdenadas),
    print_doencas_probabilidades(DoencasProbabilidadesOrdenadas),
    nl,
    diagnostico(SintomasEscolhidos, T).

% Predicado para imprimir as doenças possíveis com suas probabilidades
print_doencas_probabilidades([]).
print_doencas_probabilidades([Probabilidade-Doenca|T]) :-
    format('~w: ~2f%~n', [Doenca, Probabilidade]),
    print_doencas_probabilidades(T).

% Predicado para converter uma string de números separados por vírgula em uma lista de números
string_para_lista_numero(String, Lista) :-
    atomic_list_concat(StringList, ',', String),
    maplist(atom_number, StringList, Lista).

probabilidade_doenca(SintomasEscolhidos, Probabilidade, Doenca) :-
    doenca(Doenca, SintomasDaDoenca, _),
    intersection(SintomasDaDoenca, SintomasEscolhidos, SintomasCorrespondentes),
    length(SintomasCorrespondentes, NumCorrespondentes),
    length(SintomasEscolhidos, NumTotal),
    length(SintomasDaDoenca, NumDoenca),
    (NumTotal > 0 -> Probabilidade is ((NumCorrespondentes / NumTotal) + (NumCorrespondentes / NumDoenca)) / 2 * 100; Probabilidade is 0).

imprimir_sintomas([]).
imprimir_sintomas([Id-Descricao|T]) :-
    format('~w: ~w~n', [Id, Descricao]),
    imprimir_sintomas(T).

imprimir_tipos_infeccao :-
    write('O paciente passou por:'), nl,
    forall(tipo_infeccao(Id, Descricao), (
        format('~w: ~w~n', [Id, Descricao])
    )).

sintomas_por_tipo_infeccao(TipoInfeccao, Sintomas) :-
    findall(Id-Descricao, 
            (sintoma(Id, Descricao), 
            doenca(_, SintomasDoenca, Tipos), 
            member(TipoInfeccao, Tipos), 
            member(Id, SintomasDoenca)),
            Sintomas).

sintomas_por_tipos_infeccao([], []).
sintomas_por_tipos_infeccao([Tipo | TiposRestantes], Sintomas) :-
    sintomas_por_tipo_infeccao(Tipo, SintomasTipo),
    sintomas_por_tipos_infeccao(TiposRestantes, SintomasRestantes),
    append(SintomasTipo, SintomasRestantes, Sintomas).

remover_repetidos([], []).
remover_repetidos([H | T], ListaSemRepetidos) :-
    member(H, T),
    !,
    remover_repetidos(T, ListaSemRepetidos).
remover_repetidos([H | T], [H | T1]) :-
    remover_repetidos(T, T1).


atualizar_sintomas_escolhidos(SintomasEscolhidos) :-
    retractall(sintomas_escolhidos_paciente(_)), % Remove qualquer instância anterior
    assert(sintomas_escolhidos_paciente(SintomasEscolhidos)). % Atualiza com os novos sintomas escolhidos


% Inicia o processo de questionamento após o diagnóstico
questionar_sistema :-
    write('Você gostaria de fazer alguma pergunta sobre o diagnóstico?'), nl,
    write('1. Por que o paciente tem essa doença?'), nl,
    write('2. Por que o paciente não tem outra doença?'), nl,
    write('3. O paciente relatou outros sintomas?'), nl,
    write('4. Escolha dos sintomas.'), nl,
    write('5. Sair.'), nl,
    read_string(user_input, "\n", "\r", _, OpcaoString),
    string_to_integer(OpcaoString, Opcao),
    processar_opcao(Opcao).

string_to_integer(String, Integer) :-
    number_string(Integer, String).

processar_opcao(1) :- questionar_doenca_x.
processar_opcao(2) :- questionar_doenca_y.
processar_opcao(3) :- adicionar_sintoma.
processar_opcao(4) :- questionar_escolha_sintoma.
processar_opcao(5) :- write('Saindo do questionamento.'), nl.
processar_opcao(_) :- write('Opção inválida, tente novamente.'), nl, questionar_sistema.

% Questiona sobre a doença X e exibe seus sintomas
questionar_doenca_x :-
    write('Digite o nome da doença para saber mais sobre seus sintomas: '), nl,
    read_line_to_string(user_input, Doenca),
    (   doenca(Doenca, SintomasIds, _) ->
        findall(Sintoma, (member(Id, SintomasIds), sintoma(Id, Sintoma)), Sintomas),
        write('Sintomas de '), write(Doenca), write(':'), nl,
        imprimir_sintomas_lista(Sintomas), nl, questionar_sistema
    ;   write('Doença não encontrada.'), nl, questionar_sistema
    ).

% Imprime a lista de sintomas
imprimir_sintomas_lista([]).
imprimir_sintomas_lista([Sintoma|Resto]) :-
    write('- '), write(Sintoma), nl,
    imprimir_sintomas_lista(Resto).

% Questiona por que o paciente não tem outra doença específica
questionar_doenca_y :-
    write('Digite o nome da doença para saber por que não foi diagnosticada: '), nl,
    read_line_to_string(user_input, DoencaInput),
    string_lower(DoencaInput, DoencaLower), % Converte a entrada para minúsculas
    (   doenca(Doenca, SintomasIdsDaDoenca, _),
        string_lower(Doenca, DoencaNomeLower), % Converte o nome da doença para minúsculas
        DoencaLower = DoencaNomeLower,
        sintomas_escolhidos_paciente(SintomasEscolhidos) ->
        findall(Sintoma, (member(Id, SintomasIdsDaDoenca), sintoma(Id, Sintoma)), SintomasDaDoenca),
        findall(SintomaEscolhido, (member(IdEscolhido, SintomasEscolhidos), sintoma(IdEscolhido, SintomaEscolhido)), SintomasEscolhidosNomes),
        subtract(SintomasDaDoenca, SintomasEscolhidosNomes, SintomasFaltantes),
        (   SintomasFaltantes \= [] ->
            write('O paciente não apresenta os seguintes sintomas necessários para esta doença: '), nl,
            imprimir_sintomas_lista(SintomasFaltantes)
        ;   write('O paciente apresenta todos os sintomas, mas outros fatores podem ter influenciado o diagnóstico.'), nl
        )
    ;   write('Doença não encontrada ou sintomas do paciente não registrados.'), nl
    ),
    nl, questionar_sistema.

listar_sintomas_nao_escolhidos :-
    tipos_infeccao_escolhidos(TiposInfeccaoEscolhidos),
    sintomas_por_tipos_infeccao(TiposInfeccaoEscolhidos, SintomasRelevantes),
    remover_repetidos(SintomasRelevantes, SintomasFiltrados),
    sort(SintomasFiltrados, SintomasOrdenados),
    sintomas_escolhidos_paciente(SintomasEscolhidosIds),
    findall(Id, (member(Id-_, SintomasOrdenados), \+ member(Id, SintomasEscolhidosIds)), IDsSintomasNaoEscolhidos),
    findall(Id-Descricao, (member(Id, IDsSintomasNaoEscolhidos), sintoma(Id, Descricao)), SintomasNaoEscolhidos),
    imprimir_sintomas_nao_escolhidos(SintomasNaoEscolhidos).

imprimir_sintomas_nao_escolhidos([]) :-
    write('Todos os sintomas relevantes já foram escolhidos.'), nl.
imprimir_sintomas_nao_escolhidos([Id-Descricao|T]) :-
    format('~w: ~w~n', [Id, Descricao]),
    imprimir_sintomas_nao_escolhidos(T).


adicionar_sintoma :-
    listar_sintomas_nao_escolhidos,
    write('Digite os números dos sintomas adicionais que deseja adicionar (separados por vírgula), ou deixe em branco para nenhum: '), nl,
    read_string(user_input, "\n", "\r", _, SintomasString),
    (   SintomasString = "" -> write('Nenhum sintoma adicional foi adicionado.'), nl; % Se nenhuma entrada, não faça nada
        string_para_lista_numero(SintomasString, SintomasAdicionaisIds),
        sintomas_escolhidos_paciente(SintomasEscolhidos),
        union(SintomasEscolhidos, SintomasAdicionaisIds, SintomasAtualizados),
        retractall(sintomas_escolhidos_paciente(_)),
        assert(sintomas_escolhidos_paciente(SintomasAtualizados)),
        write('Sintomas atualizados.'), nl,
        tipos_infeccao_escolhidos(TiposInfeccaoEscolhidos),
        diagnostico(SintomasAtualizados, TiposInfeccaoEscolhidos) % Reavalia as doenças com base nos sintomas atualizados
    ),
    questionar_sistema.

questionar_escolha_sintoma :-
    sintomas_escolhidos_paciente(SintomasEscolhidos),
    write('Sintomas escolhidos:'), nl,
    imprimir_sintomas_escolhidos(SintomasEscolhidos),
    write('Qual sintoma você deseja questionar? Digite o número.'), nl,
    read_string(user_input, "\n", "\r", _, NumberString),
    string_to_integer(NumberString, Number),
    (   sintoma(Number, Sintoma) ->
        write('Sintoma escolhido: '), write(Sintoma), nl,
        findall(Doenca, (doenca(Doenca, Sintomas, _), member(Number, Sintomas)), ListaDoencas), nl,
        write('Doenças associadas a esse sintoma: '), nl,
        imprimir_lista_doencas(ListaDoencas)
    ;   write('Número inválido.'), nl
    ), nl,
    questionar_sistema.
    

% Função para imprimir os sintomas escolhidos
imprimir_sintomas_escolhidos([]).
imprimir_sintomas_escolhidos([ID|Rest]) :-
    sintoma(ID, Sintoma),
    format('~w: ~w~n', [ID, Sintoma]),
    imprimir_sintomas_escolhidos(Rest).

% Função para imprimir a lista de doenças
imprimir_lista_doencas([]).
imprimir_lista_doencas([Doenca|Rest]) :-
    write(Doenca), nl,
    imprimir_lista_doencas(Rest).


inverter_lista([], []).
inverter_lista([H|T], ListaInvertida) :-
    inverter_lista(T, RestoInvertido),
    append(RestoInvertido, [H], ListaInvertida).

ordenar_decrescente(Lista, ListaOrdenadaDecrescente) :-
    sort(Lista, ListaOrdenada),
    inverter_lista(ListaOrdenada, ListaOrdenadaDecrescente).

join_strings(Strings, Sep, Resultado) :-
    atomic_list_concat(Strings, Sep, Resultado).

gerar_cabecalho(NomeDoPaciente, IdadeDoPaciente, DataDoAtendimento, File) :-
    create_file("Relatório de previsão de infecções:", File),
    join_strings(["\n\nData de atendimento:", DataDoAtendimento], ' ', Data),
    append_to_file(File, Data),
    join_strings(["\nPaciente:", NomeDoPaciente], ' ', LinhaNomeDoPaciente),
    join_strings(["Idade:", IdadeDoPaciente], ' ', LinhaIdadeDoPaciente),
    append_to_file(File, LinhaNomeDoPaciente),
    append_to_file(File, LinhaIdadeDoPaciente).

gerar_tipos_de_infeccao([], File).
gerar_tipos_de_infeccao([H|T], File) :-
    tipo_infeccao(H, Descricao),
    append_to_file(File, Descricao),
    gerar_tipos_de_infeccao(T, File).

% Predicado inicial
% :- initialization(main).

main :-
    write('Nome do paciente: '),
    read_string(user_input, "\n", "\r", _, NomeDoPaciente),
    write('Idade do paciente: '),
    read_string(user_input, "\n", "\r", _, IdadeDoPaciente),
    write('Data da consulta: '),
    read_string(user_input, "\n", "\r", _, DataDoAtendimento),
    imprimir_tipos_infeccao,
    write('Escolha os modos de infecção do paciente (digite os números separados por vírgula): '),
    read_string(user_input, "\n", "\r", _, InfeccoesString),
    string_para_lista_numero(InfeccoesString, InfeccoesEscolhidos),
    retractall(tipos_infeccao_escolhidos(_)),
    assert(tipos_infeccao_escolhidos(InfeccoesEscolhidos)),
    sintomas_por_tipos_infeccao(InfeccoesEscolhidos, Sintomas),
    remover_repetidos(Sintomas, SintomasFiltrados),
    sort(SintomasFiltrados, SintomasOrdenados),
    nl,
    write('Sintomas:'),
    nl, 
    imprimir_sintomas(SintomasOrdenados),
    nl,
    write('Escolha os sintomas que o paciente possui (digite os números separados por vírgula): '),
    nl,
    read_string(user_input, "\n", "\r", _, SintomasString),
    string_para_lista_numero(SintomasString, SintomasEscolhidos),
    atualizar_sintomas_escolhidos(SintomasEscolhidos),
    write('Possíveis doenças com base nos sintomas escolhidos:'), nl, 
    diagnostico(SintomasEscolhidos, InfeccoesEscolhidos),
    questionar_sistema, 
    write('Fim do diagnóstico e questionamento.'), nl,
    write('Gerando o relatório do paciente...'), nl,
    write('O resultado do protótipo é apenas informativo.'), nl,
    write('Consulte um médico para obter um diagnóstico correto e preciso!'), nl,
    join_strings([NomeDoPaciente, "txt"], '.', File),
    gerar_cabecalho(NomeDoPaciente, IdadeDoPaciente, DataDoAtendimento, File),    
    append_to_file(File, "\nModos de infecção do paciente:"),
    gerar_tipos_de_infeccao(InfeccoesEscolhidos, File),
    % gerar_sintomas(),
    % gerar_doencas_possiveis(),
    halt.