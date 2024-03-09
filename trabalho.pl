:- dynamic sintomas_escolhidos_paciente/1.
:- dynamic tipos_infeccao_escolhidos/1.

:- include('doencas.pl').

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

% Predicado principal para fazer o diagnóstico
diagnostico(_, []).
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
questionar_sistema(File) :-
    write('Você gostaria de fazer alguma pergunta sobre o diagnóstico?'), nl,
    write('1. Por que o paciente tem essa doença?'), nl,
    write('2. Por que o paciente não tem outra doença?'), nl,
    write('3. O paciente relatou outros sintomas?'), nl,
    write('4. Escolha dos sintomas.'), nl,
    write('5. Sair.'), nl,
    read_string(user_input, "\n", "\r", _, OpcaoString),
    string_to_integer(OpcaoString, Opcao),
    processar_opcao(Opcao, File).

string_to_integer(String, Integer) :-
    number_string(Integer, String).

processar_opcao(1, File) :- questionar_doenca_x(File).
processar_opcao(2, File) :- questionar_doenca_y(File).
processar_opcao(3, File) :- adicionar_sintoma(File).
processar_opcao(4, File) :- questionar_escolha_sintoma(File).
processar_opcao(5, _) :- write('Saindo do questionamento.'), nl.
processar_opcao(_, File) :- write('Opção inválida, tente novamente.'), nl, questionar_sistema(File).

% Questiona sobre a doença X e exibe seus sintomas
questionar_doenca_x(File) :-
    write('Digite o nome da doença para saber mais sobre seus sintomas: '), nl,
    read_line_to_string(user_input, Doenca),
    (   doenca(Doenca, SintomasIds, _) ->
        findall(Sintoma, (member(Id, SintomasIds), sintoma(Id, Sintoma)), Sintomas),
        write('Sintomas de '), write(Doenca), write(':'), nl,
        imprimir_sintomas_lista(Sintomas), nl, questionar_sistema(File)
    ;   write('Doença não encontrada.'), nl, questionar_sistema(File)
    ).

% Imprime a lista de sintomas
imprimir_sintomas_lista([]).
imprimir_sintomas_lista([Sintoma|Resto]) :-
    write('- '), write(Sintoma), nl,
    imprimir_sintomas_lista(Resto).

% Questiona por que o paciente não tem outra doença específica
questionar_doenca_y(File) :-
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
    nl, questionar_sistema(File).

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


adicionar_sintoma(File) :-
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
        diagnostico(SintomasAtualizados, TiposInfeccaoEscolhidos),
        append_to_file(File, "Sintomas adicionados:"),
        gerar_sintomas(SintomasAtualizados, File),
        append_to_file(File, "\nNovas porcentagens:"),
        gerar_doencas_possiveis(SintomasAtualizados, File, TiposInfeccaoEscolhidos) % Reavalia as doenças com base nos sintomas atualizados
    ),
    questionar_sistema(File).

questionar_escolha_sintoma(File) :-
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
    questionar_sistema(File).
    

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

gerar_tipos_de_infeccao([], _).
gerar_tipos_de_infeccao([H|T], File) :-
    tipo_infeccao(H, Descricao),
    append_to_file(File, Descricao),
    gerar_tipos_de_infeccao(T, File).

gerar_sintomas([], _).
gerar_sintomas([H|T], File) :-
    sintoma(H, Descricao),
    append_to_file(File, Descricao),
    gerar_sintomas(T, File).

gerar_doencas_possiveis(_, _, []).
gerar_doencas_possiveis(SintomasEscolhidos, File, [H|T]) :-
    tipo_infeccao(H, TipoInfeccao),
    append_to_file(File, TipoInfeccao),
    nl,
    findall(Probabilidade-Doenca, (doenca(Doenca, _, Tipos), member(H, Tipos), probabilidade_doenca(SintomasEscolhidos, Probabilidade, Doenca)), DoencasProbabilidades),
    ordenar_decrescente(DoencasProbabilidades, DoencasProbabilidadesOrdenadas),
    gerar_linhas_da_doenca(DoencasProbabilidadesOrdenadas, File),
    nl,
    append_to_file(File, "\n"),
    gerar_doencas_possiveis(SintomasEscolhidos, File, T).

gerar_linhas_da_doenca([], _).
gerar_linhas_da_doenca([Probabilidade-Doenca|T], File) :-
    join_strings([Doenca, ":", Probabilidade], '', LinhaDaDoenca),
    append_to_file(File, LinhaDaDoenca),
    gerar_linhas_da_doenca(T, File).

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
    
    join_strings([NomeDoPaciente, "txt"], '.', Consulta),
    join_strings(["Consultas", Consulta], '/', File),
    gerar_cabecalho(NomeDoPaciente, IdadeDoPaciente, DataDoAtendimento, File),    
    append_to_file(File, "\nModos de infecção do paciente:"),
    gerar_tipos_de_infeccao(InfeccoesEscolhidos, File),
    append_to_file(File, "\nSintomas do paciente:"),
    gerar_sintomas(SintomasEscolhidos, File),
    append_to_file(File, "\nDoenças possíveis:"),
    gerar_doencas_possiveis(SintomasEscolhidos, File, InfeccoesEscolhidos),
    
    questionar_sistema(File), 
    write('Fim do diagnóstico e questionamento.'), nl,
    write('Gerando o relatório do paciente...'), nl,
    write('O resultado do protótipo é apenas informativo.'), nl,
    write('Consulte um médico para obter um diagnóstico correto e preciso!'), nl,
    
    halt.