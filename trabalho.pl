%% sintomas_escolhidos_paciente(?SintomasEscolhidos) is semidet.
%  Armazena ou consulta os sintomas escolhidos pelo usuário durante a execução do programa.
%  Este predicado é dinâmico, permitindo que os sintomas escolhidos sejam atualizados.
:- dynamic sintomas_escolhidos_paciente/1.

%% tipos_infeccao_escolhidos(?TiposInfeccaoEscolhidos) is semidet.
%  Armazena ou consulta os tipos de infecção escolhidos pelo usuário durante a execução do programa.
%  Este predicado é dinâmico, permitindo que os tipos de infecção escolhidos sejam atualizados.
:- dynamic tipos_infeccao_escolhidos/1.

:- include('doencas.pl').
:- include('leitura.pl').

%% diagnostico(+SintomasEscolhidos, +InfeccoesEscolhidas) is det.
%  Executa o diagnóstico com base nos sintomas e tipos de infecção escolhidos.
%  Imprime os resultados do diagnóstico, incluindo a probabilidade de doenças correspondentes.
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

%% print_doencas_probabilidades(+DoencasProbabilidades) is det.
%  Imprime a lista de doenças com suas respectivas probabilidades.print_doencas_probabilidades([]).
print_doencas_probabilidades([Probabilidade-Doenca|T]) :-
    format('~w: ~2f%~n', [Doenca, Probabilidade]),
    print_doencas_probabilidades(T).

%% string_para_lista_numero(+String, -Lista) is det.
%  Converte uma string contendo números separados por vírgula em uma lista de números.string_para_lista_numero(String, Lista) :-
    atomic_list_concat(StringList, ',', String),
    maplist(atom_number, StringList, Lista).

%% probabilidade_doenca(+SintomasEscolhidos, -Probabilidade, +Doenca) is semidet.
%  Calcula a probabilidade de uma doença com base nos sintomas escolhidos pelo usuário.
probabilidade_doenca(SintomasEscolhidos, Probabilidade, Doenca) :-
    doenca(Doenca, SintomasDaDoenca, _),
    intersection(SintomasDaDoenca, SintomasEscolhidos, SintomasCorrespondentes),
    length(SintomasCorrespondentes, NumCorrespondentes),
    length(SintomasEscolhidos, NumTotal),
    length(SintomasDaDoenca, NumDoenca),
    (NumTotal > 0 -> Probabilidade is ((NumCorrespondentes / NumTotal) + (NumCorrespondentes / NumDoenca)) / 2 * 100; Probabilidade is 0).

%% imprimir_sintomas(+ListaSintomas) is det.
%  Imprime a lista de sintomas fornecida.
imprimir_sintomas([]).
imprimir_sintomas([Id-Descricao|T]) :-
    format('~w: ~w~n', [Id, Descricao]),
    imprimir_sintomas(T).

%% imprimir_tipos_infeccao is det.
%  Imprime todos os tipos de infecção disponíveis no sistema.
imprimir_tipos_infeccao :-
    write('O paciente passou por:'), nl,
    forall(tipo_infeccao(Id, Descricao), (
        format('~w: ~w~n', [Id, Descricao])
    )).

%% sintomas_por_tipo_infeccao(+TipoInfeccao, -Sintomas) is nondet.
%  Retorna os sintomas associados a um tipo específico de infecção.
sintomas_por_tipo_infeccao(TipoInfeccao, Sintomas) :-
    findall(Id-Descricao, 
            (sintoma(Id, Descricao), 
            doenca(_, SintomasDoenca, Tipos), 
            member(TipoInfeccao, Tipos), 
            member(Id, SintomasDoenca)),
            Sintomas).

%% sintomas_por_tipos_infeccao(+TiposInfeccao, -Sintomas) is nondet.
%  Agrega os sintomas de múltiplos tipos de infecção em uma única lista.
sintomas_por_tipos_infeccao([], []).
sintomas_por_tipos_infeccao([Tipo | TiposRestantes], Sintomas) :-
    sintomas_por_tipo_infeccao(Tipo, SintomasTipo),
    sintomas_por_tipos_infeccao(TiposRestantes, SintomasRestantes),
    append(SintomasTipo, SintomasRestantes, Sintomas).

%% remover_repetidos(+Lista, -ListaSemRepetidos) is det.
%  Remove elementos duplicados de uma lista.
remover_repetidos([], []).
remover_repetidos([H | T], ListaSemRepetidos) :-
    member(H, T),
    !,
    remover_repetidos(T, ListaSemRepetidos).
remover_repetidos([H | T], [H | T1]) :-
    remover_repetidos(T, T1).

%% atualizar_sintomas_escolhidos(+SintomasEscolhidos) is det.
%  Atualiza os sintomas escolhidos armazenados dinamicamente.
atualizar_sintomas_escolhidos(SintomasEscolhidos) :-
    retractall(sintomas_escolhidos_paciente(_)), % Remove qualquer instância anterior
    assert(sintomas_escolhidos_paciente(SintomasEscolhidos)). % Atualiza com os novos sintomas escolhidos


%% questionar_sistema(+File) is det.
%  Inicia o processo de questionamento, permitindo ao usuário fazer perguntas pós-diagnóstico.
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

%% string_to_integer(+String, -Integer) is det.
%  Converte uma string para um número inteiro.
string_to_integer(String, Integer) :-
    number_string(Integer, String).

%% processar_opcao(+Opcao, +File) is det.
%  Processa a opção selecionada pelo usuário no menu de questionamento.
processar_opcao(1, File) :- questionar_doenca_x(File).
processar_opcao(2, File) :- questionar_doenca_y(File).
processar_opcao(3, File) :- adicionar_sintoma(File).
processar_opcao(4, File) :- questionar_escolha_sintoma(File).
processar_opcao(5, _) :- write('Saindo do questionamento.'), nl.
processar_opcao(_, File) :- write('Opção inválida, tente novamente.'), nl, questionar_sistema(File).

%% questionar_doenca_x(+File) is det.
%  Permite ao usuário questionar e receber informações sobre os sintomas de uma doença específica.
questionar_doenca_x(File) :-
    write('Digite o nome da doença para saber mais sobre seus sintomas: '), nl,
    read_line_to_string(user_input, Doenca),
    (   doenca(Doenca, SintomasIds, _) ->
        findall(Sintoma, (member(Id, SintomasIds), sintoma(Id, Sintoma)), Sintomas),
        write('Sintomas de '), write(Doenca), write(':'), nl,
        imprimir_sintomas_lista(Sintomas), nl, questionar_sistema(File)
    ;   write('Doença não encontrada.'), nl, questionar_sistema(File)
    ).

%% imprimir_sintomas_lista(+Sintomas) is det.
%  Imprime uma lista de sintomas.
imprimir_sintomas_lista([]).
imprimir_sintomas_lista([Sintoma|Resto]) :-
    write('- '), write(Sintoma), nl,
    imprimir_sintomas_lista(Resto).

%% questionar_doenca_y(+File) is det.
%  Permite ao usuário questionar por que uma determinada doença não foi diagnosticada.
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
            write('O paciente não apresenta os seguintes sintomas desta doença: '), nl,
            imprimir_sintomas_lista(SintomasFaltantes)
        ;   write('O paciente apresenta todos os sintomas, mas outros fatores podem ter influenciado o diagnóstico.'), nl
        )
    ;   write('Doença não encontrada ou sintomas do paciente não registrados.'), nl
    ),
    nl, questionar_sistema(File).

%% listar_sintomas_nao_escolhidos is det.
%  Lista os sintomas não escolhidos associados aos tipos de infecção selecionados.
listar_sintomas_nao_escolhidos :-
    tipos_infeccao_escolhidos(TiposInfeccaoEscolhidos),
    sintomas_por_tipos_infeccao(TiposInfeccaoEscolhidos, SintomasRelevantes),
    remover_repetidos(SintomasRelevantes, SintomasFiltrados),
    sort(SintomasFiltrados, SintomasOrdenados),
    sintomas_escolhidos_paciente(SintomasEscolhidosIds),
    findall(Id, (member(Id-_, SintomasOrdenados), \+ member(Id, SintomasEscolhidosIds)), IDsSintomasNaoEscolhidos),
    findall(Id-Descricao, (member(Id, IDsSintomasNaoEscolhidos), sintoma(Id, Descricao)), SintomasNaoEscolhidos),
    imprimir_sintomas_nao_escolhidos(SintomasNaoEscolhidos).

%% imprimir_sintomas_nao_escolhidos(+SintomasNaoEscolhidos) is det.
%  Imprime os sintomas que não foram escolhidos pelo usuário.
imprimir_sintomas_nao_escolhidos([]) :-
    write('Todos os sintomas relevantes já foram escolhidos.'), nl.
imprimir_sintomas_nao_escolhidos([Id-Descricao|T]) :-
    format('~w: ~w~n', [Id, Descricao]),
    imprimir_sintomas_nao_escolhidos(T).

%% adicionar_sintoma(+File) is det.
%  Permite ao usuário adicionar sintomas adicionais após o diagnóstico inicial.
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

%% questionar_escolha_sintoma(+File) is det.
%  Permite ao usuário questionar sobre a escolha dos sintomas realizados.
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
    
%% imprimir_sintomas_escolhidos(+SintomasEscolhidos) is det.
%  Imprime os sintomas que foram escolhidos pelo usuário.
imprimir_sintomas_escolhidos([]).
imprimir_sintomas_escolhidos([ID|Rest]) :-
    sintoma(ID, Sintoma),
    format('~w: ~w~n', [ID, Sintoma]),
    imprimir_sintomas_escolhidos(Rest).

%% imprimir_lista_doencas(+ListaDoencas) is det.
%  Imprime a lista de doenças fornecida.
imprimir_lista_doencas([]).
imprimir_lista_doencas([Doenca|Rest]) :-
    write(Doenca), nl,
    imprimir_lista_doencas(Rest).

%% inverter_lista(+Lista, -ListaInvertida) is det.
%  Inverte a ordem dos elementos em uma lista.
inverter_lista([], []).
inverter_lista([H|T], ListaInvertida) :-
    inverter_lista(T, RestoInvertido),
    append(RestoInvertido, [H], ListaInvertida).

%% ordenar_decrescente(+Lista, -ListaOrdenadaDecrescente) is det.
%  Ordena uma lista de pares probabilidades-doenças em ordem decrescente de probabilidade.
ordenar_decrescente(Lista, ListaOrdenadaDecrescente) :-
    sort(Lista, ListaOrdenada),
    inverter_lista(ListaOrdenada, ListaOrdenadaDecrescente).

%% join_strings(+Strings, +Sep, -Resultado) is det.
%  Junta uma lista de strings usando um separador e retorna o resultado como uma única string.
join_strings(Strings, Sep, Resultado) :-
    atomic_list_concat(Strings, Sep, Resultado).

%% gerar_cabecalho(+NomeDoPaciente, +IdadeDoPaciente, +DataDoAtendimento, +File) is det.
%  Gera o cabeçalho do relatório de diagnóstico.
gerar_cabecalho(NomeDoPaciente, IdadeDoPaciente, DataDoAtendimento, File) :-
    create_file("Relatório de previsão de infecções:", File),
    join_strings(["\n\nData de atendimento:", DataDoAtendimento], ' ', Data),
    append_to_file(File, Data),
    join_strings(["\nPaciente:", NomeDoPaciente], ' ', LinhaNomeDoPaciente),
    join_strings(["Idade:", IdadeDoPaciente], ' ', LinhaIdadeDoPaciente),
    append_to_file(File, LinhaNomeDoPaciente),
    append_to_file(File, LinhaIdadeDoPaciente).

%% gerar_tipos_de_infeccao(+TiposDeInfeccao, +File) is det.
%  Gera a seção do relatório que lista os tipos de infecção.
gerar_tipos_de_infeccao([], _).
gerar_tipos_de_infeccao([H|T], File) :-
    tipo_infeccao(H, Descricao),
    append_to_file(File, Descricao),
    gerar_tipos_de_infeccao(T, File).

%% gerar_sintomas(+Sintomas, +File) is det.
%  Gera a seção do relatório que lista os sintomas do paciente.
gerar_sintomas([], _).
gerar_sintomas([H|T], File) :-
    sintoma(H, Descricao),
    append_to_file(File, Descricao),
    gerar_sintomas(T, File).

%% gerar_doencas_possiveis(+SintomasEscolhidos, +File, +TiposDeInfeccao) is det.
%  Gera a seção do relatório que lista as possíveis doenças com base nos sintomas escolhidos.
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

%% gerar_linhas_da_doenca(+DoencasProbabilidades, +File) is det.
%  Gera as linhas individuais para cada doença e sua probabilidade no relatório.
gerar_linhas_da_doenca([], _).
gerar_linhas_da_doenca([Probabilidade-Doenca|T], File) :-
    join_strings([Doenca, ":", Probabilidade], '', LinhaDaDoenca),
    append_to_file(File, LinhaDaDoenca),
    gerar_linhas_da_doenca(T, File).

%% inicializacao is det.
%  Mostra o menu inicial e processa a seleção do usuário.
inicializacao :-
    write('Escolha uma opção:'), nl,
    write('1. Adicionar paciente'), nl,
    write('2. Remover paciente'), nl,
    write('3. Ver lista de pacientes'), nl,
    write('4. Ver consulta do paciente'), nl,
    write('5. Sair.'), nl,
    read_string(user_input, "\n", "\r", _, OpcaoString),
    string_to_integer(OpcaoString, Opcao),
    processar_inicializacao(Opcao).

%% processar_inicializacao(+Opcao) is det.
%  Direciona o usuário para a funcionalidade correspondente à opção escolhida no menu inicial.
processar_inicializacao(1) :- main.
processar_inicializacao(2) :- remover_paciente.
processar_inicializacao(3) :- listar_pacientes.
processar_inicializacao(4) :- consultar_paciente.
processar_inicializacao(5) :- write('Saindo do programa.'), nl.
processar_inicializacao(_) :- write('Opção inválida, tente novamente.'), nl, inicializacao.

%% main is det.
%  O ponto de entrada principal do programa. Gerencia o fluxo de interação com o usuário e geração de relatórios de diagnóstico.
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
    nl,
    inicializacao.