find_txt_files(Directory) :-
    directory_files(Directory, Files),
    findall(File, (member(File, Files), file_name_extension(_, 'txt', File)), TxtFiles),
    print_files(TxtFiles).

read_files([]).
read_files([H|T]) :-
    write(H),
    nl,
    read_file(H, _),
    read_files(T).

read_file(File, Content) :-
    open(File, read, Stream),
    read_lines(Stream, Content),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).

read_lines(Stream, [Line|Rest]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    format('~w~n', [Line]),
    read_lines(Stream, Rest).

create_file(Texto, File) :-
    open(File, write, Stream),
    write(Stream, Texto),
    close(Stream).
    
append_to_file(File, Line) :-
    open(File, append, Stream),
    write(Stream, Line),
    write(Stream, '\n'),
    close(Stream).

delete_file_from_path(FilePath) :-
    delete_file(FilePath).

print_files([]).
print_files([H|T]) :-
    file_name_extension(Name, 'txt', H),
    write(Name), nl,
    print_files(T).

remover_paciente :-
    write("Selecione o paciente a ser removido:"), nl,
    find_txt_files("./Consultas"), nl,
    read_string(user_input, "\n", "\r", _, NomeDoPaciente),
    join_strings([NomeDoPaciente, ".txt"], '', File),
    join_strings(["./Consultas", File], '/', Path),
    delete_file_from_path(Path),
    write("Paciente removido!"), nl,
    inicializacao.

listar_pacientes :- 
    write("Listagem de todos os pacientes:"), nl,
    find_txt_files("./Consultas"), nl,
    inicializacao.

consultar_paciente :-
    write("Selecione o paciente a ser consultado:"), nl,
    find_txt_files("./Consultas"),
    read_string(user_input, "\n", "\r", _, NomeDoPaciente),
    join_strings([NomeDoPaciente, ".txt"], '', File),
    join_strings(["./Consultas", File], '/', Path),
    read_file(Path, Content), nl,
    inicializacao.

