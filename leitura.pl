% Predicate to find all .txt files in a directory
find_txt_files(Directory, TxtFiles) :-
    directory_files(Directory, Files),
    findall(File, (member(File, Files), file_name_extension(_, 'txt', File)), TxtFiles),
    read_files(TxtFiles).

read_files([]).
read_files([H|T]) :-
    write(H),
    nl,
    read_file(H, Content),
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