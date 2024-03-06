% Define predicates for reading and writing lines to a file

% Read lines from a file
read_lines(File, Lines) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).

read_lines(Stream, [Line|Rest]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Line),
    read_lines(Stream, Rest).

% Append a line to a file
append_line(File, Line) :-
    open(File, append, Stream),
    write(Stream, Line),
    nl(Stream), % Add a newline after the line
    close(Stream).

% Example usage:
% Read lines from a file and append a new line
example(File) :-
    read_lines(File, Lines),
    writeln('Contents of the file:'),
    maplist(writeln, Lines),
    append_line(File, 'This is a new line added by Prolog.'),
    writeln('Line added successfully.').

perguntar_sintoma(Sintoma) :-
  write('Você está com '), write(Sintoma), write('? (sim/nao) '),
  read(Resposta),
  ( Resposta = 'sim' -> assert(sintoma(Sintoma)); true ).
  
iterar_lista([Head | Tail]) :-
  perguntar_sintoma(Head),
  iterar_lista(Tail).


nausea(true).
vomito(true).
febre(true).
dor_de_cabeca(true).
dor_muscular(true).
rash_cutaneo(true).
tosse(true).
dificuldade_para_respirar(true).
fadiga(true).
perda_de_peso(true).
ferida_genital_dolorosa(true).
ferida_genital_indolor(true).
ferida_genital(true).
ictericia(true).
suores_noturnos(true).
feridas_na_pele(true).
dor_ao_urinar(true).
verrugas_genitais(true).
inchaco_dos_linfonodos(true).
corrimento_genital(true).

doenca(aids, [fadiga, perda_de_peso, febre, suores_noturnos]).
doenca(cancro_mole, [ferida_genital_dolorosa]).
doenca(corrimento, [corrimento_genital]).
doenca(donovanose, [ferida_genital_indolor]).
doenca(hepatite_b, [fadiga, nausea, vomito, ictericia]).
doenca(herpes, [feridas_na_pele]).
doenca(linfogranuloma, [inchaco_dos_linfonodos]).
doenca(papilomavirus, [verrugas_genitais]).
doenca(sifilis, [ferida_genital, rash_cutaneo]).
doenca(uretrite, [dor_ao_urinar]).
