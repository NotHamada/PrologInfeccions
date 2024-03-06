sintoma(febre).
sintoma(tosse).
sintoma(dor_de_cabeca).
sintoma(fadiga).

pergunta(febre) :-
    write('Você está com febre? (sim/nao): '),
    read(Resposta),
    Resposta = sim.

pergunta(tosse) :-
    write('Você está tossindo? (sim/nao): '),
    read(Resposta),
    Resposta = sim.

pergunta(dor_de_cabeca) :-
    write('Você está com dor de cabeça? (sim/nao): '),
    read(Resposta),
    Resposta = sim.

pergunta(fadiga) :-
    write('Você está se sentindo cansado ou com fadiga? (sim/nao): '),
    read(Resposta),
    Resposta = sim.

possivel_doenca(gripe) :-
    sintoma(febre),
    sintoma(tosse),
    sintoma(fadiga).

possivel_doenca(resfriado) :-
    sintoma(tosse),
    sintoma(dor_de_cabeca).

possivel_doenca(enxaqueca) :-
    sintoma(dor_de_cabeca),
    sintoma(fadiga).

diagnostico :-
    pergunta(Sintoma),
    possivel_doenca(Doenca),
    write('Baseado nos sintomas informados, você pode ter '), write(Doenca), nl,
    fail.

start :-
    write('Responda as seguintes perguntas para fazer um diagnóstico:'), nl,
    diagnostico,
    write('Fim do diagnóstico.'), nl.

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
