ler_arquivo(Arquivo, Linhas) :-
  open(Arquivo, read, Stream),
  ler_linhas(Stream, Linhas),
  close(Stream).

ler_linhas(Stream, Linhas) :-
  read_line(Stream, Linha),
  (   Linha = end_of_file ->
      Linhas = []
  ;   Linhas = [Linha | LinhasRestantes],
      ler_linhas(Stream, LinhasRestantes)
  ).

print_list_newline([Head]) :-
  write(Head), nl.

print_list_newline([Head|Tail]) :-
  write(Head), nl,
  \+ member(Head, Tail),  % Verifica se o elemento atual está na cauda
  print_list_newline(Tail).
  
perguntar_sintoma(Sintoma) :-
  write('Você está com '), write(Sintoma), write('? (sim/nao) '),
  read(Resposta),
  ( Resposta = 'sim' -> assert(sintoma(Sintoma)); true ).
  
iterar_lista([Head | Tail]) :-
  perguntar_sintoma(Head),
  iterar_lista(Tail).


febre(true).
dor_de_cabeca(true).
dor_muscular(true).
diarreia(true).
vomito(true).
rash_cutaneo(true).
tosse(true).
dificuldade_para_respirar(true).
fadiga(true).
perda_peso(true).




doenca(colera, [diarreia, vomito, desidratacao], [contato_agua_contaminada, contato_solo_contaminado]).
doenca(doenca_dos_viajantes, [diarreia, vomito, febre, dor_abdominal], [contato_agua_contaminada, contato_solo_contaminado]).
doenca(vaca_louca, [demencia, perda_de_coordenacao, mudancas_de_comportamento], [contato_carne_contaminada]).
doenca(febre_tifoide, [febre, dor_de_cabeca, dor_abdominal, perda_de_apetite], [contato_agua_contaminada, contato_solo_contaminado]).
doenca(hepatite_a, [fadiga, nausea, vomito, ictericia], [contato_agua_contaminada, contato_alimento_contaminado]).
doenca(hepatite_e, [febre, fadiga, nausea, vomito, dor_abdominal], [contato_agua_contaminada, contato_alimento_contaminado]).
doenca(leptospirose, [febre, dor_de_cabeca, dor_muscular, ictericia], [contato_agua_contaminada, contato_solo_contaminado]).
doenca(poliomelite, [febre, dor_de_cabeca, rigidez_na_nuca, fraqueza_muscular], [contato_fezes_contaminadas]).
doenca(toxoplasmose, [febre, dor_de_cabeca, inchaco_dos_linfonodos, fadiga], [contato_alimento_contaminado]).
doenca(esquitossomose, [febre, dor_abdominal, tosse, sangue_na_urina], [contato_agua_contaminada]).
doenca(amebas, [diarreia, vomito, dor_abdominal, febre], [contato_agua_contaminada]).

doenca(raiva, [febre, dor_de_cabeca, confusao, hidrofobia], [contato_mordida_animal_infectado]).
doenca(doenca_do_gato, [febre, dor_de_cabeca, inchaco_dos_linfonodos, fadiga], [contato_fezes_gato_infectado]).
doenca(doenca_do_rato, [febre, dor_de_cabeca, dor_muscular, rash_cutaneo], [contato_urina_rato_infectado]).
doenca(tetano, [rigidez_muscular, espasmos_musculares, dificuldade_para_engolir], [contato_ferida_contaminada]).

doenca(doenca_de_chagas, [febre, dor_de_cabeca, inchaco_dos_linfonodos, fadiga], [picada_mosquito_infectado]).
doenca(dengue, [febre, dor_de_cabeca, dor_muscular, rash_cutaneo], [picada_mosquito_infectado]).
doenca(encefalite, [febre, dor_de_cabeca, rigidez_na_nuca, confusao], [picada_mosquito_infectado]).
doenca(febre_amarela, [febre, dor_de_cabeca, dor_muscular, ictericia, hemorragia], [picada_mosquito_infectado]).
doenca(malaria, [febre, dor_de_cabeca, calafrios, suor], [picada_mosquito_infectado]).

doenca(caxumba, [febre, dor_de_cabeca, inchaco_das_parotidas], [contato_saliva_contaminada]).
doenca(difteria, [febre, dor_de_garganta, dificuldade_para_respirar], [contato_gotículas_respiratórias]).
doenca(gripe, [febre, dor_de_cabeca, dor_de_garganta, tosse], [contato_gotículas_respiratórias]).
doenca(pneumonia, [febre, tosse, dificuldade_para_respirar, dor_no_peito], [contato_gotículas_respiratórias]).
doenca(sarampo, [febre, dor_de_cabeca, tosse, coriza, rash_cutaneo], [contato_gotículas_respiratórias]).
doenca(sars, [febre, dor_de_cabeca, tosse, dificuldade_para_respirar], [contato_gotículas_respiratórias]).
doenca(rubeola, [febre, dor_de_cabeca, inchaco_dos_linfonodos, rash_cutaneo], [contato_gotículas_respiratórias]).
doenca(tuberculose, [tosse, perda_de_peso, fadiga, febre], [contato_gotículas_respiratórias]).
doenca(varicela, [febre, dor_de_cabeca, fadiga, rash_cutaneo], [contato_gotículas_respiratórias]).

doenca(aids, [fadiga, perda_de_peso, febre, suores_noturnos], [contato_sexual]).
doenca(cancro_mole, [ferida_genital_dolorosa], [contato_sexual]).
doenca(corrimento, [corrimento_vaginal], [contato_sexual]).
doenca(donovanose, [ferida_genital_indolor], [contato_sexual]).
doenca(hepatite_b, [fadiga, nausea, vomito, ictericia], [contato_sexual]).
doenca(herpes, [feridas_na_pele], [contato_sexual]).
doenca(linfogranuloma, [inchaco_dos_linfonodos], [contato_sexual]).
doenca(papilomavirus, [verrugas_genitais], [contato_sexual]).
doenca(sifilis, [ferida_genital, rash_cutaneo], [contato_sexual]).
doenca(uretrite, [dor_ao_urinar], [contato_sexual]).
