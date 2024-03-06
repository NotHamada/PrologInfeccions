sintoma(1, febre).
sintoma(2, tosse).
sintoma(3, dor_de_cabeca).
sintoma(4, fadiga).

doenca(gripe, [1, 2, 4]).
doenca(resfriado, [2, 3]).
doenca(enxaqueca, [3, 4]).

doenca(colera, [diarreia, vomito, desidratacao], [contato_agua_contaminada, contato_solo_contaminado]).
doenca(doenca_dos_viajantes, [diarreia, vomito, febre, dor_abdominal], [contato_agua_contaminada, contato_solo_contaminado]).
doenca(vaca_louca, [demencia, perda_de_coordenacao, mudancas_de_comportamento], [contato_carne_contaminada]).
doenca(febre_tifoide, [febre, dor_de_cabeca, dor_abdominal  ], [contato_agua_contaminada, contato_solo_contaminado]).
doenca(hepatite_a, [fadiga, nausea, vomito, ictericia], [contato_agua_contaminada, contato_alimento_contaminado]).
doenca(hepatite_e, [febre, fadiga, nausea, vomito, dor_abdominal], [contato_agua_contaminada, contato_alimento_contaminado]).
doenca(leptospirose, [febre, dor_de_cabeca, dor_muscular, ictericia], [contato_agua_contaminada, contato_solo_contaminado]).
doenca(poliomelite, [febre, dor_de_cabeca, rigidez_na_nuca, fraqueza_muscular], [contato_fezes_contaminadas]).
doenca(toxoplasmose, [febre, dor_de_cabeca, inchaco_dos_linfonodos, fadiga], [contato_alimento_contaminado]).
doenca(esquitossomose, [febre, dor_abdominal, tosse, sangue_na_urina], [contato_agua_contaminada]).
doenca(amebas, [diarreia, vomito, dor_abdominal, febre], [contato_agua_contaminada]).

% Definição de sintomas e suas doenças correspondentes
sintoma(1, diarreia).
sintoma(2, vomito).
sintoma(3, desidratacao).
sintoma(4, febre).
sintoma(5, dor_abdominal).
sintoma(6, demencia).
sintoma(7, perda_de_coordenacao).
sintoma(8, mudancas_de_comportamento).
sintoma(9, perda_de_apetite).
sintoma(10, dor_de_cabeca).
sintoma(11, fadiga).
sintoma(12, nausea).
sintoma(13, ictericia).
sintoma(14, dor_muscular).
sintoma(15, rigidez_na_nuca).
sintoma(16, fraqueza_muscular).
sintoma(17, inchaco_dos_linfonodos).
sintoma(18, tosse).
sintoma(19, sangue_na_urina).

doenca(colera, [1, 2, 3, 4, 5]).
doenca(doenca_dos_viajantes, [1, 2, 3, 4, 5]).
doenca(vaca_louca, [6, 7, 8, 9]).
doenca(febre_tifoide, [4, 10, 11, 12, 13]).
doenca(hepatite_a, [4, 9, 11, 13, 17]).
doenca(hepatite_e, [4, 9, 11, 13, 17]).
doenca(leptospirose, [4, 10, 14, 15, 16]).
doenca(poliomelite, [4, 8, 14, 16]).
doenca(toxoplasmose, [4, 9, 10, 12, 13]).
doenca(esquitossomose, [1, 2, 3, 17, 18]).
doenca(amebas, [1, 2, 3, 5]).


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