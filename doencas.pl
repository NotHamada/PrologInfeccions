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
sintoma(20, hidrofobia).
sintoma(21, rash_cutaneo).
sintoma(22, rigidez_muscular).
sintoma(23, espasmos_musculares).
sintoma(24, dificuldade_para_engolir).
sintoma(25, confusao).
sintoma(26, hemorragia).
sintoma(27, calafrios).
sintoma(28, suor).
sintoma(29, inchaco_das_parotidas).
sintoma(30, dificuldade_para_respirar).
sintoma(31, perda_de_peso).
sintoma(32, suores_noturnos).
sintoma(33, ferida_genital_dolorosa).
sintoma(34, corrimento_vaginal).
sintoma(35, ferida_genital_indolor).
sintoma(36, feridas_na_pele).
sintoma(37, verrugas_genitais).
sintoma(38, ferida_genital).
sintoma(39, dor_ao_urinar).

tipo_infeccao(1, contato_agua_contaminada).
tipo_infeccao(2, contato_solo_contaminado).
tipo_infeccao(3, contato_alimento_contaminado).
tipo_infeccao(4, mordida_animal_infectado).
tipo_infeccao(5, contato_fezes_gato_infectado).
tipo_infeccao(6, contato_urina_rato_infectado).
tipo_infeccao(7, contato_ferida_contaminada).
tipo_infeccao(8, picada_mosquito_infectado).
tipo_infeccao(9, contato_gotículas_respiratórias).
tipo_infeccao(10, contato_saliva_contaminada).
tipo_infeccao(11, contato_sexual)

doenca(colera, [1, 2, 3], [1, 2]).
doenca(doenca_dos_viajantes, [1, 2, 4, 5], [1, 2]).
doenca(vaca_louca, [6, 7, 8], [3]).
doenca(febre_tifoide, [4, 10, 5  ], [1, 2]).
doenca(hepatite_a, [11, 12, 2, 13], [1, 3]).
doenca(hepatite_e, [4, 11, 12, 2, 5], [1, 3]).
doenca(leptospirose, [4, 10, 14, 13], [1, 2]).
doenca(poliomelite, [4, 10, 15, 16], [1, 2]).
doenca(toxoplasmose, [4, 10, 17, 11], [3]).
doenca(esquitossomose, [4, 5, 18, 19], [1]).
doenca(amebas, [1, 2, 5, 4], [1]).

doenca(raiva, [4, 10, 25, 20], [4]).
doenca(doenca_do_gato, [4, 10, 17, 11], [5]).
doenca(doenca_do_rato, [4, 10, 14, 21], [6]).
doenca(tetano, [22, 23, 24], [7]).

doenca(doenca_de_chagas, [4, 10, 17, 11], [8]).
doenca(dengue, [4, 10, 14, 21], [8]).
doenca(encefalite, [4, 10, 15, 25], [8]).
doenca(febre_amarela, [4, 10, 14, 13, 26], [8]).
doenca(malaria, [4, 10, 27, 28], [8]).

doenca(caxumba, [4, 10, 29], [10]).
doenca(difteria, [4, dor_de_garganta, 30], [9]).
doenca(gripe, [4, 10, dor_de_garganta, 18], [9]).
doenca(pneumonia, [4, 18, 30, dor_no_peito], [9]).
doenca(sarampo, [4, 10, 18, coriza, 21], [9]).
doenca(sars, [4, 10, 18, 30], [9]).
doenca(rubeola, [4, 10, 17, 21], [9]).
doenca(tuberculose, [18, 31, 11, 4], [9]).
doenca(varicela, [4, 10, 11, 21], [9]).

doenca(aids, [11, 31, 4, 32], [11]).
doenca(cancro_mole, [33], [11]).
doenca(corrimento, [34], [11]).
doenca(donovanose, [35], [11]).
doenca(hepatite_b, [11, 12, 2, 13], [11]).
doenca(herpes, [36], [11]).
doenca(linfogranuloma, [17], [11]).
doenca(papilomavirus, [37], [11]).
doenca(sifilis, [38, 21], [11]).
doenca(uretrite, [39], [11]).