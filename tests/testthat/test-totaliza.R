test_that("testa totalizacao", {
  zip::unzip(system.file("extdata", "dados_entrada.zip", package = "smapOnsR"), exdir = system.file("extdata", package = "smapOnsR"))

  pasta_entrada <- system.file("extdata", "caso_completo2", "Arq_Entrada", package = "smapOnsR")
  pasta_saida <- system.file("extdata", "caso_completo2", "Arq_Saida", package = "smapOnsR")

  entrada <- le_arq_entrada(pasta_entrada)
  execucao <- le_execucao(pasta_saida, entrada$datas_rodadas$data)

  set.seed(129852)
  saida <- rodada_sem_assimilacao(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia, execucao)

  configuracao <- data.table::fread(file.path(system.file("extdata", "arq_entrada_novo", package = "smapOnsR"), "configuracao.csv"))
  configuracao[, sub_bacia_agrupada := tolower(sub_bacia_agrupada)]
  configuracao <- configuracao[sub_bacia_agrupada %in% entrada$caso$nome]
  vazao_observada <- entrada$vazao

  previsao_totalizada <- totaliza_previsao(saida$previsao[variavel == "Qcalc"], vazao_observada, configuracao)

  expect_equal(previsao_totalizada[nome == "Jirau" & data_previsao == "2023-03-13", previsao_total], 36810.152)

  unlink(system.file("extdata", "Arq_Entrada", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "arq_entrada_novo_sem_etp", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "Arq_Entrada0", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "Arq_Entrada1", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "caso_completo", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "caso_completo2", package = "smapOnsR"), recursive = TRUE)
})