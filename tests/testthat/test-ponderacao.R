test_that("Testa ponderacao temporal", {
  #CT24.1
  sub_bacia <- "avermelha"
  modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome == sub_bacia])
  kt <- unlist(modelo)[12:74]
  kt_min <- sum(kt[4:63] > 0)
  kt_max <- sum(kt[1:2] > 0)
  data_inicio <- as.Date("2021-12-01")
  data_fim <- as.Date("2021-12-29")

  serie_temporal <- historico_precipitacao[posto == "psatagv" &  data >= data_inicio - kt_min & data <= data_fim + kt_max, valor]

  saida <- ponderacao_temporal(serie_temporal, kt, kt_max, kt_min)
  expect_equal(saida[29], 4.9405)

  #CT24.3
  kt[2] <- kt[2] + 0.001
  saida <- ponderacao_temporal(serie_temporal, kt, kt_max, kt_min)
  expect_equal(saida[29], 4.95354)

  #CT24.4
  pasta_entrada <- file.path(system.file("extdata", package = "smapOnsR"), "Validacao")
  dir.create(pasta_entrada)
  arquivos_zip <- unzip(system.file("extdata", "validacao.zip", package = "smapOnsR"), 
                  list = TRUE)
  pasta_especifica <- "CN07/CT7.97/"
  arquivos <- arquivos_zip$Name[grepl(pasta_especifica, arquivos_zip$Name)]
  zip::unzip(system.file("extdata", "validacao.zip", package = "smapOnsR"),
            file = arquivos, 
            exdir = pasta_entrada)
  pasta_entrada <- file.path(pasta_entrada, "CN07", "CT7.97")
  entrada <- le_arq_entrada(pasta_entrada)

  sub_bacia <- "jirau"
  dataRodada <- entrada$datas_rodadas$data
  previsao_rodada <- entrada$previsao_precipitacao[nome == sub_bacia]
  numero_dias_assimilacao <- 31
  modelo <- new_modelo_smap_ons(entrada$parametros[nome == "jirau"],
            entrada$postos_plu[nome == "jirau"])
  kt_max <- attributes(modelo)$kt_max
  kt_min <- attributes(modelo)$kt_min
  kt <- modelo$kt

  precipitacao <- data.table::data.table(entrada$precipitacao[nome == sub_bacia &
  data <= dataRodada & data >= (dataRodada  - kt_min)])
  precipitacao[, data_rodada := dataRodada]
  
  precipitacao <- combina_observacao_previsao(precipitacao, previsao_rodada)
  precipitacao <- precipitacao[data_previsao <= (dataRodada + kt_max) & 
                data_previsao >= (dataRodada  - kt_min)]
  previsao <- ponderacao_temporal(precipitacao[, valor], kt,
                                                    kt_max, kt_min)
  expect_equal(previsao, 3.9277411)
  unlink(system.file("extdata", "Validacao", package = "smapOnsR"), recursive = TRUE)
})

test_that("Testa ponderacao espacial", {
  #CT24.2
  sub_bacia <- "pimentalt"
  modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome == sub_bacia])
  kt <- unlist(modelo)[12:74]
  kt_min <- sum(kt[4:63] > 0)
  kt_max <- sum(kt[1:2] > 0)
  data_inicio <- as.Date("2021-12-01")
  data_fim <- as.Date("2021-12-29")
  precipitacao <- historico_precipitacao[data <= (data_fim + kt_max) &
            data >= (data_inicio - kt_min) & posto %in% postos_plu[nome == sub_bacia, posto]]

  precipitacao_ponderada <- ponderacao_espacial(precipitacao, postos_plu[nome == sub_bacia])

  calculo <- precipitacao[data == (data_fim + kt_max), sum(valor* c(0.264, 0.037, 0.699))]
  expect_equal(precipitacao_ponderada[data == (data_fim + kt_max), valor], calculo)
})