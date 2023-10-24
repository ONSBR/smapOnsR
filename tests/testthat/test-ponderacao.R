test_that("Testa ponderacao temporal", {
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
})

test_that("Testa ponderacao espacial", {
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