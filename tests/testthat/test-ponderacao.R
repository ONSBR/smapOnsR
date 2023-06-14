test_that("Testa ponderacao temporal", {
  sub_bacia <- "avermelha"
  modelo <- new_modelo_smap_ons(parametros[Nome == sub_bacia], postos_plu[posto == sub_bacia])
  kt <- unlist(modelo)[12:74]
  kt_min <- sum(kt[4:63] > 0)
  kt_max <- sum(kt[1:2] > 0)
  data_inicio <- historico_precipitacao[, max(data) - 30]
  data_fim <- historico_precipitacao[, max(data) - 2]

  serie_temporal <- historico_precipitacao[id == 1 &  data >= data_inicio - kt_min & data <= data_fim + kt_max, valor]

  saida <- ponderacao_temporal(serie_temporal, kt, kt_max, kt_min)
  expect_equal(saida[29], 4.9405)
})