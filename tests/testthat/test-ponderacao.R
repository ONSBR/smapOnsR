test_that("Testa ponderacao temporal", {
  serie_temporal <- historico_precipitacao[id == 1 & data >= max(data) - 100]
  data_inicio <- historico_precipitacao[, max(data) - 30]
  data_fim <- historico_precipitacao[, max(data) - 2]
  kt_min <- parametros[Nome == "AVERMELHA" & parametro == "ktMin", valor]
  kt_max <- parametros[Nome == "AVERMELHA" & parametro == "ktMax", valor]

  saida <- poderacao_temporal(serie_temporal, pesos, kt_min, kt_max,
  data_inicio, data_fim)
  expect_equal(saida[, data], seq.Date(from = data_inicio, to = data_fim, by = "day"))
  expect_equal(saida[, valor][29], 4.9405)
})
