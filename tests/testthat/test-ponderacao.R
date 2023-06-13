#test_that("Testa ponderacao temporal", {
#  serie_temporal <- historico_precipitacao[id == 1 & data >= max(data) - 100]
#  data_inicio <- historico_precipitacao[, max(data) - 30]
#  data_fim <- historico_precipitacao[, max(data) - 2]
#  modelo <- new_modelo_smap_ons(parametros[Nome == "avermelha"])
#  kt <- unlist(modelo)[12:74]
#
#  saida <- poderacao_temporal(serie_temporal, kt, data_inicio, data_fim)
#  expect_equal(saida[, data], seq.Date(from = data_inicio, to = data_fim, by = "day"))
#  expect_equal(saida[, valor][29], 4.9405)
#})

test_that("Testa ponderacao temporal 2", {
  modelo <- new_modelo_smap_ons(parametros[Nome == "avermelha"])
  kt <- unlist(modelo)[12:74]
  kt_min <- sum(kt[4:63] > 0)
  kt_max <- sum(kt[1:2] > 0)
  data_inicio <- historico_precipitacao[, max(data) - 30]
  data_fim <- historico_precipitacao[, max(data) - 2]

  serie_temporal <- historico_precipitacao[id == 1 &  data >= data_inicio - kt_min & data <= data_fim + kt_max, valor]

  saida <- ponderacao_temporal2(serie_temporal, kt, kt_max, kt_min)
  expect_equal(saida[29], 4.9405)
})

#test_that("Testa ponderacao temporal 2", {
#  modelo <- new_modelo_smap_ons(parametros[Nome == "avermelha"])
#  kt <- unlist(modelo)[12:74]
#  kt_min <- sum(kt[4:63] > 0)
#  kt_max <- sum(kt[1:2] > 0)
#  data_inicio <- historico_precipitacao[, max(data) - 30]
#  data_fim <- historico_precipitacao[, max(data) - 2]
#
#  serie_temporal <- historico_precipitacao[id == 1 &  data >= data_inicio - kt_min & data <= data_fim + kt_max, valor]
#
#  saida <- ponderacao_temporal_cpp(serie_temporal, kt)
#  expect_equal(saida[29], 4.9405)
#})