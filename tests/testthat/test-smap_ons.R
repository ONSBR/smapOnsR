test_that("testa rodada 1 dia SMAP/ONS", {
  inicializacao <- smap_ons.inic(parametros[Nome == "SOBRADINHO"], EbInic = 800, TuInic = 0.15, Supin = 300, Rsup2Inic = 0)
  precipitacao <- 0.846906
  evapotranspiracao <- 5.29 * 0.9
  Emarg <- 5.29
  saida <- smap_ons.previsao(parametros[Nome == "SOBRADINHO"], inicializacao, precipitacao, evapotranspiracao, Emarg)

  expect_equal(colnames(saida), c("Qcalc", "Rsolo", "Rsup", "Rsup2", "Rsub",
                       "Es", "Er", "Rec", "Marg", "Ed", "Ed2", "Ed3",
                       "Eb", "Tu"))
  expect_equal(as.numeric(saida[1, 1]), 1100)

  precipitacao <- 0.904504
  saidaAnterior <- saida
  saida <- smap_ons.previsao(parametros[Nome == "SOBRADINHO"], inicializacao, precipitacao, evapotranspiracao, Emarg, saidaAnterior)
  expect_equal(as.numeric(saida[2, 1]), 1065.9265)
})
