test_that("testa rodada 2 dias SMAP/ONS", {
  
  modelo <- new_modelo_smap_ons(parametros[Nome == "sobradinho"])
  inicializacao <- smap_ons.inic(modelo, EbInic = 800, TuInic = 0.15, Supin = 300)
  precipitacao <- 0.846906
  evapotranspiracao <- 5.29 * 0.9
  Emarg <- 5.29
  saida <- rodada_diaria(modelo, inicializacao, precipitacao, evapotranspiracao, Emarg)

  expect_equal(colnames(saida), c("Qcalc", "Rsolo", "Rsup", "Rsup2", "Rsub",
                       "Es", "Er", "Rec", "Marg", "Ed", "Ed2", "Ed3",
                       "Eb", "Tu"))
  expect_equal(as.numeric(saida[1, 1]), 1100)

  precipitacao <- 0.904504
  saidaAnterior <- saida
  inicializacao$RsoloInic <- saidaAnterior[1, 2]
  inicializacao$RsupInic <- saidaAnterior[1, 3]
  inicializacao$Rsup2Inic <- saidaAnterior[1, 4]
  inicializacao$RsubInic <- saidaAnterior[1, 5]
  
  saida <- rodada_diaria(modelo, inicializacao, precipitacao, evapotranspiracao, Emarg, saidaAnterior)
  expect_equal(as.numeric(saida[2, 1]), 1065.9265)
})

#test_that("testa rodada 2 dias SMAP/ONS", {
  
#  modelo <- new_modelo_smap_ons(parametros[Nome == "AVERMELHA"])
#  EbInic <- 100
#  TuInic <- 0.15
#  Supin <- 300
#  precipitacao = historico_precipitacao[id == 1]
#  evapotranspiracao = historico_precipitacao[id == 1]
#  evapotranspiracao[, valor := lubridate::month(data)]
#  vazao = historico_vazao[posto == "AVERMELHA"]
#  data_inicio_objetivo <- "2011-01-01"
#  data_fim_objetivo <- evapotranspiracao[, max(data) - modelo$kt_max] 
#})
