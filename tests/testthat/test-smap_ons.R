test_that("testa rodada 2 dias SMAP/ONS", {
  
  modelo <- new_modelo_smap_ons(parametros[Nome == "sobradinho"])
  inicializacao <- smap_ons.inic(unlist(modelo), area, EbInic = 800, TuInic = 0.15, Supin = 300)
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

test_that("testa rodada 2 dias SMAP/ONS", {
  
  modelo <- new_modelo_smap_ons(parametros[Nome == "sobradinho"])
  inicializacao <- smap_ons.inic(unlist(modelo), area, EbInic = 800, TuInic = 0.15, Supin = 300)
  precipitacao <- c(0.846906, 0.904504)
  evapotranspiracao <- c(5.29 * 0.9, 5.29 * 0.9)
  Emarg <- c(5.29, 5.29)
  saida <- rodada_varios_dias(modelo, inicializacao, precipitacao, evapotranspiracao, Emarg,  numero_dias = 2)

  expect_equal(colnames(saida), c("Qcalc", "Rsolo", "Rsup", "Rsup2", "Rsub",
                       "Es", "Er", "Rec", "Marg", "Ed", "Ed2", "Ed3",
                       "Eb", "Tu"))
  expect_equal(as.numeric(saida[1, 1]), 1100)
  expect_equal(as.numeric(saida[2, 1]), 1065.9265)
})

test_that("testa rodada 2 dias SMAP/ONS rotina cpp", {
  
  modelo <- new_modelo_smap_ons(parametros[Nome == "sobradinho"])
  area <- attributes(modelo)$area
  inicializacao <- smap_ons.inic(unlist(modelo), area, EbInic = 800, TuInic = 0.15, Supin = 300)
  precipitacao <- c(0.846906, 0.904504)
  evapotranspiracao <- c(5.29 * 0.9, 5.29 * 0.9)
  Emarg <- c(5.29, 5.29)
  saida <- rodada_varios_dias_cpp(unlist(modelo),
            unlist(inicializacao), attributes(modelo)$area, precipitacao,
            evapotranspiracao, Emarg,  numero_dias = 2)

  expect_equal(colnames(saida), c("Qcalc", "Rsolo", "Rsup", "Rsup2", "Rsub",
                       "Es", "Er", "Rec", "Marg", "Ed", "Ed2", "Ed3",
                       "Eb", "Tu"))
  expect_equal(as.numeric(saida[1, 1]), 1100)
  expect_equal(as.numeric(saida[2, 1]), 1065.9265)
})

#  precipitacao <- rep(5,1000)
#  evapotranspiracao <- precipitacao
#  Emarg <- evapotranspiracao
#  vetor1 <- unlist(modelo)
#  vetor2 <- unlist(inicializacao)
#  area <- attributes(modelo)$area
#  bnch <- bench::mark(rodada_varios_dias_cpp(vetor1,
#            vetor2, area, precipitacao,
#            evapotranspiracao, Emarg,  1000),
#            rodada_varios_dias(modelo, inicializacao, precipitacao,
#            evapotranspiracao, Emarg,  1000))


#test_that("testa rodada 2 dias SMAP/ONS", {
  
#  modelo <- new_modelo_smap_ons(parametros[Nome == "avermelha"])
#  EbInic <- 100
#  TuInic <- 0.15
#  Supin <- 300
#  precipitacao = historico_precipitacao[id == 1]
#  evapotranspiracao = historico_etp[id == 1]
#  evapotranspiracao[, valor := lubridate::month(data)]
#  vazao = historico_vazao[posto == "avermelha"]
#  data_inicio_objetivo <- "2011-01-01"
#  kt_max <- sum(modelo$kt[1:2] > 0)
#  data_fim_objetivo <- evapotranspiracao[, max(data) - kt_max]
#})

#test_that("testa rodada 2 dias SMAP/ONS", {
#      aux <- parametros[Nome == "avermelha"]
#      aux[, valor := valor * 0.5]
#      limite_inferior <- aux
#      limite_inferior <- new_modelo_smap_ons(limite_inferior)

#      aux <- parametros[Nome == "avermelha"]
#      aux[, valor := valor * 1.5]
#      limite_superior <- aux
#      limite_superior <- new_modelo_smap_ons(limite_superior)
#})