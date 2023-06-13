test_that("testa rodada 2 dias SMAP/ONS", {
  
  modelo <- new_modelo_smap_ons(parametros[Nome == "sobradinho"])
  area <- attributes(modelo)$area
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
  area <- attributes(modelo)$area
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

  #modelo <- new_modelo_smap_ons(parametros[Nome == "sobradinho"])
  #precipitacao <- rep(5,1000)
  #evapotranspiracao <- precipitacao
  #Emarg <- evapotranspiracao
  #vetor1 <- unlist(modelo)
  #inicializacao <- smap_ons.inic(unlist(modelo), area, EbInic = 800, TuInic = 0.15, Supin = 300)
  #vetor2 <- unlist(inicializacao)
  #area <- attributes(modelo)$area
  #bnch <- bench::mark(rodada_varios_dias_cpp(vetor1,
  #          vetor2, area, precipitacao,
  #          evapotranspiracao, Emarg,  1000),
  #          rodada_varios_dias(modelo, inicializacao, precipitacao,
  #          evapotranspiracao, Emarg,  1000))



#modelo <- new_modelo_smap_ons(parametros[Nome == "baixoig"])
#kt_max <- sum(modelo$kt[1:2] > 0)
#kt_min <- sum(modelo$kt[4:63] > 0)
#
#EbInic <- 300
#TuInic <- 0.8
#Supin <- 300
#
#normal_climatologica <- historico_etp_NC[posto == 'baixoig']
#precipitacao <- historico_precipitacao[posto == 'psatbigu']
#data_inicio_objetivo <- "2011-01-01"
#data_fim_objetivo <- precipitacao[, max(data) - kt_max]
#evapotranspiracao <- transforma_NC_serie(precipitacao[data >= min(data) + kt_min & data <= data_fim_objetivo], normal_climatologica)
#vazao <- historico_vazao[data >= data_inicio_objetivo & data <= data_fim_objetivo & posto == "baixoig"]
#
#area <- attributes(modelo)$area
#modelo <- unlist(modelo)
#modelo <- c(modelo[1:11], modelo[75:77], 5, 5)
#
#limite_inferior <- modelo * 0.5
#limite_superior <- modelo * 2
#limite_inferior[12] <- 0.8
#limite_superior[12] <- 1.2
#limite_inferior[15:16] <- 0.1
#limite_superior[15:16] <- 10
#limite_superior[14] <- 0.1
#
#fo <- funcao_objetivo(modelo, kt_min, kt_max, area, EbInic, TuInic, Supin, precipitacao,
#    evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo)
#
#par <- calibracao(modelo,  kt_min, kt_max, area, EbInic, TuInic, Supin, precipitacao,
#    evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo,
#    limite_inferior, limite_superior)
