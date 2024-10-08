test_that("testa rodada 2 dias SMAP/ONS", {
  sub_bacia <- "sobradinho"
  modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[posto == sub_bacia])
  area <- attributes(modelo)$area
  inicializacao <- inicializacao_smap(unlist(modelo), area, EbInic = 800, TuInic = 0.15, Supin = 300)
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
  sub_bacia <- "sobradinho"
  modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[posto == sub_bacia])
  area <- attributes(modelo)$area
  inicializacao <- inicializacao_smap(unlist(modelo), area, EbInic = 800, TuInic = 0.15, Supin = 300)
  precipitacao <- c(0.846906, 0.904504)
  evapotranspiracao <- c(5.29 * 0.9, 5.29 * 0.9)
  Emarg <- c(5.29, 5.29)
  saida <- funcaoSmapCpp::rodada_varios_dias_cpp2(unlist(modelo),
            unlist(inicializacao), attributes(modelo)$area, precipitacao,
            evapotranspiracao, Emarg,  numero_dias = 2)

  expect_equal(colnames(saida), c("Qcalc", "Rsolo", "Rsup", "Rsup2", "Rsub",
                                  "Es", "Er", "Rec", "Marg", "Ed", "Ed3", "Ed2",
                                  "Eb", "Tu", "Qsup1", "Qplan", "Qsup2", "Qbase"))
  expect_equal(as.numeric(saida[1, 1]), 1100)
  expect_equal(as.numeric(saida[2, 1]), 1065.9265)
})

test_that("testa rodada 2 dias e 2 cenarios SMAP/ONS rotina cpp", {
  sub_bacia <- "sobradinho"
  modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[posto == sub_bacia])
  vetor_modelo <- unlist(modelo)
  numero_cenarios <- 2
  area <- attributes(modelo)$area
  inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic = 800, TuInic = 0.15, Supin = 300)
  vetor_inicializacao <- t(array(rep(unlist(inicializacao), numero_cenarios), c(7, numero_cenarios)))
  
  precipitacao <- t(array(rep(c(0.846906, 0.904504), numero_cenarios), c(2, numero_cenarios)))
  evapotranspiracao <- t(array(rep(c(5.29 * 0.9, 5.29 * 0.9), numero_cenarios), c(2, numero_cenarios)))
  Emarg <- t(array(rep(c(5.29, 5.29), numero_cenarios), c(2, numero_cenarios)))

  saida <- funcaoSmapCpp::rodada_cenarios_dias_cpp2(vetor_modelo,
            vetor_inicializacao, area, precipitacao,
            evapotranspiracao, Emarg,  numero_dias = 2, numero_cenarios)

  expect_equal(colnames(saida[[1]]), c("Qcalc", "Rsolo", "Rsup", "Rsup2", "Rsub",
                                  "Es", "Er", "Rec", "Marg", "Ed", "Ed3", "Ed2",
                                  "Eb", "Tu", "Qsup1", "Qplan", "Qsup2", "Qbase"))
  expect_equal(as.numeric(saida[[1]][1, 1]), 1100)
  expect_equal(as.numeric(saida[[2]][2, 1]), 1065.9265)
})

test_that("testa calculo de kt_min e kt_max", {
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

  modelo <- new_modelo_smap_ons(entrada$parametros[nome == "jirau"],
            entrada$postos_plu[nome == "jirau"])
  expect_equal(attributes(modelo)$kt_max, 0)
  expect_equal(attributes(modelo)$kt_min, 16)

  unlink(system.file("extdata", "Validacao", package = "smapOnsR"), recursive = TRUE)

  pasta_entrada <- file.path(system.file("extdata", package = "smapOnsR"))
  arquivos_zip <- unzip(system.file("extdata", "dados_entrada.zip", package = "smapOnsR"), 
                  list = TRUE)
  pasta_especifica <- "caso_completo2"
  arquivos <- arquivos_zip$Name[grepl(pasta_especifica, arquivos_zip$Name)]
  zip::unzip(system.file("extdata", "dados_entrada.zip", package = "smapOnsR"),
            file = arquivos, 
            exdir = pasta_entrada)
  
  pasta_entrada <- system.file("extdata", "caso_completo2", "Arq_Entrada", package = "smapOnsR")
  entrada <- le_arq_entrada(pasta_entrada)

  modelo <- new_modelo_smap_ons(entrada$parametros[nome == "tucurui"],
            entrada$postos_plu[nome == "tucurui"])
  expect_equal(attributes(modelo)$kt_max, 2)
  expect_equal(attributes(modelo)$kt_min, 4)
  unlink(system.file("extdata", "caso_completo2", package = "smapOnsR"), recursive = TRUE)
})

  #modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[posto == sub_bacia])
  #precipitacao <- rep(5,1000)
  #evapotranspiracao <- precipitacao
  #Emarg <- evapotranspiracao
  #vetor1 <- unlist(modelo)
  #inicializacao <- inicializacao_smap(unlist(modelo), area, EbInic = 800, TuInic = 0.15, Supin = 300)
  #vetor2 <- unlist(inicializacao)
  #area <- attributes(modelo)$area
  #bnch <- bench::mark(rodada_varios_dias_cpp(vetor1,
  #          vetor2, area, precipitacao,
  #          evapotranspiracao, Emarg,  1000),
  #          rodada_varios_dias(modelo, inicializacao, precipitacao,
  #          evapotranspiracao, Emarg,  1000))



#modelo <- new_modelo_smap_ons(parametros[nome == "baixoig"])
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
