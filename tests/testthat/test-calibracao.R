test_that("multiplication works", {
  modelo <- new_modelo_smap_ons(parametros[Nome == "baixoig"])
  kt_max <- sum(modelo$kt[1:2] > 0)
  kt_min <- sum(modelo$kt[4:63] > 0)

  EbInic <- 300
  TuInic <- 0.8
  Supin <- 300

  normal_climatologica <- historico_etp_NC[posto == 'baixoig']
  precipitacao <- historico_precipitacao[posto == 'psatbigu']
  data_inicio_objetivo <- "2011-01-01"
  data_fim_objetivo <- "2011-12-31"
  evapotranspiracao <- transforma_NC_serie(precipitacao[data >= min(data) + kt_min & data <= data_fim_objetivo], normal_climatologica)
  vazao <- historico_vazao[data >= data_inicio_objetivo & data <= data_fim_objetivo & posto == "baixoig"]

  area <- attributes(modelo)$area
  vetor_modelo <- unlist(modelo)
  vetor_modelo <- c(vetor_modelo[1:11], vetor_modelo[75:77], 5, 5)

  limite_inferior <- vetor_modelo * 0.5
  limite_superior <- vetor_modelo * 2
  limite_inferior[12] <- 0.8
  limite_superior[12] <- 1.2
  limite_inferior[15:16] <- 0.1
  limite_superior[15:16] <- 10
  limite_superior[14] <- 0.0000001

  fo <- funcao_objetivo(vetor_modelo, kt_min, kt_max, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo)

  par <- calibracao(vetor_modelo,  kt_min, kt_max, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo,
      limite_inferior, limite_superior)
      
  expect_equal((ajuste$value < fo), TRUE)
})
