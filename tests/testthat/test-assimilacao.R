test_that("testa a assimilacao de dados oficial", {
  sub_bacia <- "baixoig"
  data_rodada <- as.Date('2020/01/01')
  dias_assimilacao <- 31
  numero_dias <- dias_assimilacao
  EbInic <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
  Supin <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
  TuInic <- 0.5

  modelo <- new_modelo_smap_ons(parametros[Nome == sub_bacia], postos_plu[nome == sub_bacia])
  vetor_modelo <- unlist(modelo)
  area <- attributes(modelo)$area

  vazao <- historico_vazao[data < data_rodada & data >= (data_rodada - dias_assimilacao) 
                          & posto == sub_bacia, valor]
  normal_climatologica <- historico_etp_NC[nome == sub_bacia]

  kt_max <- sum(modelo$kt[1:2] > 0)
  kt_min <- sum(modelo$kt[4:63] > 0)
  precipitacao <- historico_precipitacao[data < (data_rodada + kt_max) & data >= (data_rodada - dias_assimilacao - kt_min) & posto == 'psatbigu']
  precipitacao <- ponderacao_espacial(precipitacao, postos_plu[nome == sub_bacia])

  evapotranspiracao <- transforma_NC_serie(precipitacao[data < data_rodada & data >= (data_rodada - dias_assimilacao)], normal_climatologica) 
  evapotranspiracao_planicie <- evapotranspiracao[, valor] * vetor_modelo[77]
  evapotranspiracao <- evapotranspiracao[, valor] * vetor_modelo[76]

  kt <- vetor_modelo[12:74]
  precipitacao_ponderada <- data.table::data.table(precipitacao)
  precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
  precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt, kt_max, kt_min)

  pesos <- rep(1, numero_dias)
  limite_prec <- c(0.5, 2)
  limite_ebin <- c(0.8, 1.2)
  limite_supin <- c(0, 2)

  vetor_parametros <- c(pesos, EbInic, Supin)

  fo <- funcao_objetivo_assimilacao_oficial(vetor_parametros, vetor_modelo, TuInic, 
      precipitacao_ponderada, evapotranspiracao, evapotranspiracao_planicie, vazao, area,
      numero_dias)

  saida <- assimilacao_oficial(vetor_modelo, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias = dias_assimilacao)
  
  expect_equal((saida$ajuste$value < fo), TRUE)
})

test_that("testa a assimilacao de dados com evapotranspiracao", {
  sub_bacia <- "baixoig"
  data_rodada <- as.Date('2020/01/01')
  dias_assimilacao <- 31
  numero_dias <- dias_assimilacao
  EbInic <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
  Supin <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
  TuInic <- 0.5

  modelo <- new_modelo_smap_ons(parametros[Nome == sub_bacia], postos_plu[nome == sub_bacia])
  vetor_modelo <- unlist(modelo)
  area <- attributes(modelo)$area

  vazao <- historico_vazao[data < data_rodada & data >= (data_rodada - dias_assimilacao) 
                          & posto == sub_bacia, valor]
  normal_climatologica <- historico_etp_NC[nome == sub_bacia]

  kt_max <- sum(modelo$kt[1:2] > 0)
  kt_min <- sum(modelo$kt[4:63] > 0)
  precipitacao <- historico_precipitacao[data < (data_rodada + kt_max) & data >= (data_rodada - dias_assimilacao - kt_min) & posto == 'psatbigu']
  evapotranspiracao <- historico_etp[data < data_rodada & data >= (data_rodada - dias_assimilacao) & posto == sub_bacia]
  evapotranspiracao_planicie <- evapotranspiracao[, valor] * vetor_modelo[77]
  evapotranspiracao <- evapotranspiracao[, valor] * vetor_modelo[76]

  kt <- vetor_modelo[12:74]
  precipitacao_ponderada <- data.table::data.table(precipitacao)
  precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
  precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt, kt_max, kt_min)

  pesos_prec <- rep(1, numero_dias)
  pesos_etp <- rep(1, numero_dias)
  limite_prec <- c(0.5, 2)
  limite_etp <- c(0.5, 2)
  limite_ebin <- c(0.8, 1.2)
  limite_supin <- c(0, 2)

  vetor_parametros <- c(pesos_prec, pesos_etp, EbInic, Supin)

  fo <- funcao_objetivo_assimilacao_evapotranspiracao(vetor_parametros, vetor_modelo, TuInic, 
      precipitacao_ponderada, evapotranspiracao, evapotranspiracao_planicie, vazao, area,
      numero_dias)

  saida <- assimilacao_evapotranspiracao(vetor_modelo, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias = dias_assimilacao)

  expect_equal((saida$ajuste$value < fo), TRUE)
})