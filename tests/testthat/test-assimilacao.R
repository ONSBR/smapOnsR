test_that("multiplication works", {
  
  data_rodada <- as.Date('2020/01/01')
  dias_assimilacao <- 31
  numero_dias <- dias_assimilacao
  EbInic <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == "baixoig", valor] / 2
  Supin <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == "baixoig", valor] / 2
  TuInic <- 0.5

  modelo <- new_modelo_smap_ons(parametros[Nome == "baixoig"])
  vetorModelo <- unlist(modelo)
  area <- attributes(modelo)$area

  vazao <- historico_vazao[data < data_rodada & data >= (data_rodada - dias_assimilacao) 
                          & posto == "baixoig", valor]
  normal_climatologica <- historico_etp_NC[posto == 'baixoig']

  kt_max <- sum(modelo$kt[1:2] > 0)
  kt_min <- sum(modelo$kt[4:63] > 0)
  precipitacao <- historico_precipitacao[data < (data_rodada + kt_max) & data >= (data_rodada - dias_assimilacao - kt_min) & posto == 'psatbigu']
  evapotranspiracao <- transforma_NC_serie(precipitacao[data < data_rodada & data >= (data_rodada - dias_assimilacao)], normal_climatologica) 
  evapotranspiracao_planicie <- evapotranspiracao[, valor] * vetorModelo[77]
  evapotranspiracao <- evapotranspiracao[, valor] * vetorModelo[76]

  kt <- vetorModelo[12:74]
  precipitacao_ponderada <- data.table::data.table(precipitacao)
  precipitacao_ponderada[, valor := valor * vetorModelo[75]]
  precipitacao_ponderada <- ponderacao_temporal2(precipitacao_ponderada[, valor], kt, kt_max, kt_min)

  pesos <- rep(1,numero_dias)
  limite_prec <- c(0.5, 2)
  limite_ebin <- c(0.8, 1.2)
  limite_supin <- c(0, 2)

  limite_inferior = c(rep(limite_prec[1],numero_dias), limite_ebin[1], limite_supin[1])
  limite_superior = c(rep(limite_prec[2],numero_dias), limite_ebin[2], limite_supin[2])

  vetorParametros <- c(pesos, EbInic, Supin)

  fo <- funcao_objetivo_assimilacao(vetorParametros, vetorModelo, TuInic, 
      precipitacao_ponderada, evapotranspiracao, evapotranspiracao_planicie, vazao, area,
      numero_dias)

  ajuste <- assimilacao_oficial(vetorModelo, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias = dias_assimilacao,
      data_rodada = data_rodada)
  
  expect_equal((ajuste$value < fo), TRUE)
})