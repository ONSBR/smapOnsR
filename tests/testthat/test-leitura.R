test_that("transformacao de NC em serie temporal", {
  normal_climatologica <- historico_etp_NC[posto == 'baixoig']
  serie_temporal <- historico_precipitacao[posto == 'psatbigu']
  serie_temporal_NC <- transforma_NC_serie(serie_temporal, normal_climatologica)

  expect_equal(serie_temporal_NC[data == "2019/05/23", valor], normal_climatologica[mes == 5, valor])
})

test_that("transformacao de NC em serie temporal para vetor com apenas 1 mes", {
  data_rodada <- as.Date('2020/01/01')
  normal_climatologica <- historico_etp_NC[posto == 'baixoig']
  precipitacao <- historico_precipitacao[data < data_rodada & data >= (data_rodada - 30) & posto == 'psatbigu']
  evapotranspiracao <- transforma_NC_serie(precipitacao, normal_climatologica)

  expect_equal(evapotranspiracao[1, valor], normal_climatologica[mes == 12, valor])
})
