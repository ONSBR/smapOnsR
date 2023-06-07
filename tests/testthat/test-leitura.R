test_that("transformacao de NC em serie temporal", {
  normal_climatologica <- historico_etp_NC[posto == 'baixoig']
  serie_temporal <- historico_precipitacao[posto == 'psatbigu']
  serie_temporal_NC <- transforma_NC_serie(serie_temporal, normal_climatologica)

  expect_equal(serie_temporal_NC[data == "2019/05/23", valor], normal_climatologica[mes == 5, valor])
})
