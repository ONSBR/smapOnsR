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

test_that("transformacao historico em previsao", {
  sub_bacia <- "avermelha"
  precipitacao <- historico_precipitacao[posto %in% postos_plu[nome == sub_bacia, posto]]

  datas_rodadas <- data.table(
    data = as.Date(c("2020-05-01", "2020-05-08")),
    numero_dias_previsao = c(15, 20)
  )

  previsao <- transforma_historico_previsao(precipitacao, datas_rodadas)

  expect_equal(previsao[data_previsao == "2020-05-05", valor], precipitacao[data  == "2020-05-05", valor] )
  expect_equal(previsao[data_previsao == "2020-05-10" & data_rodada == "2020-05-01" , valor], 
              previsao[data_previsao == "2020-05-10" & data_rodada == "2020-05-08" , valor])

  datas_rodadas <- data.table(
    a = as.Date(c("2020-05-01", "2020-05-08")),
    numero_dias_previsao = c(15, 20)
  )        
  expect_error(transforma_historico_previsao(precipitacao, datas_rodadas))

  datas_rodadas <- data.table(
    data = c("2020-05-01", "2020-05-08"),
    numero_dias_previsao = c(15, 20)
  )
  expect_error(transforma_historico_previsao(precipitacao, datas_rodadas))
})