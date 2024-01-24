test_that("Testa calculo de metricas", {
  #CT23.1
  observacao <- 1:30
  simulacao <- observacao - 0.5
  nse <- calcula_nse(simulacao, observacao)
  mape <- calcula_mape(simulacao, observacao)
  dm <- calcula_dm(simulacao, observacao)
  kge <- calcula_kge(simulacao, observacao)
  pbias <- calcula_pbias(simulacao, observacao)
  correlacao <- calcula_correlacao(simulacao, observacao)
  expect_equal(nse, 0.99666296)
  
  #CT23.2
  expect_equal(mape, 0.066583119)

  #CT23.3
  expect_equal(dm, 0.06666669)

  #CT23.4
  expect_equal(kge, 0.96774194)

  #CT23.5
  expect_equal(pbias, 0.96774194)

  #CT23.6
  expect_equal(correlacao, 1)
})