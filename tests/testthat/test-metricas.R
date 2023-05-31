test_that("Testa calculo de metricas", {
  observacao <- 1:30
  simulacao <- observacao - 0.5
  nse <- calcula_nse(simulacao, observacao)
  mape <- calcula_mape(simulacao, observacao)
  dm <- calcula_dm(simulacao, observacao)
  expect_equal(nse, 0.99666296)
  expect_equal(mape, 0.066583119)
  expect_equal(dm, 0.06666669)
})