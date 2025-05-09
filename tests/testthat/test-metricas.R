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

test_that("analisa_previsoes diária funciona para horizonte 1 com dois data_caso e observações distintas", {
  # Cria duas rodadas (data_caso) distintas
  data_casos <- as.Date(c("2000-01-01", "2000-01-10"))
  
  # Simulação: sempre os mesmos valores de previsão para cada rodada
  simulacao <- rbindlist(lapply(data_casos, function(dc) {
    data.table(
      data_caso     = rep(dc, 3),
      data_previsao = dc + 0:2,
      cenario       = "h",
      nome          = "A",
      variavel      = "Qcalc",
      valor         = c(10, 20, 30)
    )
  }))
  
  # Observação: valores diferentes em cada rodada
  observacao <- rbindlist(list(
    data.table(data = data_casos[1] + 0:2, posto = "A", valor = c(12, 18, 25)),
    data.table(data = data_casos[2] + 0:2, posto = "A", valor = c(11, 17, 24))
  ))
  
  out <- analisa_previsoes(simulacao, observacao,
                           semanal = FALSE,
                           mensal  = FALSE,
                           anual   = FALSE)$resultado

  # Agora temos 3 horizontes * 4 métricas = 12 linhas
  expect_equal(nrow(out), 12)
  
  # Recupera PBIAS e NSE para horizonte 1
  sub1 <- out[discretizacao=="diaria" & horizonte==1 & nome=="A"]
  
  # Deve haver exatamente 4 métricas neste sub-grupo
  expect_equal(nrow(sub1), 4L)
  expect_setequal(sub1$metrica, c("PBIAS","NSE","MAPE","DM"))
  
  # Verifica que NSE não seja NA ou Inf
  nse1 <- sub1[metrica=="NSE", valor]
  expect_true(is.finite(nse1))
  
  # Verifica um valor previsto de PBIAS manualmente:
  # prevs  = c(20,20), obss = c(18,17)
  # PBIAS = sum(prev-observacao)/sum(observacao)*100
  esperado_pbias <- 1 + sum(c(20,20) - c(18,17)) / sum(c(18,17)) 
  pbias1 <- sub1[metrica=="PBIAS", valor]
  expect_equal(pbias1, esperado_pbias)
})

test_that("analisa_previsoes retorna todas as combinações esperadas", {
  data_caso <- as.Date("2000-01-01")
  simulacao <- data.table(
    data_caso     = rep(data_caso, 5),
    data_previsao = data_caso + 0:4,
    cenario       = "h",
    nome          = "D",
    variavel      = "Qcalc",
    valor         = 1:5
  )
  observacao <- data.table(
    data  = data_caso + 0:4,
    posto = "D",
    valor = 1:5
  )

  out <- analisa_previsoes(simulacao, observacao, semanal = TRUE, mensal = TRUE, anual = FALSE)$resultado

  # Deve conter as três discretizações
  expect_setequal(unique(out$discretizacao), c("diaria","semanal","mensal"))
  # Horizonte mínimo de cada = 1
  primeiros <- out[, .SD[1], by = discretizacao]
  expect_equal(c(0, 1, 1), primeiros$horizonte)
})