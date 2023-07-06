test_that("testa execucao oficial", {
  pasta_entrada <- system.file("extdata", "Arq_entrada1", package = "smapOnsR")

  saida <- executa_caso_oficial(pasta_entrada)

  expect_equal(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "ecmwf_ex42", valor][27], 202.10048)
})
