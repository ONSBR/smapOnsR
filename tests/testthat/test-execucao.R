test_that("testa execucao oficial", {
  zip::unzip(system.file("extdata", "dados_entrada.zip", package = "smapOnsR"), exdir = system.file("extdata", package = "smapOnsR"))
  
  pasta_entrada <- system.file("extdata", "Arq_Entrada1", package = "smapOnsR")

  saida <- executa_caso_oficial(pasta_entrada)

  expect_equal(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "ecmwf_ex42", valor][27], 202.09854)

  unlink(system.file("extdata", "Arq_Entrada", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "arq_entrada_novo_sem_etp", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "Arq_Entrada0", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "Arq_Entrada1", package = "smapOnsR"), recursive = TRUE)
})
