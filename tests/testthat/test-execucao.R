test_that("testa execucao oficial", {
  zip::unzip(system.file("extdata", "dados_entrada.zip", package = "smapOnsR"), exdir = system.file("extdata", package = "smapOnsR"))
  
  pasta_entrada <- system.file("extdata", "Arq_Entrada1", package = "smapOnsR")

  saida <- executa_caso_oficial(pasta_entrada)

  secao <- sessionInfo()
  
  if (secao$R.version$os == "mingw32") {
    expect_equal(round(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "ecmwf_ex42", valor][27], 0), 202)
  } else {
    expect_true(abs(round(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "ecmwf_ex42", valor][27], 0) - 202) < 202 * 0.01)
  }
})

test_that("testa execucao novo formato", {
  pasta_entrada <- system.file("extdata", "arq_entrada_novo", package = "smapOnsR")

  saida <- executa_caso_etp(pasta_entrada)

  secao <- sessionInfo()
  
  if (secao$R.version$os == "mingw32") {
    expect_true(abs(round(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "historico", valor][27], 0) - 733) < 733 * 0.01)
  } else {
    expect_equal(round(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "historico", valor][27], 0), 733)
  }

  unlink(system.file("extdata", "Arq_Entrada", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "arq_entrada_novo_sem_etp", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "Arq_Entrada0", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "Arq_Entrada1", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "caso_completo", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "caso_completo2", package = "smapOnsR"), recursive = TRUE)
})