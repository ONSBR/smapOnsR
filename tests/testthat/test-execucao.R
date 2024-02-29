test_that("testa execucao oficial", {
  zip::unzip(system.file("extdata", "dados_entrada.zip", package = "smapOnsR"), exdir = system.file("extdata", package = "smapOnsR"))
  
  pasta_caso <- system.file("extdata", "caso_completo2", package = "smapOnsR")
  file.remove(list.files((file.path(pasta_caso, "Arq_Saida")), full.names = TRUE))
  executa_caso_oficial(pasta_caso)

  secao <- sessionInfo()
  entrada <- le_arq_entrada(file.path(pasta_caso, "Arq_Entrada"))
  saida <- le_previsao(file.path(pasta_caso, "Arq_Saida"), entrada$previsao_precipitacao[, unique(cenario)], 
                       entrada$caso$nome_subbacia[1], entrada$datas_rodadas$data)

  if (secao$R.version$os == "linux-gnu") {
    expect_equal(round(saida[, valor][10], 0), 3493)
  } else {
    expect_true(abs(round(saida[, valor][10], 0) - 3493) < 3493 * 0.01)
  }
  
  #CN12.2
  pasta_caso <- system.file("extdata", package = "smapOnsR")
  expect_error(executa_caso_oficial(pasta_caso))

  #CN12.1
  pasta_caso <- system.file("extdata", "Arq_Entrada", package = "smapOnsR")
  expect_error(executa_caso_oficial(pasta_caso))
})

test_that("testa execucao novo formato", {
  pasta_entrada <- system.file("extdata", "arq_entrada_novo", package = "smapOnsR")

  saida <- suppressWarnings(executa_caso_novo(pasta_entrada))

  secao <- sessionInfo()
  
  if (secao$R.version$os == "mingw32") {
#    expect_equal(round(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "historico", valor][27], 0), 732)
  } else {
    expect_true(abs(round(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "historico", valor][27], 0) - 732) < 732 * 0.01)
    
  }

  unlink(system.file("extdata", "Arq_Entrada", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "arq_entrada_novo_sem_etp", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "Arq_Entrada0", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "Arq_Entrada1", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "caso_completo", package = "smapOnsR"), recursive = TRUE)
  unlink(system.file("extdata", "caso_completo2", package = "smapOnsR"), recursive = TRUE)
})
