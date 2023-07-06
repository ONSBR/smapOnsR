test_that("testa rodadas encadeadas", {
    numero_dias_assimilacao <- 31
    
    datas_rodadas<- data.table::data.table(data = as.Date(c("2020-05-01", "2020-05-08")), numero_dias_previsao = c(15, 20))

    sub_bacias <- c("avermelha", "ssimao2")
    
    precipitacao <- historico_precipitacao[posto %in% postos_plu[nome %in% sub_bacias, posto]]

    precipitacao <- data.table::rbindlist(lapply(sub_bacias, function(sub_bacia) {
  ponderacao_espacial(precipitacao, postos_plu[nome %in% sub_bacia])
        }))
    previsao_precipitacao <- transforma_historico_previsao(precipitacao, datas_rodadas)
    aux <- data.table::data.table(previsao_precipitacao)
    aux[, cenario := "cenario2"]
    previsao_precipitacao <- rbind(aux, previsao_precipitacao)

    numero_cenarios <- length(unique(previsao_precipitacao[, cenario]))
    vazao <- historico_vazao[posto %in% sub_bacias]
    evapotranspiracao <- historico_etp_NC[nome %in% sub_bacias]

    inicializacao <- data.table::data.table(nome = c(rep("avermelha", 4), rep("ssimao2", 4)), variavel = rep(c("Ebin", "Supin", "Tuin", "numero_dias_assimilacao"),2), 
    valor = c(218.71, 46.69, 0.2891, 31, 441.67, 256.98, 0.3141, 31))
    
    saida <- rodada_encadeada_oficial(parametros[Nome %in% sub_bacias],
    inicializacao, precipitacao, previsao_precipitacao, evapotranspiracao, vazao,
    postos_plu, datas_rodadas, numero_cenarios, sub_bacias)

    expect_equal(saida$previsao[data_previsao == "2020-05-05" & cenario == "cenario2", valor],
                saida$previsao[data_previsao == "2020-05-05" & cenario == "historico", valor])
})

test_that("testa rodada ecmwf", {
  pasta_entrada <- system.file("extdata", "Arq_entrada", package = "smapOnsR")
  #pasta_entrada <- "inst//extdata//Arq_Entrada"

  entrada <- le_arq_entrada(pasta_entrada)

  #parametros <- entrada$parametros
  #inicializacao <- entrada$inicializacao
  #historico_precipitacao <- entrada$precipitacao
  #    previsao_precipitacao <- entrada$previsao_precipitacao
  #    historico_etp_NC <-entrada$evapotranspiracao
  #      historico_vazao <- entrada$vazao
  #      postos_plu <- entrada$postos_plu
  #      datas_rodadas <- entrada$datas_rodadas
  #    numero_cenarios <- length(unique(entrada$previsao_precipitacao[, cenario]))
  #    sub_bacias <- entrada$caso$nome_subbacia

  set.seed(129852)
  saida <- rodada_encadeada_oficial(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia)
    
  expect_equal(saida$previsao[nome == "pimentalt" & variavel == "Qcalc" & cenario == "ecmwf_1", valor][31], 10164.954)
})

test_that("testa rodada ecmwf formato oficial", {
  pasta_entrada <- system.file("extdata", "Arq_entrada1", package = "smapOnsR")

  entrada <- le_arq_entrada(pasta_entrada)

  set.seed(129852)
    saida <- rodada_encadeada_oficial(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia)
    
  expect_equal(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "ecmwf_ex42", valor][27], 202.10048)
})

test_that("testa rodada oficial", {
  pasta_entrada <- system.file("extdata", "Arq_entrada0", package = "smapOnsR")

  entrada <- le_arq_entrada(pasta_entrada)

  set.seed(129852)
  saida <- rodada_encadeada_oficial(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia)
    
  expect_equal(saida$previsao[nome == "ssimao2" & variavel == "Qcalc", valor][17], 1210.44864)
})

test_that("testa rodada com serie temporal etp", {
    numero_dias_assimilacao <- 31
    
    datas_rodadas<- data.table::data.table(data = as.Date(c("2020-05-01", "2020-05-08")), numero_dias_previsao = c(15, 20))

    sub_bacias <- c("avermelha", "ssimao2")
    
    precipitacao_observada <- historico_precipitacao[posto %in% postos_plu[nome %in% sub_bacias, posto]]

    precipitacao_observada <- data.table::rbindlist(lapply(sub_bacias, function(sub_bacia) {
  ponderacao_espacial(precipitacao_observada, postos_plu[nome %in% sub_bacia])
        }))
    precipitacao_prevista <- transforma_historico_previsao(precipitacao_observada, datas_rodadas)
    aux <- data.table::data.table(precipitacao_prevista)
    aux[, cenario := "cenario2"]
    precipitacao_prevista <- rbind(aux, precipitacao_prevista)

    numero_cenarios <- length(unique(precipitacao_prevista[, cenario]))
    vazao_observada <- historico_vazao[posto %in% sub_bacias]

    evapotranspiracao_observada <- historico_etp[posto %in% sub_bacias]
    colnames(evapotranspiracao_observada)[2] <- "nome"
    datas_rodadas$numero_dias_previsao <- datas_rodadas$numero_dias_previsao - 2
    evapotranspiracao_prevista <- transforma_historico_previsao(evapotranspiracao_observada, datas_rodadas)
    aux <- data.table::data.table(evapotranspiracao_prevista)
    aux[, cenario := "cenario2"]
    evapotranspiracao_prevista <- rbind(aux, evapotranspiracao_prevista)

    datas_rodadas$numero_dias_previsao <- datas_rodadas$numero_dias_previsao + 2

    inicializacao <- data.table::data.table(nome = c(rep("avermelha", 4), rep("ssimao2", 4)), variavel = rep(c("Ebin", "Supin", "Tuin", "numero_dias_assimilacao"),2), 
    valor = c(218.71, 46.69, 0.2891, 31, 441.67, 256.98, 0.3141, 31))
    
    saida <- rodada_encadeada_etp(parametros[Nome %in% sub_bacias],
    inicializacao, precipitacao_observada, precipitacao_prevista, evapotranspiracao_observada, evapotranspiracao_prevista, vazao_observada,
    postos_plu, datas_rodadas, numero_cenarios, sub_bacias)

    expect_equal(saida$previsao[data_previsao == "2020-05-05" & cenario == "cenario2", valor],
                saida$previsao[data_previsao == "2020-05-05" & cenario == "historico", valor])
})

