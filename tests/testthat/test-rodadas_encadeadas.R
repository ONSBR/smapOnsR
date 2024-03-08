test_that("testa rodadas encadeadas", {
    #CT25.1
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
    evapotranspiracao_nc <- historico_etp_NC[nome %in% sub_bacias]

    inicializacao <- data.table::data.table(nome = c(rep("avermelha", 9), rep("ssimao2", 9)),
    variavel = rep(c("Ebin", "Supin", "Tuin", "numero_dias_assimilacao", 
    "limite_inferior_ebin", "limite_superior_ebin", "limite_superior_prec", "limite_inferior_prec",
    "ajusta_precipitacao"), 2),
    valor = c(218.71, 46.69, 0.2891, 31, 0.8, 1.2, 2, 0.5, 0, 441.67, 256.98, 0.3141, 31, 0.8, 1.2, 2, 0.5, 0))
    
    saida <- rodada_encadeada_oficial(parametros[nome %in% sub_bacias],
    inicializacao, precipitacao_observada, precipitacao_prevista, evapotranspiracao_nc, vazao_observada,
    postos_plu, datas_rodadas, sub_bacias)

    expect_equal(saida$previsao[data_previsao == "2020-05-05" & cenario == "cenario2", valor],
                saida$previsao[data_previsao == "2020-05-05" & cenario == "historico", valor])

    #CT25.2
    Ebin_segundo_caso <- saida$otimizacao[nome == "ssimao2" & data_caso == "2020-05-08" &
              variavel == "Ebin", limite_inferior / 0.8]
    
    data_assimilacao_seguinte <- as.Date("2020-05-08") - 32
    Ebin_assimilacao_primeiro_caso <- saida$assimilacao[nome == "ssimao2" & data_caso == "2020-05-01" & variavel == "Qbase" & 
                      data_assimilacao == data_assimilacao_seguinte, valor]
    expect_equal(Ebin_segundo_caso, Ebin_assimilacao_primeiro_caso)
})

test_that("testa rodada ecmwf", {
  zip::unzip(system.file("extdata", "dados_entrada.zip", package = "smapOnsR"), exdir = system.file("extdata", package = "smapOnsR"))

  pasta_entrada <- system.file("extdata", "Arq_Entrada", package = "smapOnsR")
  #pasta_entrada <- "inst//extdata//Arq_Entrada"

  entrada <- le_arq_entrada(pasta_entrada)

  #parametros <- entrada$parametros
  #inicializacao <- entrada$inicializacao
  #precipitacao_observada <- entrada$precipitacao
  #    precipitacao_prevista <- entrada$previsao_precipitacao
  #    evapotranspiracao_nc <- entrada$evapotranspiracao
  #      vazao_observada <- entrada$vazao
  #      postos_plu <- entrada$postos_plu
  #      datas_rodadas <- entrada$datas_rodadas
  #    numero_cenarios <- length(unique(entrada$previsao_precipitacao[, cenario]))
  #    sub_bacias <- entrada$caso$nome_subbacia

  set.seed(129852)
  saida <- rodada_encadeada_oficial(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, entrada$caso$nome_subbacia)
  
  secao <- sessionInfo()

  if (secao$R.version$os == "mingw32") {
    expect_equal(round(saida$previsao[nome == "pimentalt" & variavel == "Qcalc" & cenario == "ecmwf_1", valor][31], 0), 10166)
  } else {
    expect_true(abs(round(saida$previsao[nome == "pimentalt" & variavel == "Qcalc" & cenario == "ecmwf_1", valor][31], 0) - 10166) < 10166 * 0.01)
  }

  entrada$inicializacao[variavel == "ajusta_precipitacao", valor := 1]

  set.seed(129852)
  saida <- rodada_encadeada_oficial(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, entrada$caso$nome_subbacia)
  
  secao <- sessionInfo()

  if (secao$R.version$os == "mingw32") {
    expect_equal(round(saida$previsao[nome == "tucurui" & variavel == "Qcalc" & cenario == "ecmwf_1", valor][1], 0), 2211)
  } else {
    expect_true(abs(round(saida$previsao[nome == "tucurui" & variavel == "Qcalc" & cenario == "ecmwf_1", valor][1], 0) - 2211) < 2211 * 0.01)
  }
})

test_that("testa rodada ecmwf formato oficial", {
  pasta_entrada <- system.file("extdata", "Arq_Entrada1", package = "smapOnsR")

  entrada <- le_arq_entrada(pasta_entrada)

  set.seed(129852)
    saida <- rodada_encadeada_oficial(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, entrada$caso$nome_subbacia)
    
  secao <- sessionInfo()
  
  if (secao$R.version$os == "mingw32") {
    expect_equal(round(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "ecmwf_ex42", valor][27], 0), 202)
  } else {
    expect_true(abs(round(saida$previsao[nome == "avermelha" & variavel == "Qcalc" & cenario == "ecmwf_ex42", valor][27], 0) - 202) < 202 * 0.01)
  }
})

test_that("testa rodada oficial", {
  pasta_entrada <- system.file("extdata", "Arq_Entrada0", package = "smapOnsR")

  entrada <- le_arq_entrada(pasta_entrada)

  set.seed(129852)
  saida <- rodada_encadeada_oficial(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, entrada$caso$nome_subbacia)
    
  secao <- sessionInfo()
  
  if (secao$R.version$os == "mingw32") {
    expect_equal(round(saida$previsao[nome == "ssimao2" & variavel == "Qcalc", valor][17], 0), 1208)
  } else {
    expect_true(abs(round(saida$previsao[nome == "ssimao2" & variavel == "Qcalc", valor][17], 0) - 1208) < 1208 * 0.01)
  }
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
    datas_rodadas$numero_dias_previsao <- datas_rodadas$numero_dias_previsao - 2
    evapotranspiracao_prevista <- data.table::copy(evapotranspiracao_observada)
    colnames(evapotranspiracao_prevista)[2] <- "nome"
    evapotranspiracao_prevista <- transforma_historico_previsao(evapotranspiracao_prevista, datas_rodadas)
    aux <- data.table::data.table(evapotranspiracao_prevista)
    aux[, cenario := "cenario2"]
    evapotranspiracao_prevista <- rbind(aux, evapotranspiracao_prevista)

    datas_rodadas$numero_dias_previsao <- datas_rodadas$numero_dias_previsao + 2

    inicializacao <- data.table::data.table(nome = c(rep("avermelha", 9), rep("ssimao2", 9)),
    variavel = rep(c("Ebin", "Supin", "Tuin", "numero_dias_assimilacao",
    "limite_inferior_ebin", "limite_superior_ebin", "limite_superior_prec", "limite_inferior_prec",
    "ajusta_precipitacao"), 2),
    valor = c(218.71, 46.69, 0.2891, 31, 0.8, 1.2, 2, 0.5, 0, 441.67, 256.98, 0.3141, 31, 0.8, 1.2, 2, 0.5, 0))
    
    saida <- rodada_encadeada_etp(parametros[nome %in% sub_bacias],
    inicializacao, precipitacao_observada, precipitacao_prevista, evapotranspiracao_observada, evapotranspiracao_prevista, vazao_observada,
    postos_plu, datas_rodadas, sub_bacias)

    expect_equal(saida$previsao[data_previsao == "2020-05-05" & cenario == "cenario2", valor],
                saida$previsao[data_previsao == "2020-05-05" & cenario == "historico", valor])
})

test_that("testa caso pegando a assimilacao original do aplicativo",{
    pasta_entrada <- system.file("extdata", "caso_completo", "Arq_Entrada", package = "smapOnsR")
    pasta_saida <- system.file("extdata", "caso_completo", "Arq_Saida", package = "smapOnsR")

    entrada <- le_arq_entrada(pasta_entrada)
    execucao <- le_execucao(pasta_saida, entrada$datas_rodadas$data)

    saida <- rodada_sem_assimilacao(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia, execucao)
    previsao <- saida$previsao[variavel == "Qcalc"]
    previsao_original <- le_previsao_2(pasta_saida, entrada$datas_rodadas$data)
    previsao[, dif := valor - previsao_original[, valor]]

    expect_true(previsao[, max(abs(dif))] < 0.01)

    unlink(system.file("extdata", "Arq_Entrada", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "arq_entrada_novo_sem_etp", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "Arq_Entrada0", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "Arq_Entrada1", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "caso_completo", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "caso_completo2", package = "smapOnsR"), recursive = TRUE)
})

#test_that("testa rodada com serie temporal etp", {
#    pasta_entrada <- system.file("extdata", "arq_entrada_novo", package = "smapOnsR")
#
#    entrada <- le_arq_entrada_novo(pasta_entrada)
#    
#    sub_bacias <- c("smesa", "avermelha")
#
#    set.seed(129852)
#    saida <- rodada_encadeada_etp(entrada$parametros,
#    entrada$inicializacao, entrada$precipitacao_observada, entrada$precipitacao_prevista, entrada$evapotranspiracao_observada, 
#    entrada$evapotranspiracao_prevista, entrada$vazao_observada,
#    entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$precipitacao_prevista[, cenario])), sub_bacias)
#
#    expect_equal(round(saida$previsao[data_caso == "2011-01-15" & data_previsao == "2011-02-16"
#     & cenario == "historico" & variavel == "Qcalc" & nome == "smesa", valor], 0), 1054)
#  
#  secao <- sessionInfo()
#  
#  if (secao$R.version$os == "mingw32") {
#    expect_equal(round(saida$previsao[nome == "ssimao2" & variavel == "Qcalc", valor][17], 0), 1210)
#  } else {
#    expect_true(abs(round(saida$previsao[nome == "ssimao2" & variavel == "Qcalc", valor][17], 0) - 1210) < 1210 * 0.01)
#  }
#})