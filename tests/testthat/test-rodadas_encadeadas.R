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
    
    saida <- rodada_encadeada_oficial(parametros[nome %in% sub_bacias],
    inicializacao, precipitacao, previsao_precipitacao, evapotranspiracao, vazao,
    postos_plu, datas_rodadas, numero_cenarios, sub_bacias)

    expect_equal(saida$previsao[data_previsao == "2020-05-05" & cenario == "cenario2", valor],
                saida$previsao[data_previsao == "2020-05-05" & cenario == "historico", valor])
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
  #    evapotranspiracao_nc <-entrada$evapotranspiracao
  #      vazao_observada <- entrada$vazao
  #      postos_plu <- entrada$postos_plu
  #      datas_rodadas <- entrada$datas_rodadas
  #    numero_cenarios <- length(unique(entrada$previsao_precipitacao[, cenario]))
  #    sub_bacias <- entrada$caso$nome_subbacia

  set.seed(129852)
  saida <- rodada_encadeada_oficial(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia)
  
  secao <- sessionInfo()

  if (secao$R.version$os == "mingw32") {
    expect_equal(round(saida$previsao[nome == "pimentalt" & variavel == "Qcalc" & cenario == "ecmwf_1", valor][31], 0), 10165)
  } else {
    expect_true(abs(round(saida$previsao[nome == "pimentalt" & variavel == "Qcalc" & cenario == "ecmwf_1", valor][31], 0) - 10165) < 10165 * 0.01)
  }
})

test_that("testa rodada ecmwf formato oficial", {
  pasta_entrada <- system.file("extdata", "Arq_Entrada1", package = "smapOnsR")

  entrada <- le_arq_entrada(pasta_entrada)

  set.seed(129852)
    saida <- rodada_encadeada_oficial(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia)
    
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
      entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia)
    
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

    inicializacao <- data.table::data.table(nome = c(rep("avermelha", 4), rep("ssimao2", 4)), variavel = rep(c("Ebin", "Supin", "Tuin", "numero_dias_assimilacao"),2), 
    valor = c(218.71, 46.69, 0.2891, 31, 441.67, 256.98, 0.3141, 31))
    
    saida <- rodada_encadeada_etp(parametros[nome %in% sub_bacias],
    inicializacao, precipitacao_observada, precipitacao_prevista, evapotranspiracao_observada, evapotranspiracao_prevista, vazao_observada,
    postos_plu, datas_rodadas, numero_cenarios, sub_bacias)

    expect_equal(saida$previsao[data_previsao == "2020-05-05" & cenario == "cenario2", valor],
                saida$previsao[data_previsao == "2020-05-05" & cenario == "historico", valor])
})

test_that("testa caso pegando a assimilacao original do aplicativo",{
    pasta_entrada <- system.file("extdata", "caso_completo", "Arq_Entrada", package = "smapOnsR")
    pasta_saida <- system.file("extdata", "caso_completo", "Arq_Saida", package = "smapOnsR")

    entrada <- le_arq_entrada(pasta_entrada)
    execucao <- le_execucao(pasta_saida, entrada$datas_rodadas$data)

  testa_rodada_sem_assimilacao <- function(parametros, inicializacao, precipitacao_observada, 
    precipitacao_prevista, evapotranspiracao_nc, vazao_observada, postos_plu, datas_rodadas, 
    numero_cenarios, sub_bacias, execucao) {
      numero_sub_bacias <- length(sub_bacias)
      numero_datas <- nrow(datas_rodadas)
      numero_dias_previsao <- datas_rodadas$numero_dias_previsao
      nome_cenario <- unique(precipitacao_prevista[, cenario])

      saida <- data.table::data.table()
      saida_ajuste_otimizacao <- data.table::data.table()
      saida_ajuste_assimilacao <- data.table::data.table()
      saida_ajuste_fo <- data.table::data.table()
      saida_precipitacao <- data.table::data.table()
      for (ibacia in 1:numero_sub_bacias){
          sub_bacia <- sub_bacias[ibacia]

          EbInic <- inicializacao[nome == sub_bacia & variavel == "Ebin", valor]
          Supin <- inicializacao[nome == sub_bacia & variavel == "Supin", valor]
          TuInic <- inicializacao[nome == sub_bacia & variavel == "Tuin", valor]
          numero_dias_assimilacao <- inicializacao[nome == sub_bacia & variavel == "numero_dias_assimilacao", valor]
          vetor_inicializacao <- array(rep(0, numero_cenarios * 7), c(numero_cenarios, 7))

          modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome %in% sub_bacia])
          kt <- modelo$kt
          kt_max <- parametros[nome == sub_bacia & parametro == "ktMax", valor]
          kt_min <- parametros[nome == sub_bacia & parametro == "ktMin", valor]
          vetor_modelo <- unlist(modelo)
          area <- attributes(modelo)$area

          for (idata in 1:numero_datas){
              saida_bacia_aux <- data.table::data.table()
              
              dataRodada <- datas_rodadas[idata, data]
              numero_dias_previsao <- datas_rodadas[data == dataRodada, numero_dias_previsao]
              matriz_precipitacao <- array(rep(0, numero_cenarios * numero_dias_previsao), c(numero_cenarios, numero_dias_previsao))
              matriz_evapotranspiracao <- array(rep(0, numero_cenarios * numero_dias_previsao), c(numero_cenarios, numero_dias_previsao))
              matriz_evapotranspiracao_planicie <- array(rep(0, numero_cenarios * numero_dias_previsao), c(numero_cenarios, numero_dias_previsao))

              vazao <- vazao_observada[data < dataRodada & data >= (dataRodada - numero_dias_assimilacao) 
                            & posto == sub_bacia, valor]
                            
              normal_climatologica <- evapotranspiracao_nc[nome == sub_bacia]

              precipitacao <- data.table::data.table(precipitacao_observada[nome == sub_bacia &
              data <= dataRodada & data >= (dataRodada - numero_dias_assimilacao - kt_min)])
              previsao_rodada <- precipitacao_prevista[nome == sub_bacia & data_rodada == dataRodada]

              precipitacao[, data_rodada := dataRodada]
              precipitacao <- combina_observacao_previsao(precipitacao, previsao_rodada)

              precipitacao_assimilacao <- data.table::data.table(precipitacao[data_previsao < (dataRodada + kt_max) & 
                  data_previsao >= (dataRodada - numero_dias_assimilacao - kt_min) & cenario == unique(cenario)[1]])
              colnames(precipitacao_assimilacao)[1] <- "data"
              precipitacao_assimilacao[, cenario := NULL]
              precipitacao_assimilacao[, data_rodada := NULL]

              evapotranspiracao <- transforma_NC_serie(precipitacao_assimilacao[data < dataRodada & data >= (dataRodada - numero_dias_assimilacao)], normal_climatologica) 
              evapotranspiracao_planicie <- evapotranspiracao[, valor] * vetor_modelo[77]
              evapotranspiracao <- evapotranspiracao[, valor] * vetor_modelo[76]

              EbInic <- execucao[subbacia == sub_bacia & dia_assimilacao == 1, ebin]
              Supin <- execucao[subbacia == sub_bacia & dia_assimilacao == 1, supin]
              if (Supin < 0) { #L-BFGS-B as vezes fornece valor negativo próximo a 0 ('-1e-17')
                Supin <- 0
              }
              inicializacao2 <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
              vetor_inicializacao <- unlist(inicializacao2)
              precipitacao_ponderada <- data.table::data.table(precipitacao_assimilacao)
              precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
              precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt,
                                                              kt_max, kt_min)
              precipitacao_ponderada <- precipitacao_ponderada * execucao[subbacia == sub_bacia, peso_chuva]

              simulacao <- funcaoSmapCpp::rodada_varios_dias_cpp2(vetor_modelo,
                        vetor_inicializacao, area, precipitacao_ponderada,
                        evapotranspiracao, evapotranspiracao_planicie, numero_dias_assimilacao)

              simulacao <- data.table::data.table(simulacao)

              ajuste <- list(ajuste = execucao, simulacao = simulacao)

              saida_precipitacao <- data.table::rbindlist(list(saida_precipitacao, precipitacao))

              precipitacao[, valor := valor * vetor_modelo[75]]
              for (icenario in 1:length(unique(precipitacao[, cenario]))){
                  matriz_precipitacao[icenario,] <- ponderacao_temporal(precipitacao[data_previsao < (dataRodada + numero_dias_previsao + kt_max) & data_rodada == dataRodada & 
                  data_previsao >= (dataRodada - kt_min) & cenario == nome_cenario[icenario], valor], kt, kt_max, kt_min)
              }
              
              colnames(precipitacao)[1] <- "data"
              precipitacao <- precipitacao[cenario == unique(cenario)[1]]
              precipitacao[, data_rodada := NULL]
              precipitacao[, cenario := NULL]
              evapotranspiracao <- transforma_NC_serie(precipitacao[data <= dataRodada + numero_dias_previsao - 1 & data >= dataRodada], normal_climatologica)
              for (icenario in 1:numero_cenarios){
                  matriz_evapotranspiracao_planicie[icenario,] <- evapotranspiracao[, valor] * vetor_modelo[77]
                  matriz_evapotranspiracao[icenario,] <- evapotranspiracao[, valor] * vetor_modelo[76]
              }

              vetor_inicializacao <- array(rep(0, numero_cenarios * 7), c(numero_cenarios, 7))
              vetor_inicializacao[, 4] <- ajuste$simulacao[numero_dias_assimilacao, Rsup2]
              vetor_inicializacao[, 5] <- ajuste$simulacao[numero_dias_assimilacao, Rsolo]
              vetor_inicializacao[, 6] <- ajuste$simulacao[numero_dias_assimilacao, Rsup]
              vetor_inicializacao[, 7] <- ajuste$simulacao[numero_dias_assimilacao, Rsub]
              
              simulacao <- funcaoSmapCpp::rodada_cenarios_dias_cpp2(vetor_modelo,
              vetor_inicializacao, area, matriz_precipitacao,
              matriz_evapotranspiracao, matriz_evapotranspiracao_planicie, numero_dias_previsao, numero_cenarios)

              saida_bacia_aux <- data.table::data.table(do.call(rbind, simulacao))

              saida_bacia_aux[, nome := sub_bacia]
              saida_bacia_aux[, data_caso := dataRodada]
              saida_bacia_aux[, cenario := rep(nome_cenario, each = numero_dias_previsao)]
              saida_bacia_aux[, data_previsao := rep(seq.Date(dataRodada, dataRodada + numero_dias_previsao - 1, by = 1), numero_cenarios)]
              saida <- data.table::rbindlist(list(saida, saida_bacia_aux))
          }
      }
      saida <- melt(saida, id.vars = c("data_caso", "data_previsao", "cenario", "nome"), variable.name = "variavel",
            value.name = "valor")
      saida <- list(previsao = saida)
      saida
    }

    saida <- testa_rodada_sem_assimilacao(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia, execucao)
    previsao <- saida$previsao[variavel == "Qcalc"]
    previsao_original <- le_previsao(pasta_saida, entrada$datas_rodadas$data)
    previsao[, dif := valor - previsao_original[, valor]]

    expect_true(previsao[, max(abs(dif))] < 0.01)

    unlink(system.file("extdata", "Arq_Entrada", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "arq_entrada_novo_sem_etp", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "Arq_Entrada0", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "Arq_Entrada1", package = "smapOnsR"), recursive = TRUE)
    unlink(system.file("extdata", "caso_completo", package = "smapOnsR"), recursive = TRUE)
})

#test_that("testa rodada com serie temporal etp", {
#    pasta_entrada <- system.file("extdata", "Arq_entrada_novo", package = "smapOnsR")
#
#    entrada <- le_arq_entrada_novo(pasta_entrada)
#    entrada$datas_rodadas <- entrada$datas_rodadas[1:3]
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
#})
