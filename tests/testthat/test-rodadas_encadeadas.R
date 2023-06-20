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

    inicializacao <- data.table::data.table(nome = c("avermelha", "ssimao2"), Ebin = c(218.71, 441.67), Supin = c(46.69, 256.98), Tuin = c(0.2891, 0.3141))
    
    saida <- rodada_encadeada_oficial(parametros[Nome %in% sub_bacias],
    inicializacao, precipitacao, previsao_precipitacao, evapotranspiracao, vazao,
    postos_plu, datas_rodadas, numero_dias_assimilacao, numero_cenarios, sub_bacias)

    expect_equal(saida[data_previsao == "2020-05-05" & cenario == "cenario2", valor], 
                saida[data_previsao == "2020-05-05" & cenario == "historico", valor])
})