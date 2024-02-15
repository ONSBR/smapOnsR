#' Funcao para realizar rodadas encadeadas do SMAP/ONS
#' 
#' Realiza execucoes encadeadas do modelo SMAP/ONS
#'
#' @param parametros data table com os parametros dos modelos
#'     \itemize{
#'     \item{nome - nome da sub-bacia}
#'     \item{parametro - nome do parametro}
#'     \item{valor - valor do parametro}
#'     }
#' @param inicializacao data.table com a inicializacao com as colunas:
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{variavel - vazao de base inicial}
#'     \item{valor - valor da variavel}
#'     }
#' @param precipitacao_observada data table com o historico de precipitacao com as colunas:
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param precipitacao_prevista data.table com a previsao de precipitacao com as colunas:
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da previsao}
#'     }
#' @param evapotranspiracao_nc data.table com o historico de NC deevapotranspiracao com as colunas:
#'     \itemize{
#'     \item{mes - mes da NC}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da NC de evapotranspiracao observada}
#'     }
#' @param vazao_observada data table com o historico de vazao com as colunas:
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param postos_plu data table contendo a relacao sub-bacia x postos_plu com as colunas:
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{posto - nome do posto plu}
#'     \item{valor - peso do posto plu}
#'     }
#' @param datas_rodadas data table contendo as datas dos casos a serem executados e seus respectivos horizontes:
#'     \itemize{
#'     \item{data - data do caso}
#'     \item{numero_dias_previsao - horizonte do caso}
#'     }
#' @param numero_cenarios numero de cenarios a serem gerados
#' @param sub_bacias vetor com o nome das sub-bacias a serem consideradas
#' @importFrom data.table data.table
#' @return saida lista com o data.table previsao contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#'  otimizacao data table com as colunas:
#' \itemize{
#'     \item{otimizacao - valor das variaveis da otimizacao}
#'     \item{nome - nome da sub-bacia}
#'     \item{data_caso - data do caso}
#'     }
#'  assimilacao data table com as colunas:
#' \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_assimilacao - data da assimilacao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' precipitacao data table contendo precipitacao observada e previsata com as colunas:
#' \itemize{
#'     \item{data_previsao - data da precipitacao}
#'     \item{data_rodada - data da rodada}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' @examples 
#' \dontrun{
#' pasta_entrada <- system.file("extdata", "Arq_Entrada0", package = "smapOnsR")
#' 
#' entrada <- le_arq_entrada(pasta_entrada)
#' 
#' saida <- rodada_encadeada_oficial(entrada$parametros,
#' entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
#' entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia)
#' }
#' @export
rodada_encadeada_oficial <- function(parametros, inicializacao, precipitacao_observada, 
    precipitacao_prevista, evapotranspiracao_nc, vazao_observada, postos_plu, datas_rodadas, 
    numero_cenarios, sub_bacias) {

    numero_sub_bacias <- length(sub_bacias)
    numero_datas <- nrow(datas_rodadas)
    numero_dias_previsao <- datas_rodadas$numero_dias_previsao
    nome_cenario <- unique(precipitacao_prevista[, cenario])

    saida <- data.table::data.table()
    saida_ajuste_otimizacao <- data.table::data.table()
    saida_ajuste_assimilacao <- data.table::data.table()
    saida_ajuste_fo <- data.table::data.table()
    saida_precipitacao <- data.table::data.table()

    data.table::setorder(precipitacao_prevista, "data_rodada", "nome", "cenario", "data_previsao")
    for (ibacia in 1:numero_sub_bacias){
        sub_bacia <- sub_bacias[ibacia]

        EbInic <- inicializacao[nome == sub_bacia & variavel == "Ebin", valor]
        Supin <- inicializacao[nome == sub_bacia & variavel == "Supin", valor]
        TuInic <- inicializacao[nome == sub_bacia & variavel == "Tuin", valor]
        limite_inferior_ebin <- inicializacao[nome == sub_bacia & variavel == "limite_inferior_ebin", valor]
        limite_superior_ebin <- inicializacao[nome == sub_bacia & variavel == "limite_superior_ebin", valor]
        limite_inferior_prec <- inicializacao[nome == sub_bacia & variavel == "limite_inferior_prec", valor]
        limite_superior_prec <- inicializacao[nome == sub_bacia & variavel == "limite_superior_prec", valor]
        numero_dias_assimilacao <- inicializacao[nome == sub_bacia & variavel == "numero_dias_assimilacao", valor]
        
        if (nrow(inicializacao[nome == sub_bacia & variavel == "funcao_objetivo"]) > 0) {
            if (inicializacao[nome == sub_bacia & variavel == "funcao_objetivo", valor] == 0) {
                funcao_objetivo <- calcula_dm
                fnscale <- 1
            } else if (inicializacao[nome == sub_bacia & variavel == "funcao_objetivo", valor] == 1){
                funcao_objetivo <- calcula_nse
                fnscale <- -1
            } else if (inicializacao[nome == sub_bacia & variavel == "funcao_objetivo", valor] == 2){
                funcao_objetivo <- calcula_mape
                fnscale <- 1
            }
        } else {
            funcao_objetivo <- calcula_dm
            fnscale <- 1
        }

        ajusta_precipitacao <- inicializacao[nome == sub_bacia & variavel == "ajusta_precipitacao", valor]

        vetor_inicializacao <- array(rep(0, numero_cenarios * 7), c(numero_cenarios, 7))

        modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome %in% sub_bacia])
        kt <- modelo$kt
        kt_max <- sum(modelo$kt[1:2] > 0)
        kt_min <- sum(modelo$kt[4:63] > 0)
    
        vetor_modelo <- unlist(modelo)
        area <- attributes(modelo)$area

        for (idata in 1:numero_datas){
            saida_bacia_aux <- data.table::data.table()
            
            dataRodada <- datas_rodadas[idata, data]
            numero_dias_previsao <- datas_rodadas[data == dataRodada, numero_dias_previsao]
            matriz_precipitacao <- array(rep(0, numero_cenarios * (numero_dias_previsao + numero_dias_assimilacao)), c(numero_cenarios, (numero_dias_previsao + numero_dias_assimilacao)))
            matriz_evapotranspiracao <- array(rep(0, numero_cenarios * (numero_dias_previsao + numero_dias_assimilacao)), c(numero_cenarios, (numero_dias_previsao + numero_dias_assimilacao)))
            matriz_evapotranspiracao_planicie <- array(rep(0, numero_cenarios * (numero_dias_previsao + numero_dias_assimilacao)), c(numero_cenarios, (numero_dias_previsao + numero_dias_assimilacao)))

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

            ajuste <- assimilacao_oficial(vetor_modelo, area, EbInic, TuInic, Supin, precipitacao_assimilacao,
                        evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias_assimilacao,
                        ajusta_precipitacao, limite_prec = c(limite_inferior_prec, limite_superior_prec),
                        limite_ebin = c(limite_inferior_ebin, limite_superior_ebin), funcao_objetivo = funcao_objetivo, fnscale = fnscale)
            
            ajuste$simulacao[, data_assimilacao := seq.Date((dataRodada - numero_dias_assimilacao), dataRodada - 1, 1)]
            ajuste$simulacao[, nome := sub_bacia]
            ajuste$simulacao[, data_caso := dataRodada]

            saida_ajuste_otimizacao_aux <- ajuste$otimizacao
            saida_ajuste_otimizacao_aux[, nome := sub_bacia]
            saida_ajuste_otimizacao_aux[, data_caso := dataRodada]

            saida_ajuste_fo_aux <- data.table::data.table(ajuste$ajuste$value)
            colnames(saida_ajuste_fo_aux) <- "funcao_objetivo"
            saida_ajuste_fo_aux[, nome := sub_bacia]
            saida_ajuste_fo_aux[, data_caso := dataRodada]

            saida_precipitacao <- data.table::rbindlist(list(saida_precipitacao, precipitacao))
            
            precipitacao[, valor := valor * vetor_modelo[75]]
            matriz_precipitacao <- array(precipitacao[data_previsao < (dataRodada + kt_max - 1) & data_rodada == dataRodada & 
                data_previsao >= (dataRodada - numero_dias_assimilacao - kt_min), valor], c(numero_dias_assimilacao + kt_max + kt_min - 1, numero_cenarios))
            matriz_precipitacao <- t(ponderacao_temporal(matriz_precipitacao, kt, kt_max, kt_min))
            if (numero_cenarios == 1) {
                matriz_precipitacao[, 1:(numero_dias_assimilacao - 1)] <- matriz_precipitacao[, 1:(numero_dias_assimilacao - 1)] * ajuste$ajuste$par[1:(numero_dias_assimilacao - 1)]
            } else {
                matriz_precipitacao[, 1:(numero_dias_assimilacao - 1)] <- sweep(matriz_precipitacao[, 1:(numero_dias_assimilacao - 1)], 2, ajuste$ajuste$par[1:(numero_dias_assimilacao - 1)], `*`)
            }

            if (ajusta_precipitacao == 1) {
                precipitacao[data_previsao <= dataRodada, valor := valor * ajuste$ajuste$par[numero_dias_assimilacao - 1]]
            }
            matriz_precipitacao_previsao <- array(precipitacao[data_previsao < (dataRodada + numero_dias_previsao + kt_max) & data_rodada == dataRodada & 
                data_previsao >= (dataRodada - kt_min - 1), valor], c(numero_dias_previsao + kt_max + kt_min + 1, numero_cenarios))
            matriz_precipitacao_previsao <- t(ponderacao_temporal(matriz_precipitacao_previsao, kt, kt_max, kt_min))

            matriz_precipitacao <- cbind(matriz_precipitacao, matriz_precipitacao_previsao)
                      
            colnames(precipitacao)[1] <- "data"
            precipitacao <- precipitacao[cenario == unique(cenario)[1]]
            precipitacao[, data_rodada := NULL]
            precipitacao[, cenario := NULL]
            evapotranspiracao <- transforma_NC_serie(precipitacao[data <= dataRodada + numero_dias_previsao - 1 & data >= (dataRodada - numero_dias_assimilacao)], normal_climatologica)
            matriz_evapotranspiracao_planicie <- matrix(evapotranspiracao[, valor] * vetor_modelo[77], nrow = numero_cenarios, ncol = nrow(evapotranspiracao), byrow = TRUE)
            matriz_evapotranspiracao <- matrix(evapotranspiracao[, valor] * vetor_modelo[76], nrow = numero_cenarios, ncol = nrow(evapotranspiracao), byrow = TRUE)

            EbInic <- ajuste$ajuste$par[numero_dias_assimilacao + 1]
            Supin <-  ajuste$ajuste$par[numero_dias_assimilacao + 2]
            if (Supin < 0) { #L-BFGS-B as vezes fornece valor negativo próximo a 0 ('-1e-17')
                Supin <- 0
            }
            inicializacao_caso <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
            inicializacao_caso <- unlist(inicializacao_caso)

            vetor_inicializacao[, 4] <- inicializacao_caso[4]
            vetor_inicializacao[, 5] <- inicializacao_caso[5]
            vetor_inicializacao[, 6] <- inicializacao_caso[6]
            vetor_inicializacao[, 7] <- inicializacao_caso[7]
            
            simulacao <- funcaoSmapCpp::rodada_cenarios_dias_cpp2(vetor_modelo,
            vetor_inicializacao, area, matriz_precipitacao,
            matriz_evapotranspiracao, matriz_evapotranspiracao_planicie, (numero_dias_previsao + numero_dias_assimilacao), numero_cenarios)

            saida_bacia_aux <- data.table::data.table(do.call(rbind, simulacao))

            saida_bacia_aux[, nome := sub_bacia]
            saida_bacia_aux[, data_caso := dataRodada]
            saida_bacia_aux[, cenario := rep(nome_cenario, each = (numero_dias_previsao + numero_dias_assimilacao))]
            saida_bacia_aux[, data_previsao := rep(seq.Date(dataRodada - numero_dias_assimilacao, dataRodada + numero_dias_previsao - 1, by = 1), numero_cenarios)]
            saida_bacia_aux <- saida_bacia_aux[data_previsao >= dataRodada]
            saida <- data.table::rbindlist(list(saida, saida_bacia_aux))

            saida_ajuste_otimizacao <- data.table::rbindlist(list(saida_ajuste_otimizacao, saida_ajuste_otimizacao_aux))
            
            saida_ajuste_assimilacao <- data.table::rbindlist(list(saida_ajuste_assimilacao, ajuste$simulacao))
            
            saida_ajuste_fo <- data.table::rbindlist(list(saida_ajuste_fo, saida_ajuste_fo_aux))

            if (idata < numero_datas) {
                inicio_proxima_assimilacao <- datas_rodadas[idata + 1, data] - numero_dias_assimilacao - 1
                EbInic <- ajuste$simulacao[data_assimilacao == inicio_proxima_assimilacao, Qbase]
                Supin <- ajuste$simulacao[data_assimilacao == inicio_proxima_assimilacao, Qsup1 + Qsup2]
                TuInic <- ajuste$simulacao[data_assimilacao == inicio_proxima_assimilacao, Tu]
            }
        }
    }
    saida <- melt(saida, id.vars = c("data_caso", "data_previsao", "cenario", "nome"), variable.name = "variavel",
           value.name = "valor")
    saida_ajuste_assimilacao <- melt(saida_ajuste_assimilacao, id.vars = c("data_caso", "data_assimilacao", "nome"), variable.name = "variavel",
           value.name = "valor")
    saida <- list(previsao = saida, otimizacao = saida_ajuste_otimizacao, funcao_objetivo = saida_ajuste_fo, assimilacao = saida_ajuste_assimilacao, precipitacao = saida_precipitacao)
    saida
}


#' Funcao para realizar rodadas encadeadas considerando etp variavel do SMAP/ONS
#' 
#' Realiza execucoes encadeadas do modelo SMAP/ONS considerando etp variavel
#'
#' @param parametros data table com os parametros dos modelos
#'     \itemize{
#'     \item{nome - nome da sub-bacia}
#'     \item{parametro - nome do parametro}
#'     \item{valor - valor do parametro}
#'     }
#' @param inicializacao data.table com a inicializacao com as colunas:
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{variavel - vazao de base inicial}
#'     \item{valor - valor da variavel}
#'     }
#' @param precipitacao_observada data table com o historico de precipitacao com as colunas:
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param precipitacao_prevista data.table com a previsao de precipitacao com as colunas:
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da previsao}
#'     }
#' @param evapotranspiracao_observada data.table com o historico de evapotranspiracao com as colunas:
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param evapotranspiracao_prevista data.table com a previsao de evapotranspiracao com as colunas:
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da previsao}
#'     }
#' @param vazao_observada data table com o historico de vazao com as colunas:
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param postos_plu data table contendo a relacao sub-bacia x postos_plu com as colunas:
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{posto - nome do posto plu}
#'     \item{valor - peso do posto plu}
#'     }
#' @param datas_rodadas data table contendo as datas dos casos a serem executados e seus respectivos horizontes:
#'     \itemize{
#'     \item{data - data do caso}
#'     \item{numero_dias_previsao - horizonte do caso}
#'     }
#' @param numero_cenarios numero de cenarios a serem gerados
#' @param sub_bacias vetor com o nome das sub-bacias a serem consideradas
#' @importFrom data.table data.table
#' @return saida lista com o data.table previsao contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#'  otimizacao data table com as colunas:
#' \itemize{
#'     \item{otimizacao - valor das variaveis da otimizacao}
#'     \item{nome - nome da sub-bacia}
#'     \item{data_caso - data do caso}
#'     }
#'  assimilacao data table com as colunas:
#' \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_assimilacao - data da assimilacao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' precipitacao data table contendo precipitacao observada e previsata com as colunas:
#' \itemize{
#'     \item{data_previsao - data da precipitacao}
#'     \item{data_rodada - data da rodada}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' @export
rodada_encadeada_etp <- function(parametros, inicializacao, precipitacao_observada, 
    precipitacao_prevista, evapotranspiracao_observada, evapotranspiracao_prevista, vazao_observada, postos_plu, datas_rodadas, 
    numero_cenarios, sub_bacias) {
    
    numero_sub_bacias <- length(sub_bacias)
    numero_datas <- nrow(datas_rodadas)
    nome_cenario <- unique(precipitacao_prevista[, cenario])

    saida <- data.table::data.table()
    saida_ajuste_otimizacao <- data.table::data.table()
    saida_ajuste_assimilacao <- data.table::data.table()
    saida_precipitacao <- data.table::data.table()
    saida_ajuste_fo <- data.table::data.table()

    data.table::setorder(precipitacao_prevista, "data_rodada", "nome", "cenario", "data_previsao")
    data.table::setorder(evapotranspiracao_prevista, "data_rodada", "nome", "cenario", "data_previsao")
    for (ibacia in 1:numero_sub_bacias){
        sub_bacia <- sub_bacias[ibacia]

        EbInic <- inicializacao[nome == sub_bacia & variavel == "Ebin", valor]
        Supin <- inicializacao[nome == sub_bacia & variavel == "Supin", valor]
        TuInic <- inicializacao[nome == sub_bacia & variavel == "Tuin", valor]
        numero_dias_assimilacao <- inicializacao[nome == sub_bacia & variavel == "numero_dias_assimilacao", valor]
        limite_inferior_ebin <- inicializacao[nome == sub_bacia & variavel == "limite_inferior_ebin", valor]
        limite_superior_ebin <- inicializacao[nome == sub_bacia & variavel == "limite_superior_ebin", valor]
        limite_inferior_prec <- inicializacao[nome == sub_bacia & variavel == "limite_inferior_prec", valor]
        limite_superior_prec <- inicializacao[nome == sub_bacia & variavel == "limite_superior_prec", valor]
        if(nrow(inicializacao[variavel == "funcao_objetivo"]) > 0) {
            if (inicializacao[nome == sub_bacia & variavel == "funcao_objetivo", valor] == 0) {
                funcao_objetivo <- calcula_dm
                fnscale <- 1
            } else if (inicializacao[nome == sub_bacia & variavel == "funcao_objetivo", valor] == 1){
                funcao_objetivo <- calcula_nse
                fnscale <- -1
            } else if (inicializacao[nome == sub_bacia & variavel == "funcao_objetivo", valor] == 2){
                funcao_objetivo <- calcula_mape
                fnscale <- 1
            }
        } else{
            funcao_objetivo <- calcula_dm
            fnscale <- 1
        }

        ajusta_precipitacao <- inicializacao[nome == sub_bacia & variavel == "ajusta_precipitacao", valor]

        vetor_inicializacao <- array(rep(0, numero_cenarios * 7), c(numero_cenarios, 7))

        modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome %in% sub_bacia])
        kt <- modelo$kt
        kt_max <- sum(modelo$kt[1:2] > 0)
        kt_min <- sum(modelo$kt[4:63] > 0)
        vetor_modelo <- unlist(modelo)
        area <- attributes(modelo)$area

        for (idata in 1:numero_datas){
            saida_bacia_aux <- data.table::data.table()
            
            dataRodada <- datas_rodadas[idata, data]
            numero_dias_previsao <- datas_rodadas[data == dataRodada, numero_dias_previsao]
            matriz_precipitacao <- array(rep(0, numero_cenarios * (numero_dias_previsao + numero_dias_assimilacao)), c(numero_cenarios, (numero_dias_previsao + numero_dias_assimilacao)))
            matriz_evapotranspiracao <- array(rep(0, numero_cenarios * (numero_dias_previsao + numero_dias_assimilacao)), c(numero_cenarios, (numero_dias_previsao + numero_dias_assimilacao)))
            matriz_evapotranspiracao_planicie <- array(rep(0, numero_cenarios * (numero_dias_previsao + numero_dias_assimilacao)), c(numero_cenarios, (numero_dias_previsao + numero_dias_assimilacao)))

            vazao <- vazao_observada[data < dataRodada & data >= (dataRodada - numero_dias_assimilacao) 
                          & posto == sub_bacia, valor]
            
            previsao_rodada <- precipitacao_prevista[nome == sub_bacia & data_rodada == dataRodada]
            precipitacao <- data.table::data.table(precipitacao_observada[nome == sub_bacia &
            data <= dataRodada & data >= (dataRodada - numero_dias_assimilacao - kt_min)])
            precipitacao[, data_rodada := dataRodada]
            precipitacao <- combina_observacao_previsao(precipitacao, previsao_rodada)

            previsao_rodada <- precipitacao_prevista[nome == sub_bacia & data_rodada == dataRodada]

            precipitacao_assimilacao <- data.table::data.table(precipitacao[data_previsao < (dataRodada + kt_max) & 
                data_previsao >= (dataRodada - numero_dias_assimilacao - kt_min) & cenario == unique(cenario)[1]])
            colnames(precipitacao_assimilacao)[1] <- "data"
            precipitacao_assimilacao[, cenario := NULL]
            precipitacao_assimilacao[, data_rodada := NULL]

            evapotranspiracao_planicie <- evapotranspiracao_observada[posto == sub_bacia & data < dataRodada & data >= (dataRodada - numero_dias_assimilacao), valor] * vetor_modelo[77]
            evapotranspiracao <- evapotranspiracao_observada[posto == sub_bacia & data < dataRodada & data >= (dataRodada - numero_dias_assimilacao), valor] * vetor_modelo[76]

            ajuste <- assimilacao_evapotranspiracao(vetor_modelo, area, EbInic, TuInic, Supin, precipitacao_assimilacao,
                        evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias_assimilacao, ajusta_precipitacao,
                        limite_prec = c(limite_inferior_prec, limite_superior_prec), limite_etp = c(0.5, 2),
                        limite_ebin = c(limite_inferior_ebin, limite_superior_ebin), limite_supin = c(0, 2),
                        funcao_objetivo = funcao_objetivo, fnscale = fnscale)

            ajuste$simulacao[, data_assimilacao := seq.Date((dataRodada - numero_dias_assimilacao), dataRodada - 1, 1)]
            ajuste$simulacao[, nome := sub_bacia]
            ajuste$simulacao[, data_caso := dataRodada]
            
            saida_ajuste_otimizacao_aux <- ajuste$otimizacao
            saida_ajuste_otimizacao_aux[, nome := sub_bacia]
            saida_ajuste_otimizacao_aux[, data_caso := dataRodada]

            saida_ajuste_fo_aux <- data.table::data.table(ajuste$ajuste$value)
            colnames(saida_ajuste_fo_aux) <- "funcao_objetivo"
            saida_ajuste_fo_aux[, nome := sub_bacia]
            saida_ajuste_fo_aux[, data_caso := dataRodada]

            saida_precipitacao <- data.table::rbindlist(list(saida_precipitacao, precipitacao))

            precipitacao[, valor := valor * vetor_modelo[75]]
            matriz_precipitacao <- array(precipitacao[data_previsao < (dataRodada + kt_max - 1) & data_rodada == dataRodada & 
                data_previsao >= (dataRodada - numero_dias_assimilacao - kt_min), valor], c(numero_dias_assimilacao + kt_max + kt_min - 1, numero_cenarios))
            matriz_precipitacao <- t(ponderacao_temporal(matriz_precipitacao, kt, kt_max, kt_min))
            if (numero_cenarios == 1) {
                matriz_precipitacao[, 1:(numero_dias_assimilacao - 1)] <- matriz_precipitacao[, 1:(numero_dias_assimilacao - 1)] * ajuste$ajuste$par[1:(numero_dias_assimilacao - 1)]
            } else {
                matriz_precipitacao[, 1:(numero_dias_assimilacao - 1)] <- sweep(matriz_precipitacao[, 1:(numero_dias_assimilacao - 1)], 2, ajuste$ajuste$par[1:(numero_dias_assimilacao - 1)], `*`)
            }

            if (ajusta_precipitacao == 1) {
                precipitacao[data_previsao <= dataRodada, valor := valor * ajuste$ajuste$par[numero_dias_assimilacao - 1]]
            }
            matriz_precipitacao_previsao <- array(precipitacao[data_previsao < (dataRodada + numero_dias_previsao + kt_max) & data_rodada == dataRodada & 
                data_previsao >= (dataRodada - kt_min - 1), valor], c(numero_dias_previsao + kt_max + kt_min + 1, numero_cenarios))
            matriz_precipitacao_previsao <- t(ponderacao_temporal(matriz_precipitacao_previsao, kt, kt_max, kt_min))

            matriz_precipitacao <- cbind(matriz_precipitacao, matriz_precipitacao_previsao)
            previsao_evapotranspiracao_rodada <- evapotranspiracao_prevista[nome == sub_bacia & data_rodada == dataRodada & data_previsao < (dataRodada + numero_dias_previsao)]
            evapotranspiracao <- data.table::data.table(evapotranspiracao_observada[posto == sub_bacia &
            data <= dataRodada & data >= (dataRodada - numero_dias_assimilacao)])
            data.table::setnames(evapotranspiracao, "posto", "nome")
            evapotranspiracao[, data_rodada := dataRodada]
            evapotranspiracao <- combina_observacao_previsao(evapotranspiracao, previsao_evapotranspiracao_rodada)
            
            matriz_evapotranspiracao_planicie <- matrix(evapotranspiracao[, valor] * vetor_modelo[77], nrow = numero_cenarios, ncol = nrow(evapotranspiracao), byrow = TRUE)
            matriz_evapotranspiracao <- matrix(evapotranspiracao[, valor] * vetor_modelo[76], nrow = numero_cenarios, ncol = nrow(evapotranspiracao), byrow = TRUE)
            if (numero_cenarios == 1) {
                matriz_evapotranspiracao_planicie[, 1:numero_dias_assimilacao] <- matriz_evapotranspiracao_planicie[, 1:numero_dias_assimilacao] * ajuste$ajuste$par[(1:numero_dias_assimilacao) * 2]
                matriz_evapotranspiracao[, 1:numero_dias_assimilacao] <- matriz_evapotranspiracao[, 1:numero_dias_assimilacao] * ajuste$ajuste$par[(1:numero_dias_assimilacao) * 2]
            } else {
                matriz_evapotranspiracao_planicie[, 1:numero_dias_assimilacao] <- sweep(matriz_evapotranspiracao_planicie[, 1:numero_dias_assimilacao], 2, ajuste$ajuste$par[(1:numero_dias_assimilacao) * 2], `*`)
                matriz_evapotranspiracao[, 1:numero_dias_assimilacao] <- sweep(matriz_evapotranspiracao[, 1:numero_dias_assimilacao], 2, ajuste$ajuste$par[(1:numero_dias_assimilacao) * 2], `*`)
            }

            EbInic <- ajuste$ajuste$par[numero_dias_assimilacao * 2 + 1]
            Supin <-  ajuste$ajuste$par[numero_dias_assimilacao * 2 + 2]
            if (Supin < 0) { #L-BFGS-B as vezes fornece valor negativo próximo a 0 ('-1e-17')
                Supin <- 0
            }
            inicializacao_caso <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
            inicializacao_caso <- unlist(inicializacao_caso)

            vetor_inicializacao[, 4] <- inicializacao_caso[4]
            vetor_inicializacao[, 5] <- inicializacao_caso[5]
            vetor_inicializacao[, 6] <- inicializacao_caso[6]
            vetor_inicializacao[, 7] <- inicializacao_caso[7]
            
            simulacao <- funcaoSmapCpp::rodada_cenarios_dias_cpp2(vetor_modelo,
            vetor_inicializacao, area, matriz_precipitacao,
            matriz_evapotranspiracao, matriz_evapotranspiracao_planicie, (numero_dias_previsao + numero_dias_assimilacao), numero_cenarios)

            saida_bacia_aux <- data.table::data.table(do.call(rbind, simulacao))

            saida_bacia_aux[, nome := sub_bacia]
            saida_bacia_aux[, data_caso := dataRodada]
            saida_bacia_aux[, cenario := rep(nome_cenario, each = (numero_dias_previsao + numero_dias_assimilacao))]
            saida_bacia_aux[, data_previsao := rep(seq.Date(dataRodada - numero_dias_assimilacao, dataRodada + numero_dias_previsao - 1, by = 1), numero_cenarios)]
            saida_bacia_aux <- saida_bacia_aux[data_previsao >= dataRodada]
            saida <- data.table::rbindlist(list(saida, saida_bacia_aux))

            saida_ajuste_otimizacao <- data.table::rbindlist(list(saida_ajuste_otimizacao, saida_ajuste_otimizacao_aux))
            
            saida_ajuste_assimilacao <- data.table::rbindlist(list(saida_ajuste_assimilacao, ajuste$simulacao))
            
            saida_ajuste_fo <- data.table::rbindlist(list(saida_ajuste_fo, saida_ajuste_fo_aux))

            if (idata < numero_datas) {
                inicio_proxima_assimilacao <- datas_rodadas[idata + 1, data] - numero_dias_assimilacao - 1
                EbInic <- ajuste$simulacao[data_assimilacao == inicio_proxima_assimilacao, Qbase]
                Supin <- ajuste$simulacao[data_assimilacao == inicio_proxima_assimilacao, Qsup1 + Qsup2]
                TuInic <- ajuste$simulacao[data_assimilacao == inicio_proxima_assimilacao, Tu]
            }
        }
    }
    saida <- melt(saida, id.vars = c("data_caso", "data_previsao", "cenario", "nome"), variable.name = "variavel",
           value.name = "valor")
    saida_ajuste_assimilacao <- melt(saida_ajuste_assimilacao, id.vars = c("data_caso", "data_assimilacao", "nome"), variable.name = "variavel",
           value.name = "valor")
    saida <- list(previsao = saida, otimizacao = saida_ajuste_otimizacao, assimilacao = saida_ajuste_assimilacao, precipitacao = saida_precipitacao, funcao_objetivo = saida_ajuste_fo)
    saida
}

#' Funcao para realizar rodadas do SMAP/ONS a partir de dados assimilados
#' 
#' Realiza execucoes do modelo SMAP/ONS a partir de dados ja assimilados. Utilizada
#' principalmente para validar os resultados do pacote com o aplicativo SMAP/ONS original.
#'
#' @param parametros data table com os parametros dos modelos
#'     \itemize{
#'     \item{nome - nome da sub-bacia}
#'     \item{parametro - nome do parametro}
#'     \item{valor - valor do parametro}
#'     }
#' @param inicializacao data.table com a inicializacao com as colunas:
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{variavel - vazao de base inicial}
#'     \item{valor - valor da variavel}
#'     }
#' @param precipitacao_observada data table com o historico de precipitacao com as colunas:
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param precipitacao_prevista data.table com a previsao de precipitacao com as colunas:
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da previsao}
#'     }
#' @param evapotranspiracao_nc data.table com o historico de NC deevapotranspiracao com as colunas:
#'     \itemize{
#'     \item{mes - mes da NC}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da NC de evapotranspiracao observada}
#'     }
#' @param vazao_observada data table com o historico de vazao com as colunas:
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param postos_plu data table contendo a relacao sub-bacia x postos_plu com as colunas:
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{posto - nome do posto plu}
#'     \item{valor - peso do posto plu}
#'     }
#' @param datas_rodadas data table contendo as datas dos casos a serem executados e seus respectivos horizontes:
#'     \itemize{
#'     \item{data - data do caso}
#'     \item{numero_dias_previsao - horizonte do caso}
#'     }
#' @param execucao data table contendo as informacoes do arquivo execucao.txt gerado pelo aplicativo SMAP/ONS original com o seguinte formato:
#'     \itemize{
#'         \item{subbacia} nome da subbacia da qual estes dados dizem respeito
#'         \item{data} dia referencia de execucao da assimilacao
#'         \item{dia_assimilacao} numero de dias atras da variavel assimilada com respeito a rodada
#'         \item{qcal} vazao calculada da assimilacao
#'         \item{rsolo} variavel Rsolo da assimilacao
#'         \item{rsub} variavel Rsub da assimilacao
#'         \item{rsup} variavel Rsup da assimilacao
#'         \item{peso_chuva} pesos das precs passadas na assimilacao 
#'         \item{ebin} valor de ebin obtido na assimilacao
#'         \item{supin} valor de supin obtido na assimilacao
#'         \item{funcao_objetivo} valor da funcao objetivo obtida (valores repetidos por subbacia)
#'         \item{data_caso} data do caso
#'     }
#' @param numero_cenarios numero de cenarios a serem gerados
#' @param sub_bacias vetor com o nome das sub-bacias a serem consideradas
#' @importFrom data.table data.table
#' @return saida lista com o data.table previsao contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#'  otimizacao data table com as colunas:
#' \itemize{
#'     \item{otimizacao - valor das variaveis da otimizacao}
#'     \item{nome - nome da sub-bacia}
#'     \item{data_caso - data do caso}
#'     }
#'  assimilacao data table com as colunas:
#' \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_assimilacao - data da assimilacao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' precipitacao data table contendo precipitacao observada e previsata com as colunas:
#' \itemize{
#'     \item{data_previsao - data da precipitacao}
#'     \item{data_rodada - data da rodada}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' @export
rodada_sem_assimilacao <- function(parametros, inicializacao, precipitacao_observada, 
    precipitacao_prevista, evapotranspiracao_nc, vazao_observada, postos_plu, datas_rodadas, 
    numero_cenarios, sub_bacias, execucao) {
    
    numero_sub_bacias <- length(sub_bacias)
    numero_datas <- nrow(datas_rodadas)
    numero_dias_previsao <- datas_rodadas$numero_dias_previsao
    nome_cenario <- unique(precipitacao_prevista[, cenario])

    saida <- data.table::data.table()
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
            matriz_precipitacao <- array(precipitacao[data_previsao < (dataRodada + numero_dias_previsao + kt_max) & data_rodada == dataRodada & 
                data_previsao >= (dataRodada - kt_min), valor], c(numero_dias_previsao + kt_max + kt_min, numero_cenarios))
            matriz_precipitacao <- t(ponderacao_temporal(matriz_precipitacao, kt, kt_max, kt_min))
            
            colnames(precipitacao)[1] <- "data"
            precipitacao <- precipitacao[cenario == unique(cenario)[1]]
            precipitacao[, data_rodada := NULL]
            precipitacao[, cenario := NULL]
            evapotranspiracao <- transforma_NC_serie(precipitacao[data <= dataRodada + numero_dias_previsao - 1 & data >= dataRodada], normal_climatologica)
            matriz_evapotranspiracao_planicie <- matrix(evapotranspiracao[, valor] * vetor_modelo[77], nrow = numero_cenarios, ncol = nrow(evapotranspiracao), byrow = TRUE)
            matriz_evapotranspiracao <- matrix(evapotranspiracao[, valor] * vetor_modelo[76], nrow = numero_cenarios, ncol = nrow(evapotranspiracao), byrow = TRUE)
        

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
