#' Calcula NSE
#' 
#' Realiza o calculo do NSE
#'
#' @param simulacao vetor com os valores da serie simulada
#' @param observacao vetor com os valores da serie simulada
#' @param pesos pesos a serem utilizados para cada data
#' @examples 
#' observacao <- 1:30
#' simulacao <- observacao - 0.5
#' nse <- calcula_nse(simulacao, observacao)
#' @importFrom stats weighted.mean
#' @return nse
#' @export

calcula_nse <- function(simulacao, observacao, pesos = rep(1 / length(observacao), length(observacao))){
    erro_previsao <- sum((observacao - simulacao) ^ 2 * pesos)
    erro_media <- sum((observacao - stats::weighted.mean(observacao, pesos)) ^ 2 * pesos)
    nse <- 1 - erro_previsao / erro_media
    nse
}

#' Calcula MAPE
#' 
#' Realiza o calculo do MAPE
#'
#' @param simulacao vetor com os valores da serie simulada
#' @param observacao vetor com os valores da serie simulada
#' @param pesos pesos a serem utilizados para cada data
#' @examples 
#' observacao <- 1:30
#' simulacao <- observacao - 0.5
#' nse <- calcula_mape(simulacao, observacao)
#' @return mape
#' @export

calcula_mape <- function(simulacao, observacao, pesos = rep(1 /length(observacao), length(observacao))){
    mape <- sum(abs((observacao - simulacao) / observacao) * pesos)
    mape
}

#' Calcula DM
#' 
#' Realiza o calculo da DM
#'
#' @param simulacao vetor com os valores da serie simulada
#' @param observacao vetor com os valores da serie simulada
#' @param pesos pesos a serem utilizados para cada data
#' @examples 
#' observacao <- 1:30
#' simulacao <- observacao - 0.5
#' nse <- calcula_dm(simulacao, observacao)
#' @return dm distancia multicriterio
#' @export 

calcula_dm <- function(simulacao, observacao, pesos = rep(1 /length(observacao), length(observacao))){
    mape <- calcula_mape(simulacao, observacao, pesos)
    nse <- calcula_nse(simulacao, observacao, pesos)
    dm <- sqrt(mape ^ 2 + (1 - nse)^2)
    dm
}

#' Calcula PBIAS
#' 
#' Realiza o calculo do vies percentual de duas amostras
#'
#' @param simulacao vetor com os valores da serie simulada
#' @param observacao vetor com os valores da serie simulada
#' @param pesos pesos a serem utilizados para cada data
#' 
#' @examples
#' observacao <- 1:30
#' simulacao <- observacao - 0.5
#' pbias <- calcula_pbias(simulacao, observacao)
#' 
#' @return vies percentual
#' 
#' @export

calcula_pbias <- function(simulacao, observacao, pesos = rep(1 /length(observacao), length(observacao))){
    pbias <- sum((simulacao * pesos)) /  sum((observacao * pesos))
    pbias
}

#' Calcula correlacao
#' 
#' Realiza o calculo da correlacao de duas amostras
#'
#' @param simulacao vetor com os valores da serie simulada
#' @param observacao vetor com os valores da serie simulada
#' @param pesos pesos a serem utilizados para cada data
#' 
#' @examples 
#' observacao <- 1:30
#' simulacao <- observacao - 0.5
#' pbias <- calcula_correlacao(simulacao, observacao)
#' @return correlacao ponderada entre as amostras
#' 
#' @export 

calcula_correlacao <- function(simulacao, observacao, pesos = rep(1 /length(observacao), length(observacao))){
      
    media_observacao <- sum(observacao * pesos)
    media_simulacao <- sum(simulacao * pesos)

    numerador <- sum(pesos * (observacao - media_observacao) * (simulacao - media_simulacao))
    denominador_observacao <- sqrt(sum(pesos * (observacao - media_observacao) ^ 2))
    denominador_simulacao <- sqrt(sum(pesos * (simulacao - media_simulacao) ^ 2))

    correlacao <- numerador / (denominador_observacao * denominador_simulacao)

    correlacao
}

#' Calcula alfa
#' 
#' Realiza o calculo do termo alfa para o kge, representando a variabilidade dos erros
#'
#' @param simulacao vetor com os valores da serie simulada
#' @param observacao vetor com os valores da serie simulada
#' @param pesos pesos a serem utilizados para cada data
#' 
#' @examples
#' observacao <- 1:30
#' simulacao <- observacao - 0.5
#' pbias <- calcula_alfa(simulacao, observacao)
#' @return correlacao ponderada entre as amostras
#' 
#' @export 

calcula_alfa <- function(simulacao, observacao, pesos = rep(1 /length(observacao), length(observacao))){

    dp_simulacao <- sqrt(sum(pesos * (simulacao - sum(simulacao * pesos)) ^ 2))
    dp_observacao <- sqrt(sum(pesos * (observacao - sum(observacao * pesos)) ^ 2))

    alfa <- dp_simulacao / dp_observacao

    alfa
}

#' Calcula kge
#' 
#' Realiza o calculo do indice de eficiencia kge
#'
#' @param simulacao vetor com os valores da serie simulada
#' @param observacao vetor com os valores da serie simulada
#' @param pesos pesos a serem utilizados para cada data
#' @examples
#' observacao <- 1:30
#' simulacao <- observacao - 0.5
#' kge <- calcula_kge(simulacao, observacao)
#' @return Valor do KGE ponderado
#' 
#' @export 

calcula_kge <- function(simulacao, observacao, pesos = rep(1 /length(observacao), length(observacao))) {
    
    correlacao <- calcula_correlacao(simulacao, observacao, pesos)
    pbias <- calcula_pbias(simulacao, observacao, pesos)
    alfa <- calcula_alfa(simulacao, observacao, pesos)

    kge <- 1 - sqrt((correlacao - 1) ^ 2 + (pbias - 1) ^ 2 + (alfa - 1) ^ 2)

    kge
}

#' Analise das previsoes
#' 
#' Calcula diversas metricas de avaliacao para series diaria, podendo fazer acumulados semanais, mensais,
#' sazonais, e anuais
#' 
#' 
#' @param simulacao data table com a previsao contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{nome}{nome da sub-bacia}
#'     \item{variavel}{nome da variavel}
#'     \item{valor}{valor da variavel}
#'     }
#' @param observacao data table com o historico de vazao com as colunas:
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @param semanal booleano indicando se a analise deve ser feita em acumulados semanais
#' @param mensal booleano indicando se a analise deve ser feita em acumulados semanais
#' @param anual booleano indicando se a analise deve ser feita em acumulados semanais
#' 
#'
#' @return saida lista contendo data table resultado com as colunas:
#' \itemize{
#'     \item{nome}{nome da sub-bacia}
#'     \item{metrica}{nome da metrica}
#'     \item{valor}{valor da metrica}
#'     \item{dia_previsao}{horizonte da previsao}
#'     }
#' e data table resultado semanal com as colunas:
#' \itemize{
#'     \item{nome}{nome da sub-bacia}
#'     \item{metrica}{nome da metrica}
#'     \item{valor}{valor da metrica}
#'     \item{numero_semana}{horizonte semanal da previsao}
#'     }
#' @export

analisa_previsoes <- function(simulacao, observacao, semanal = TRUE, mensal = TRUE, anual = FALSE){
    colnames(simulacao)[2] <- "data"
    simulacao[, horizonte := as.numeric(data - data_caso)]
    resultado <- data.table::data.table()
    dia_maximo <- max(simulacao[, horizonte])
    dia_minimo <- min(simulacao[, horizonte])
    numero_dias <- length(unique(simulacao[, horizonte]))
    for (idia in 0:dia_maximo){
        obs <- observacao[data %in% simulacao[horizonte == idia, data], valor]
        prev <- simulacao[horizonte == idia, valor][1:length(obs)]
        PBIAS <- smapOnsR::calcula_pbias(prev, obs)
        resultado <- rbind(resultado, PBIAS)
        NSE <- smapOnsR::calcula_nse(prev, obs)
        resultado <- rbind(resultado, NSE)
        MAPE <- smapOnsR::calcula_mape(prev, obs)
        resultado <- rbind(resultado, MAPE)
        DM <- smapOnsR::calcula_dm(prev, obs)
        resultado <- rbind(resultado, DM)
        KGE <- smapOnsR::calcula_kge(prev, obs)
        resultado <- rbind(resultado, KGE)
        RMSE <- smapOnsR::calcula_rmse(prev, obs)
        resultado <- rbind(resultado, RMSE)
    }
    nomes_metr <- c("PBIAS", "NSE", "MAPE", "DM", "KGE", "RMSE")
    resultado[, metrica := rep(nomes_metr, numero_dias)]
    resultado[, horizonte := rep(dia_minimo:dia_maximo, each = 6)]
    resultado[, nome := rep(unique(simulacao[, nome]), numero_dias * 6)]
    resultado[, discretizacao := 'diaria']
    colnames(resultado)[1] <- "valor"
    data.table::setcolorder(resultado, c("nome", "metrica", "valor", "discretizacao", "horizonte"))

    resultado_semanal <- data.table::data.table()
    simulacao_semanal <- data.table::data.table()
    if (semanal) {
        simulacao_semanal <- agrega_semanal(simulacao, observacao)

        resultado_semanal <- simulacao_semanal[
        , .(
            metrica = nomes_metr,
            valor   = c(
                smapOnsR::calcula_pbias(previsao, observacao),
                smapOnsR::calcula_nse(previsao, observacao),
                smapOnsR::calcula_mape(previsao, observacao),
                smapOnsR::calcula_dm(previsao, observacao),
                smapOnsR::calcula_kge(previsao, observacao),
                smapOnsR::calcula_rmse(previsao, observacao)
            )
            ),
        by = .(horizonte = horizonte)
        ]
        resultado_semanal[, nome := rep(unique(simulacao[, nome]), ceiling(dia_maximo / 7) * 6)]
        resultado_semanal[, discretizacao := 'semanal']
        data.table::setcolorder(resultado_semanal, c("nome", "metrica", "valor", "discretizacao", "horizonte"))
    }

    resultado_mensal <- data.table::data.table()
    if (mensal) {
        colnames(simulacao)[2] <- "data_previsao"
        blocos_mensais <- agrega_mensal(simulacao, observacao)

        resultado_mensal <- blocos_mensais[
        , .(
            metrica = nomes_metr,
            valor   = c(
                smapOnsR::calcula_pbias(previsao, observacao),
                smapOnsR::calcula_nse(previsao, observacao),
                smapOnsR::calcula_mape(previsao, observacao),
                smapOnsR::calcula_dm(previsao, observacao),
                smapOnsR::calcula_kge(previsao, observacao),
                smapOnsR::calcula_rmse(previsao, observacao)
            )
            ),
        by = .(horizonte = horizonte)
        ]

        resultado_mensal[, nome := simulacao[, unique(nome)]]
        resultado_mensal[, discretizacao := 'mensal']
        # 7) Reordena colunas
        data.table::setcolorder(resultado_mensal,
            c("nome", "metrica", "valor", "discretizacao", "horizonte")
        )
    }
    resultado <- data.table::rbindlist(list(resultado, resultado_semanal, resultado_mensal),
     use.names = TRUE, fill = TRUE)
    
    saida <- list(resultado = resultado, simulacao_semanal = simulacao_semanal)
    saida
}

#' Calcula RMSE
#'
#' Realiza o calculo da raiz do erro quadratico medio entre duas amostras
#'
#' @param simulacao vetor com os valores da serie simulada
#' @param observacao vetor com os valores da serie observada
#' @param pesos vetor de pesos a serem utilizados para cada data (default: pesos iguais)
#'
#' @examples
#' observacao <- 1:30
#' simulacao <- observacao + rnorm(30, 0, 1)
#' rmse <- calcula_rmse(simulacao, observacao)
#'
#' @return erro quadratico medio (RMSE)
#'
#' @export

calcula_rmse <- function(simulacao, observacao, pesos = rep(1 / length(observacao), length(observacao))) {
  rmse <- sqrt(sum(((simulacao - observacao)^2) * pesos))
  rmse
}