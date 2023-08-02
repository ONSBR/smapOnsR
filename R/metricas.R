#' FUNCOES PARA CALCULO DE METRICA DE AVALIACAO DO MODELO

#' Calcula NSE
#' 
#' Realiza o calculo do NSE
#'
#' @param simulacao vetor com os valores da serie simulada
#' @param observacao vetor com os valores da serie simulada
#' @param pesos pesos a serem utilizados para cada data
#' @return nse
#' @export

calcula_nse <- function(simulacao, observacao, pesos = rep(1 /length(observacao), length(observacao))){
    erro_previsao <- sum((observacao - simulacao) ^ 2 * pesos)
    erro_media <- sum((observacao - mean(observacao)) ^ 2 * pesos)
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
#' @return dm distancia multicriterio
#' @export 

calcula_dm <- function(simulacao, observacao, pesos = rep(1 /length(observacao), length(observacao))){
    mape <- calcula_mape(simulacao, observacao, pesos)
    nse <- calcula_nse(simulacao, observacao, pesos)
    dm <- sqrt(mape ^ 2 + (1 - nse)^2)
    dm
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
#' @param inicio
#' 
#' @importFrom hydroGOF gof 
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

analisa_previsoes <- function(simulacao, observacao, semanal = TRUE, mensal = TRUE, anual = FALSE, inicio = NULL){
    colnames(simulacao)[2] <- "data"
    simulacao[, dia_previsao := as.numeric(data - data_caso)]
    resultado <- data.table::data.table()
    dia_maximo <- max(simulacao[, dia_previsao])
    dia_minimo <- min(simulacao[, dia_previsao])
    numero_dias <- length(unique(simulacao[, dia_previsao]))
    for (idia in 0:dia_maximo){
        data_inicio <- min(simulacao[dia_previsao == idia, data])
        data_fim <- max(simulacao[dia_previsao == idia, data])
        metricas <- hydroGOF::gof(simulacao[dia_previsao == idia, valor], observacao[data %in% simulacao[dia_previsao == idia, data], valor])
        MAPE <- calcula_mape(simulacao[dia_previsao == idia, valor], observacao[data %in% simulacao[dia_previsao == idia, data], valor])
        metricas <- rbind(metricas, MAPE)
        DM <- calcula_dm(simulacao[dia_previsao == idia, valor], observacao[data %in% simulacao[dia_previsao == idia, data], valor])
        metricas <- rbind(metricas, DM)
        resultado <- rbind(resultado, metricas)
    }
    nomes <- rownames(metricas)
    resultado[, metrica := rep(nomes, numero_dias)]
    resultado[, dia_previsao := rep(dia_minimo:dia_maximo, each = 22)]
    resultado[, nome := rep(unique(simulacao[, nome]), numero_dias * 22)]
    colnames(resultado)[1] <- "valor"
    data.table::setcolorder(resultado, c("nome", "metrica", "valor", "dia_previsao"))

    if (semanal){
        numero_semana <- rep(1:ceiling(dia_maximo / 7), each = 7)[1:numero_dias]
        merged_data <- merge(simulacao, observacao, by = "data")
        data.table::setorder(merged_data, data_caso, data)
        merged_data[, numero_semana := rep(numero_semana, (nrow(simulacao) / numero_dias))]
        simulacao_semanal <- merged_data[, .(previsao_semanal = mean(valor.x), observacao_semanal = mean(valor.y)), by = .(numero_semana, data_caso)]

        resultado_semanal <- data.table::data.table()
        for (isemana in unique(simulacao_semanal[, numero_semana])){
            metricas_semanais <- hydroGOF::gof(simulacao_semanal[numero_semana == isemana, previsao_semanal],
                                                simulacao_semanal[numero_semana == isemana, observacao_semanal])
            MAPE <- calcula_mape(simulacao_semanal[numero_semana == isemana, previsao_semanal], simulacao_semanal[numero_semana == isemana, observacao_semanal])
            metricas_semanais <- rbind(metricas_semanais, MAPE)
            DM <- calcula_dm(simulacao_semanal[numero_semana == isemana, previsao_semanal], simulacao_semanal[numero_semana == isemana, observacao_semanal])
            metricas_semanais <- rbind(metricas_semanais, DM)
            resultado_semanal <- rbind(resultado_semanal, metricas_semanais)
        }
        resultado_semanal[, metrica := rep(nomes, ceiling(dia_maximo / 7))]
        resultado_semanal[, numero_semana := rep(1:ceiling(dia_maximo / 7), each = 22)]
        resultado_semanal[, nome := rep(unique(simulacao[, nome]), ceiling(dia_maximo / 7) * 22)]
        colnames(resultado_semanal)[1] <- "valor"
        data.table::setcolorder(resultado_semanal, c("nome", "metrica", "valor", "numero_semana"))
    }
    
    saida <- list(resultado = resultado, resultado_semanal = resultado_semanal)
}