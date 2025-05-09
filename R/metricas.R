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
    simulacao[, dia_previsao := as.numeric(data - data_caso)]
    resultado <- data.table::data.table()
    dia_maximo <- max(simulacao[, dia_previsao])
    dia_minimo <- min(simulacao[, dia_previsao])
    numero_dias <- length(unique(simulacao[, dia_previsao]))
    for (idia in 0:dia_maximo){
        obs <- observacao[data %in% simulacao[dia_previsao == idia, data], valor]
        prev <- simulacao[dia_previsao == idia, valor][1:length(obs)]
        PBIAS <- smapOnsR::calcula_pbias(prev, obs)
        resultado <- rbind(resultado, PBIAS)
        NSE <- smapOnsR::calcula_nse(prev, obs)
        resultado <- rbind(resultado, NSE)
        MAPE <- smapOnsR::calcula_mape(prev, obs)
        resultado <- rbind(resultado, MAPE)
        DM <- smapOnsR::calcula_dm(prev, obs)
        resultado <- rbind(resultado, DM)
    }
    nomes <- c("PBIAS", "NSE", "MAPE", "DM")
    resultado[, metrica := rep(nomes, numero_dias)]
    resultado[, horizonte := rep(dia_minimo:dia_maximo, each = 4)]
    resultado[, nome := rep(unique(simulacao[, nome]), numero_dias * 4)]
    resultado[, discretizacao := 'diaria']
    colnames(resultado)[1] <- "valor"
    data.table::setcolorder(resultado, c("nome", "metrica", "valor", "discretizacao", "horizonte"))

    resultado_semanal <- data.table::data.table()
    if (semanal) {
        merged_data <- merge(simulacao, observacao, by = "data")
        data.table::setorder(merged_data, data_caso, data)
        merged_data[, dia_semana := lubridate::wday(data)]
        data.table::setDT(merged_data)

        # Step 1: Exclude rows before the first "dia_semana" == 7 for each "data_caso"
        merged_data <- merged_data[, .SD[dia_semana == 7 | (cumsum(dia_semana == 7) > 0)], by = data_caso]

        # Step 2: Create a sequence for each "data_caso" based on the remainder when dividing by 7
        merged_data[, numero_semana := rep(1:(.N %/% 7 + (ifelse(.N %% 7 == 0, 0, 1))), each = 7, length.out = .N), by = data_caso]

        # Step 3: Calculate weekly mean and count
        simulacao_semanal <- merged_data[, .(previsao_semanal = mean(valor.x), observacao_semanal = mean(valor.y), count = .N), by = .(data_caso, numero_semana, nome)]

        
        for (isemana in unique(simulacao_semanal[, numero_semana])){
            prev <- simulacao_semanal[numero_semana == isemana, previsao_semanal]
            obs <- simulacao_semanal[numero_semana == isemana, observacao_semanal]
            PBIAS <- smapOnsR::calcula_pbias(prev, obs)
            resultado_semanal <- rbind(resultado_semanal, PBIAS)
            NSE <- smapOnsR::calcula_nse(prev, obs)
            resultado_semanal <- rbind(resultado_semanal, NSE)
            MAPE <- smapOnsR::calcula_mape(prev, obs)
            resultado_semanal <- rbind(resultado_semanal, MAPE)
            DM <- smapOnsR::calcula_dm(prev, obs)
            resultado_semanal <- rbind(resultado_semanal, DM)
        }
        resultado_semanal[, metrica := rep(nomes, ceiling(dia_maximo / 7))]
        resultado_semanal[, horizonte := rep(1:ceiling(dia_maximo / 7), each = 4)]
        resultado_semanal[, nome := rep(unique(simulacao[, nome]), ceiling(dia_maximo / 7) * 4)]
        resultado_semanal[, discretizacao := 'semanal']
        colnames(resultado_semanal)[1] <- "valor"
        data.table::setcolorder(resultado_semanal, c("nome", "metrica", "valor", "discretizacao", "horizonte"))
    }

    resultado_mensal <- data.table::data.table()
    if (mensal) {      
      prev <- simulacao_semanal[numero_semana %in% c(1,4), mean(previsao_semanal), by = data_caso]
      obs <- simulacao_semanal[numero_semana  %in% c(1,4), mean(observacao_semanal), by = data_caso]
      PBIAS <- smapOnsR::calcula_pbias(prev$V1, obs$V1)
      resultado_mensal <- rbind(resultado_mensal, PBIAS)
      NSE <- smapOnsR::calcula_nse(prev$V1, obs$V1)
      resultado_mensal <- rbind(resultado_mensal, NSE)
      MAPE <- smapOnsR::calcula_mape(prev$V1, obs$V1)
      resultado_mensal <- rbind(resultado_mensal, MAPE)
      DM <- smapOnsR::calcula_dm(prev$V1, obs$V1)
      resultado_mensal <- rbind(resultado_mensal, DM)

      resultado_mensal[, metrica := nomes]
      resultado_mensal[, nome := rep(unique(simulacao[, nome]), length(nomes))]
      resultado_mensal[, discretizacao := 'mensal']
      resultado_mensal[, horizonte := 1]
      colnames(resultado_mensal)[1] <- "valor"
      data.table::setcolorder(resultado_mensal, c("nome", "metrica", "valor", "discretizacao", "horizonte"))
    }
    resultado <- data.table::rbindlist(list(resultado, resultado_semanal, resultado_mensal),
     use.names = TRUE, fill = TRUE)
    
    saida <- list(resultado = resultado, simulacao_semanal = simulacao_semanal)
    saida
}