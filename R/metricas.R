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
#' @examples 
#' observacao <- 1:30
#' simulacao <- observacao - 0.5
#' pbias <- calcula_pbias(simulacao, observacao)
#' @return vies percentual
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
#' @examples 
#' observacao <- 1:30
#' simulacao <- observacao - 0.5
#' pbias <- calcula_correlacao(simulacao, observacao)
#' @return correlacao ponderada entre as amostras
#' @export 

calcula_correlacao <- function(simulacao, observacao, pesos = rep(1 /length(observacao), length(observacao))){
      
    media_observacao <- sum(observacao * pesos)
    media_simulacao <- sum(simulacao * pesos)

    # Calculate the components of the weighted correlation formula
    numerador <- sum(pesos * (observacao - media_observacao) * (simulacao - media_simulacao))
    denominador_observacao <- sqrt(sum(pesos * (observacao - media_observacao) ^ 2))
    denominador_simulacao <- sqrt(sum(pesos * (simulacao - media_simulacao) ^ 2))

    # Calculate weighted correlation
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
#' @examples 
#' observacao <- 1:30
#' simulacao <- observacao - 0.5
#' pbias <- calcula_alfa(simulacao, observacao)
#' @return correlacao ponderada entre as amostras
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
#' @return correlacao ponderada entre as amostras
#' @export 

calcula_kge <- function(simulacao, observacao, pesos = rep(1 /length(observacao), length(observacao))){
    
    correlacao <- calcula_correlacao(simulacao, observacao, pesos)
    pbias <- calcula_pbias(simulacao, observacao, pesos)
    alfa <- calcula_alfa(simulacao, observacao, pesos)

    kge <- 1 - sqrt((correlacao - 1) ^ 2 + (pbias - 1) ^ 2 + (alfa - 1) ^ 2)

    kge
}