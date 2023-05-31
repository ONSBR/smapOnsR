#' FUNCOES PARA CALCULO DE METRICA DE AVALIACAO DO MODELO

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
