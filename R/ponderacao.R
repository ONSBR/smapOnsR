# ----- Funcoes de ponderacao ----

#' Realiza a ponderacao temporal de variaveis
#'
#' @param serie_temporal data table com a variavel a ser ponderada com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @param pesos Vetor de pesos iniciando em kt+2 ate kt-60
#' @param kt_min numero de dias para trás a serem considerados na ponderacao
#' @param kt_max  numero de dias a frente a serem considerados na ponderacao
#' @param data_inicio data de inicio da construcao da serie temporal ponderada
#' @param data_fim data de inicio da construcao da serie temporal ponderada
#' @importFrom data.table setkey
#' @return serie_temporal_ponderada: data table com a variavel ponderada com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel ponderada}
#'     }
#' @export
poderacao_temporal <- function(serie_temporal, pesos, kt_min, kt_max, data_inicio, data_fim){
    
    if(serie_temporal[, min(data)] > (data_inicio - kt_min)){
        stop("Erro: Data de inicio da serie temporal a ser ponderada inferior ao necessario")
    }

    if(serie_temporal[, max(data)] < (data_fim + kt_max)){
        stop("Erro: Data final  da serie temporal a ser ponderada inferior ao necessario")
    }
    
    serie_temporal_ponderada <- serie_temporal
    data.table::setkey(serie_temporal_ponderada, data)

    serie_temporal_ponderada[, c("inicio_idx", "fim_idx") := {
        idx <- 1:.N
        inicio_idx <- pmax(1, idx - kt_min)
        fim_idx <- pmin(.N, idx + kt_max)
        .(inicio_idx, fim_idx)
    }]

    serie_temporal_ponderada[, media_ponderada := {
        result <- mapply(function(inicio, fim) {
            sum(valor[inicio:fim] * pesos[(kt_min + 3):(3 - kt_max)])
        }, inicio_idx, fim_idx)
        
        result
    }]

    serie_temporal_ponderada <- serie_temporal_ponderada[data >= data_inicio & data <= data_fim]
    serie_temporal_ponderada[, valor := NULL]
    serie_temporal_ponderada[, inicio_idx := NULL]
    serie_temporal_ponderada[, fim_idx := NULL]
    colnames(serie_temporal_ponderada)[4] <- "valor"
    serie_temporal_ponderada
}

