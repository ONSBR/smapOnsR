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
#' @param modelo objeto de classe smap_ons
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
poderacao_temporal <- function(serie_temporal, modelo, data_inicio, data_fim){
    
    kt_min <- sum(modelo$kt[4:63] > 0)
    kt_max <- sum(modelo$kt[1:2] > 0)

    if(serie_temporal[, min(data)] > (data_inicio - kt_min)){
        stop("Erro: Data de inicio da serie temporal a ser ponderada inferior ao necessario")
    }

    if(serie_temporal[, max(data)] < (data_fim + kt_max)){
        stop("Erro: Data final  da serie temporal a ser ponderada inferior ao necessario")
    }

    serie_temporal_ponderada = serie_temporal
    data.table::setkey(serie_temporal_ponderada, data)

    serie_temporal_ponderada[, c("inicio_idx", "fim_idx") := {
        idx <- 1:.N
        inicio_idx <- pmax(1, idx - kt_min)
        fim_idx <- pmin(.N, idx + kt_max)
        .(inicio_idx, fim_idx)
    }]

    serie_temporal_ponderada[, media_ponderada := {
        result <- mapply(function(inicio, fim) {
            sum(valor[inicio:fim] * modelo$kt[(kt_min + 3):(3 - kt_max)])
        }, inicio_idx, fim_idx)
        
        result
    }]

    serie_temporal_ponderada <- serie_temporal_ponderada[data >= data_inicio & data <= data_fim]
    serie_temporal_ponderada[, valor := NULL]
    serie_temporal_ponderada[, inicio_idx := NULL]
    serie_temporal_ponderada[, fim_idx := NULL]
    serie_temporal[, inicio_idx := NULL]
    serie_temporal[, fim_idx := NULL]
    serie_temporal[, media_ponderada := NULL]
    colnames(serie_temporal_ponderada)[4] <- "valor"

    serie_temporal_ponderada
}

