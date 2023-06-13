#' Funcao de ponderacao temporal 
#' 
#' Realiza a ponderacao temporal de variaveis atraves dos pesos kt
#'
#' @param serie_temporal vetor com a variavel a ser ponderada 
#' @param kt vetor de kts
#' @param kt_max valor do maximo lag positivo
#' @param kt_min valor do maximo lag maximo negativo
#' @return serie_temporal_ponderada: data table com a variavel ponderada com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel ponderada}
#'     }
#' @export
ponderacao_temporal <- function(serie_temporal, kt, kt_max, kt_min) {

  serie_temporal_ponderada <- rowSums(embed(serie_temporal, kt_min + kt_max + 1) * kt[(3 - kt_max):(3 + kt_min)])

  serie_temporal_ponderada
}
