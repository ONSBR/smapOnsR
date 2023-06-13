#' Funcao de ponderacao temporal 
#' 
#' Realiza a ponderacao temporal de variaveis atraves dos pesos kt
#'
#' @param serie_temporal vetor com a variavel a ser ponderada
#' @param kt vetor de kts
#' @param kt_max valor do maximo lag positivo
#' @param kt_min valor do maximo lag maximo negativo
#' @importFrom stats embed
#' @return serie_temporal_ponderada: vetor com a serie temporal ponderada pelos kts
#' @export
ponderacao_temporal <- function(serie_temporal, kt, kt_max, kt_min) {

  serie_temporal_ponderada <- rowSums(embed(serie_temporal, kt_min + kt_max + 1) * kt[(3 - kt_max):(3 + kt_min)])

  serie_temporal_ponderada
}
