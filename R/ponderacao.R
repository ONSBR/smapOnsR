#' Realiza a ponderacao temporal de variaveis
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
  
  id_min <- kt_min + 1
  id_max <- length(serie_temporal) - kt_max
  serie_temporal_ponderada <- serie_temporal[id_min:id_max]

  for (i in id_min:id_max) {
    inicio <- i - kt_min
    fim <- i + kt_max
    serie_temporal_ponderada[i - kt_min] <- sum(serie_temporal[inicio:fim] * kt[(kt_min + 3):(3 - kt_max)])
  }

  serie_temporal_ponderada
}
