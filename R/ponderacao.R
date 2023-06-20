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

#' Funcao de ponderacao espacial 
#' 
#' Realiza a ponderacao espacial de precipitacao atraves dos pesos de cada sub_bacia
#'
#' @param historico_precipitacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto plu}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @param postos_plu data.table postos_plu com as colunas
#'     \itemize{
#'     \item{posto}{nome da sub_bacia}
#'     \item{psat}{nome do posto plu}
#'     \item{valor}{peso do posto plu}
#'     }
#' @importFrom data.table setcolorder
#' @return precipitacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{sub_bacia}{nome da sub_bacia}
#'     \item{id}{id da sub_bacia}
#'     \item{valor}{valor da precipitacao ponderada}
#'     }
#' @export
ponderacao_espacial <- function(historico_precipitacao, postos_plu) {

  precipitacao <- historico_precipitacao[posto %in% postos_plu[, posto]]
  precipitacao <- merge(precipitacao, postos_plu, "posto")
  precipitacao <- precipitacao[, valor := sum(valor.x * valor.y), by = data]
  precipitacao <- unique(precipitacao, by = "data")
  precipitacao[, valor.x := NULL]
  precipitacao[, valor.y := NULL]
  precipitacao[, posto := NULL]
  colnames(precipitacao)[2] <- "nome"
  precipitacao <- data.table::setcolorder(precipitacao, c("data", "nome", "valor"))

  precipitacao
}