#' Funcao de ponderacao temporal 
#' 
#' Realiza a ponderacao temporal de variaveis atraves dos pesos kt
#'
#' @param serie_temporal vetor com a variavel a ser ponderada
#' @param kt vetor de kts
#' @param kt_max valor do maximo lag positivo
#' @param kt_min valor do maximo lag maximo negativo
#' @examples 
#' sub_bacia <- "avermelha"
#' modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome == sub_bacia])
#' kt <- unlist(modelo)[12:74]
#' kt_min <- sum(kt[4:63] > 0)
#' kt_max <- sum(kt[1:2] > 0)
#' data_inicio <- as.Date("2021-12-01")
#' data_fim <- as.Date("2021-12-29")
#'
#' serie_temporal <- historico_precipitacao[posto == "psatagv" &  data >= data_inicio - kt_min & data <= data_fim + kt_max, valor]
#'
#' saida <- ponderacao_temporal(serie_temporal, kt, kt_max, kt_min)
#' @importFrom zoo rollapply
#' @importFrom stats weighted.mean
#' @return serie_temporal_ponderada: vetor com a serie temporal ponderada pelos kts
#' @export
ponderacao_temporal <- function(serie_temporal, kt, kt_max, kt_min) {

  N <- length(kt[(3 - kt_max):(3 + kt_min)])
  serie_temporal_ponderada <- zoo::rollapply(serie_temporal, N, function(v) stats::weighted.mean(v, kt[(3 + kt_min):(3 - kt_max)]), align = "right")
  serie_temporal_ponderada

}
#' Funcao de ponderacao espacial 
#' 
#' Realiza a ponderacao espacial de precipitacao atraves dos pesos de cada sub_bacia
#'
#' @param historico_precipitacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto plu}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param postos_plu data.table postos_plu com as colunas
#'     \itemize{
#'     \item{posto - nome da sub_bacia}
#'     \item{psat - nome do posto plu}
#'     \item{valor - peso do posto plu}
#'     }
#' @examples 
#' sub_bacia <- "pimentalt"
#' modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome == sub_bacia])
#' kt <- unlist(modelo)[12:74]
#' kt_min <- sum(kt[4:63] > 0)
#' kt_max <- sum(kt[1:2] > 0)
#' data_inicio <- as.Date("2021-12-01")
#' data_fim <- as.Date("2021-12-29")
#' precipitacao <- historico_precipitacao[data <= (data_fim + kt_max) &
#'           data >= (data_inicio - kt_min) & posto %in% postos_plu[nome == sub_bacia, posto]]
#'
#' precipitacao_ponderada <- ponderacao_espacial(precipitacao, postos_plu[nome == sub_bacia])
#' @importFrom data.table setcolorder
#' @return precipitacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{sub_bacia - nome da sub_bacia}
#'     \item{id - id da sub_bacia}
#'     \item{valor - valor da precipitacao ponderada}
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

#' Funcao de ponderacao espacial 
#' 
#' Realiza a ponderacao espacial de previsao de precipitacao atraves dos pesos de cada sub_bacia
#'
#' @param precipitacao_prevista data.table com a precipitacao prevista por posto pluviometrico com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada do modelo que gerou a previsao}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - codigo do cenario}
#'     \item{posto - nome do posto pluviometrico}
#'     \item{valor - valor da previsao de precipitacao}
#'     }
#' @param postos_plu data.table postos_plu com as colunas
#'     \itemize{
#'     \item{posto}{nome da sub_bacia}
#'     \item{psat}{nome do posto plu}
#'     \item{valor}{peso do posto plu}
#'     }
#' @importFrom data.table setcolorder
#' @return data.table com a precipitacao prevista por sub-bacia com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada do modelo que gerou a previsao}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - codigo do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{valor - valor da previsao de precipitacao}
#'     }
#' @export
ponderacao_espacial_previsao <- function(precipitacao_prevista, postos_plu) {

  precipitacao <- precipitacao_prevista[posto %in% postos_plu[, posto]]
  precipitacao <- merge(precipitacao, postos_plu, "posto")
  precipitacao <- precipitacao[, valor := sum(valor.x * valor.y), 
                  by = c("data_rodada", "data_previsao", "cenario", "posto")]
  precipitacao <- unique(precipitacao, by = c("data_rodada", "data_previsao", "cenario", "posto"))
  precipitacao[, valor.x := NULL]
  precipitacao[, valor.y := NULL]
  precipitacao[, posto := NULL]
  
  precipitacao <- data.table::setcolorder(precipitacao, c("data_rodada", "data_previsao", "nome", "cenario", "valor"))

  precipitacao
}