# VALIDACAO DE ENTRADA 

#' valida_previsao_etp
#' 
#' Validador de arquivo de previsao de etp
#' 
#' @param evapotranspiracao_prevista data.table com a evapotranspiracao prevista com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada do modelo que gerou a previsao}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - codigo do cenario}
#'     \item{nome - nome da sub bacia}
#'     \item{valor - valor da previsao de precipitacao}
#'     }
#' @param precipitacao_prevista data.table com a precipitacao prevista com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada do modelo que gerou a previsao}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - codigo do cenario}
#'     \item{nome - nome da sub bacia}
#'     \item{valor - valor da previsao de precipitacao}
#'     }
#' @export 
valida_previsao_etp <- function(evapotranspiracao_prevista, precipitacao_prevista) {

    valida_cenarios(evapotranspiracao_prevista, precipitacao_prevista)
}

#' valida_cenarios
#' 
#' Valida se para cada valor unico de nome, data_previsao e data_rodada existem os mesmos
#' cenarios
#' 
#' @param evapotranspiracao_prevista data.table com a evapotranspiracao prevista com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada do modelo que gerou a previsao}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - codigo do cenario}
#'     \item{nome - nome da sub bacia}
#'     \item{valor - valor da previsao de precipitacao}
#'     }
#' @param precipitacao_prevista data.table com a precipitacao prevista com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada do modelo que gerou a previsao}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - codigo do cenario}
#'     \item{nome - nome da sub bacia}
#'     \item{valor - valor da previsao de precipitacao}
#'     }
#' @export 
valida_cenarios <- function(evapotranspiracao_prevista, precipitacao_prevista) {

    merged_data <- merge(precipitacao_prevista, evapotranspiracao_prevista,
                     by = c("data_previsao", "nome", "data_rodada", "cenario"), all = TRUE)

    if (nrow(merged_data[is.na(valor.x)]) != 0) {
        stop(paste0("falta o cenario ", merged_data$cenario, " para a data de previsao ", 
        merged_data$data_previsao, " da sub-bacia ", merged_data$nome, 
        " do caso de ", merged_data$data_rodada, " no arquivo de previsao de precipitacao. \n"))
    }

    if (nrow(merged_data[is.na(valor.y)]) != 0) {
        stop(paste0("falta o cenario ", merged_data$cenario, " para a data de previsao ", 
        merged_data$data_previsao, " da sub-bacia ", merged_data$nome, 
        " do caso de ", merged_data$data_rodada, " no arquivo de previsao de evapotranspiracao. \n"))
    }
}