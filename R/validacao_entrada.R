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

    valida_duplicadas(evapotranspiracao_prevista, colunas_chave = 
            c("data_rodada", "data_previsao", "nome", "cenario"),
            nome_dt = "evapotranspiracao_prevista")

    valida_cenarios(evapotranspiracao_prevista, precipitacao_prevista)
}

#' valida_previsao_prec
#' 
#' Validador de arquivo de previsao de prec
#' 
#' @param precipitacao_prevista data.table com a precipitacao prevista com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada do modelo que gerou a previsao}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - codigo do cenario}
#'     \item{nome - nome da sub bacia}
#'     \item{valor - valor da previsao de precipitacao}
#'     }
#' @export 
valida_previsao_prec <- function(precipitacao_prevista) {
    valida_duplicadas(precipitacao_prevista, colunas_chave = 
            c("data_rodada", "data_previsao", "nome", "cenario"),
            nome_dt = "precipitacao_prevista")
}

#' valida_duplicadas
#' 
#' Valida se existe linhas duplicadas no data.table
#' @param precipitacao_prevista data.table com a precipitacao prevista com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada do modelo que gerou a previsao}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - codigo do cenario}
#'     \item{nome - nome da sub bacia}
#'     \item{valor - valor da previsao de precipitacao}
#'     }
#' @param colunas_chave vetor com os nomes das colunas que compoem a chave unica
#' @param nome_dt nome do data.table (string) para usar na mensagem de erro
#' @export 

valida_duplicadas <- function(dt, colunas_chave, nome_dt) {

    chaves_duplicadas <- dt[, .N, by = colunas_chave][N > 1]

    if (nrow(chaves_duplicadas) > 0) {
        linhas_problematicas <- dt[chaves_duplicadas, on = colunas_chave, nomatch = 0]
        
        stop(paste0(
            "As seguintes linhas no data.table ", nome_dt, " estão duplicadas:",
            paste(capture.output(print(unique(linhas_problematicas))), 
            collapse = "\n"), sep = "\n")
        )
    }
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

    etp <- data.table::copy(evapotranspiracao_prevista)
    etp[, horizonte := as.numeric(data_previsao - data_rodada)]
    etp <- etp[horizonte != 0]
    merged_data <- merge(precipitacao_prevista, etp,
                     by = c("data_rodada", "data_previsao", "nome", "cenario"), all = TRUE)

    if (nrow(merged_data[is.na(valor.x)]) != 0) {
        stop(paste0("falta o cenario ", merged_data[is.na(valor.x), cenario], " para a data de previsao ", 
        merged_data[is.na(valor.x), data_previsao], " da sub-bacia ", merged_data[is.na(valor.x), nome],
        " do caso de ", merged_data[is.na(valor.x), data_rodada], " no arquivo de previsao de precipitacao. \n"))
    }

    if (nrow(merged_data[is.na(valor.y)]) != 0) {
        stop(paste0("falta o cenario ", merged_data[is.na(valor.y), cenario], " para a data de previsao ", 
        merged_data[is.na(valor.y), data_previsao], " da sub-bacia ", merged_data[is.na(valor.y), nome],
        " do caso de ", merged_data[is.na(valor.y), data_rodada], " no arquivo de previsao de evapotranspiracao. \n"))
    }
}