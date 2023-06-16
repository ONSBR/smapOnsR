
#' transforma_NC_serie
#' 
#' transforma normal climatologica de evapotranspiracao em uma serie temporal
#' 
#' 
#' @param serie_temporal serie temporal a ser utilizada como espelho
#' @param normal_climatologica data.table  com NC de evapotranspiracao com as colunas
#'     \itemize{
#'     \item{mes}{mes da NC}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da NC de evapotranspiracao observada}
#'     }
#' @importFrom data.table data.table
#' @importFrom lubridate month
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da serie temporal}
#'     }
#' @export 
transforma_NC_serie <- function(serie_temporal, normal_climatologica) {
    serie_temporal_etp <- data.table::data.table(serie_temporal)
    serie_temporal_etp[, mes := lubridate::month(data)]
    serie_temporal_etp <- merge(serie_temporal_etp, normal_climatologica, by = "mes")
    serie_temporal_etp[, posto.x := NULL]
    serie_temporal_etp[, id.x := NULL]
    serie_temporal_etp[, valor.x := NULL]
    serie_temporal_etp[, mes := NULL]

    colnames(serie_temporal_etp) <- c("data", "posto", "id", "valor")
    data.table::setorder(serie_temporal_etp, posto, data)
    serie_temporal_etp
}


#' Transforma historico de serie temporal em data table de previsao
#'
#' Transforma historico de serie temporal em data table de previsao
#'
#' @param serie_temporal data.table com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da serie_temporal observada}
#'     }
#' @param datas_rodadas data.table com as colunas
#'     \itemize{
#'     \item{data}{data da rodada}
#'     \item{numero_dias_previsao}{numero de dias de previsao}
#'      }
#' @importFrom data.table data.table setcolorder setorder
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data_rodada}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da previsao}
#'     }
#' @export
transforma_historico_previsao <- function(serie_temporal, datas_rodadas) {

    if (any(colnames(datas_rodadas) != c("data", "numero_dias_previsao"))) {
        stop("data table datas_rodadas deve possuir colunas 'data' e 'numero_dias_previsao'")
    }

    if (!inherits(datas_rodadas[, data], "Date")) {
        stop("data table datas_rodadas deve ter sua primeira coluna com classe 'Date'")
    }

    previsao <- data.table::data.table()

    for (i in 1:nrow(datas_rodadas)) {
        data_rodada <- datas_rodadas$data[i]
        numero_dias_previsao <- datas_rodadas$numero_dias_previsao[i]

        for (j in 1:(numero_dias_previsao + 2)){
            data_previsao <- serie_temporal[data == (data_rodada + j), data]
            aux <- serie_temporal[data %in% data_previsao, ]
            colnames(aux)[1] <- "data_previsao"
            aux[, data_rodada := data_rodada]
            aux[, cenario := "historico"]
            data.table::setcolorder(aux, c("data_rodada", "data_previsao", "cenario", "posto", "id", "valor") )

            previsao <- rbind(previsao, aux)
        }
    }

    data.table::setorder(previsao, data_rodada, id, data_previsao)
    return(previsao)

}

#' Combina data table observado e previsto
#'
#' Combina data table observado e previsto
#'
#' @param observado data.table com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da serie_temporal observada}
#'     }
#' @param previsto data.table com as colunas
#'     \itemize{
#'     \item{data_rodada}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da previsao}
#'     }
#' @importFrom data.table data.table setcolorder setorder
#' @return serie_temporal data.table com as colunas
#'     \itemize{
#'     \item{data_rodada}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da previsao}
#'     }
#' @export
combina_observacao_previsao <- function(observado, previsto){

    result <- lapply(unique(previsto$cenario), function(cenario) {
        dt <- copy(observado)
        dt[, cenario := cenario]
        return(dt)
    })
    serie_temporal <- data.table::rbindlist(result)
    colnames(serie_temporal)[1] <- "data_previsao"

    serie_temporal <- rbind(serie_temporal, previsto)

    data.table::setorder(serie_temporal, data_rodada, cenario, data_previsao, id)
    serie_temporal
}
