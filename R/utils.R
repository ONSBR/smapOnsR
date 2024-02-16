#' transforma_NC_serie
#' 
#' transforma normal climatologica de evapotranspiracao em uma serie temporal
#' 
#' 
#' @param serie_temporal serie temporal a ser utilizada como espelho
#' @param normal_climatologica data.table com NC de evapotranspiracao com as colunas
#'     \itemize{
#'     \item{mes - mes da NC}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da NC de evapotranspiracao observada}
#'     }
#' @importFrom data.table data.table setnames setorder
#' @importFrom lubridate month
#' @examples
#' nome2 <- "baixoig"
#' normal_climatologica <- historico_etp_NC[nome == nome2]
#' serie_temporal <- historico_precipitacao[posto == postos_plu[nome == nome2, posto]]
#' serie_temporal <- ponderacao_espacial(serie_temporal, postos_plu[nome == nome2])
#' serie_temporal_NC <- transforma_NC_serie(serie_temporal, normal_climatologica)
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da serie temporal}
#'     }
#' @export 
transforma_NC_serie <- function(serie_temporal, normal_climatologica) {
    serie_temporal_etp <- data.table::data.table(serie_temporal)
    serie_temporal_etp[, mes := lubridate::month(data)]
    serie_temporal_etp <- merge(serie_temporal_etp, normal_climatologica, by = "mes")
    serie_temporal_etp[, nome.x := NULL]
    serie_temporal_etp[, valor.x := NULL]
    serie_temporal_etp[, mes := NULL]

    data.table::setnames(serie_temporal_etp, old = c("nome.y", "valor.y"), new = c("nome", "valor"))
    data.table::setorder(serie_temporal_etp, nome, data)
    serie_temporal_etp
}


#' Transforma historico de serie temporal em data table de previsao
#'
#' Transforma historico de serie temporal em data table de previsao
#'
#' @param serie_temporal data.table com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da serie_temporal observada}
#'     }
#' @param datas_rodadas data.table com as colunas
#'     \itemize{
#'     \item{data - data da rodada}
#'     \item{numero_dias_previsao - numero de dias de previsao}
#'      }
#' @importFrom data.table data.table setcolorder setorder
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{valor - valor da previsao}
#'     }
#' @examples 
#' sub_bacia <- "avermelha"
#' precipitacao <- historico_precipitacao[posto %in% postos_plu[nome == sub_bacia, posto]]
#' precipitacao <- ponderacao_espacial(precipitacao, postos_plu[nome == sub_bacia])
#' datas_rodadas <- data.table::data.table(
#'   data = as.Date(c("2020-05-01", "2020-05-08")),
#'   numero_dias_previsao = c(15, 20)
#' )
#' 
#' previsao <- transforma_historico_previsao(precipitacao, datas_rodadas)
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

        data_previsao <- serie_temporal[data %in% seq.Date(data_rodada + 1, data_rodada + numero_dias_previsao + 2, by = 1)]
        aux <- serie_temporal[data %in% data_previsao[, data]]
        colnames(aux)[1] <- "data_previsao"
        aux[, data_rodada := data_rodada]
        aux[, cenario := "historico"]

        previsao <- data.table::rbindlist(list(previsao, aux))
    }
    data.table::setcolorder(aux, c("data_rodada", "data_previsao", "cenario", "nome", "valor"))
    data.table::setorder(previsao, data_rodada, nome, data_previsao)
    return(previsao)
}

#' Combina data table observado e previsto
#'
#' Combina data table observado e previsto
#'
#' @param observado data.table com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da serie_temporal observada}
#'     }
#' @param previsto data.table com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da previsao}
#'     }
#' @importFrom data.table data.table setcolorder setorder
#' @return serie_temporal data.table com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da previsao}
#'     }
#' @export
combina_observacao_previsao <- function(observado, previsto){

    result <- lapply(unique(previsto$cenario), function(cenario) {
        dt <- data.table::copy(observado)
        dt[, cenario := cenario]
        return(dt)
    })
    serie_temporal <- data.table::rbindlist(result)
    colnames(serie_temporal)[1] <- "data_previsao"

    serie_temporal <- rbind(serie_temporal, previsto)

    data.table::setorder(serie_temporal, data_rodada, cenario, data_previsao)
    serie_temporal
}


#' Completa as previsoes com mais n dias
#'
#' Completa o data table de precisoes com mais N dias, para a ponderacao temporal ate t+2
#'
#' @param datas_rodadas data table com as colunas
#'     \itemize{
#'     \item{data - data da rodada}
#'     \item{numero_dias_previsao - numero de dias de previsão}
#'     }
#' @param precipitacao_prevista data.table com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{valor - valor da previsao}
#'     }
#' @param numero_dias numero de dias alem da data final da previsao a ser verificado
#' @importFrom data.table data.table setcolorder setorder setnames CJ
#' @return precipitacao_prevista data.table com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{valor - valor da previsao}
#'     }
#' @export
completa_previsao <- function(precipitacao_prevista, datas_rodadas, numero_dias = 2){
    result_list <- list()
    
    for (i in seq_len(nrow(datas_rodadas))) {
      
      current_data_rodada <- datas_rodadas[i, ]
      
      if (any(precipitacao_prevista[data_rodada == current_data_rodada$data, as.numeric(max(data_previsao) - 
                          (current_data_rodada$data + current_data_rodada$numero_dias_previsao - 1 + numero_dias)), by = .(nome, cenario)]$V1 <= 0)) {
        
        mean_values <- precipitacao_prevista[, .(mean_valor = mean(valor)), by = .(nome, data_rodada, cenario)]
        
        unique_combinations <- unique(precipitacao_prevista[data_rodada == current_data_rodada$data, .(nome, data_rodada, cenario)])
        
        datas <- seq.Date(current_data_rodada$data + 1,
                          current_data_rodada$data + current_data_rodada$numero_dias_previsao + numero_dias, by = 1)
        
        all_combinations <- data.table::CJ(nome = unique_combinations$nome, cenario = unique_combinations$cenario, data_previsao = datas)
        
        missing_forecasts <- all_combinations[!precipitacao_prevista, on = c("nome", "cenario", "data_previsao")]
        
        missing_forecasts <- mean_values[missing_forecasts, on = .(nome, cenario)]
        
        missing_forecasts[, data_rodada := current_data_rodada$data]
        
        data.table::setnames(missing_forecasts, "mean_valor", "valor")
        
        missing_forecasts <- unique(missing_forecasts)
        
        data.table::setcolorder(missing_forecasts, c("data_rodada", "data_previsao", "cenario", "nome", "valor"))
        
        result_list[[i]] <- missing_forecasts
        
        precipitacao_prevista <- data.table::rbindlist(list(precipitacao_prevista, result_list[[i]]), use.names = TRUE)
      }
      
    }
    
    data.table::setorder(precipitacao_prevista, nome, data_rodada, data_previsao, cenario)
    
    precipitacao_prevista
}

#' Ordem de afluencia
#'
#' @param montante Nomes ou IDs das UHEs de montante da cascata
#' @param jusante Nomes ou IDs das UHEs imediatamente a jusante de cada UHE
#'
#' @return Retorna a ordem de afluencia para cada UHE
#' @export
ordem_afluencia <- function(montante, jusante) {
  # Vetor com ordens de afluencia
  ord_aflu <- rep(NA, length(montante))
  
  # UHES qua nao sao jusantes de nenhuma outra (nao tem montantes)
  tmp <- which(!is.element(montante,jusante))
  ord <- 1
  ord_aflu[tmp] <- ord
  
  # Ate preencher as ordens de todos montante
  while (any(is.na(ord_aflu))) {
    ord <- ord + 1
    uhe_c_ord <- montante[!is.na(ord_aflu)]
    uhe_s_ord <- montante[is.na(ord_aflu)]
    
    for(u in uhe_s_ord){
      # UHEs imediatamente a montande de u
      m.u <- montante[is.element(jusante, u)]
      
      # Se todos os montantes imediatos ja tiverem ordem determinada, u recebe a ordem seguinte
      if (all(is.element(m.u, uhe_c_ord))) {
        tmp <- which(montante == u)
        ord_aflu[tmp] <- ord
      }
    }
  }
  return(ord_aflu)
}