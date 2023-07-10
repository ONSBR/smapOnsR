#' Executa caso oficial
#' 
#' executa rodada oficial do modleo SMAP/ONS
#'
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @return saida lista com o data.table previsao contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{nome}{nome da sub-bacia}
#'     \item{variavel}{nome da variavel}
#'     \item{valor}{valor da variavel}
#'     }
#'  otimizacao data table com as colunas:
#' \itemize{
#'     \item{otimizacao}{valor das variaveis da otimizacao}
#'     \item{nome}{nome da sub-bacia}
#'     \item{data_caso}{data do caso}
#'     }
#'  assimilacao data table com as colunas:
#' \itemize{
#'     \item{data_caso}{data da rodada}
#'     \item{data_assimilacao}{data da assimilacao}
#'     \item{cenario}{nome do cenario}
#'     \item{nome}{nome da sub-bacia}
#'     \item{variavel}{nome da variavel}
#'     \item{valor}{valor da variavel}
#'     }
#' precipitacao data table contendo precipitacao observada e previsata com as colunas:
#' \itemize{
#'     \item{data_previsao}{data da precipitacao}
#'     \item{data_rodada}{data da rodada}
#'     \item{cenario}{nome do cenario}
#'     \item{nome}{nome da sub-bacia}
#'     \item{variavel}{nome da variavel}
#'     \item{valor}{valor da variavel}
#'     }
#' @export

executa_caso_oficial <- function(pasta_entrada){
    entrada <- le_arq_entrada(pasta_entrada)

    set.seed(129852)

    saida <- rodada_encadeada_oficial(entrada$parametros,
      entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
      entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia)

    saida
}

# RODADA em paralelo
#        entrada <- le_arq_entrada(pasta_entrada)
#        cores <- parallel::detectCores()
#        cl <- parallel::makeCluster(cores[1] - 1) #not to overload your computer
#        
#        parallel::clusterExport(cl, c("entrada",
#                      "new_modelo_smap_ons", "combina_observacao_previsao", "assimilacao_oficial",
#                      "ponderacao_temporal", "funcao_objetivo_assimilacao_oficial",
#                      "transforma_NC_serie", "inicializacao_smap", 
#                      "calcula_dm", "calcula_nse", "calcula_mape", "rodada_encadeada_oficial"))
#        
#        numero_sub_bacias <- length(entrada$caso$nome_subbacia)
#        tasks <- 1:numero_sub_bacias#

#        saida <- parallel::parLapply(cl, tasks, function(ibacia) {
#            parametros <- entrada$parametros
#            inicializacao <- entrada$inicializacao
#            precipitacao_observada <- entrada$precipitacao
#            previsao_prevista <- entrada$previsao_precipitacao
#            evapotranspiracao_nc <-entrada$evapotranspiracao
#            vazao_observada <- entrada$vazao
#            postos_plu <- entrada$postos_plu
#            datas_rodadas <- entrada$datas_rodadas
#            nome_cenario <- unique(previsao_prevista[, cenario])
#            numero_cenarios <- length(nome_cenario)
#            sub_bacias <- entrada$caso$nome_subbacia[ibacia]
#            numero_datas <- nrow(datas_rodadas)    #

#            rodada_encadeada_oficial(parametros, inicializacao, precipitacao_observada, 
#                previsao_prevista, evapotranspiracao_nc, vazao_observada, postos_plu, datas_rodadas, 
#                numero_cenarios, sub_bacias)
#                
#        })
#        parallel::stopCluster(cl)#

#        previsao <- data.table::rbindlist(lapply(saida, "[[", "previsao"))
#        otimizacao <- data.table::rbindlist(lapply(saida, "[[", "otimizacao"))
#        assimilacao <- data.table::rbindlist(lapply(saida, "[[", "assimilacao"))
#        precipitacao <- data.table::rbindlist(lapply(saida, "[[", "precipitacao"))
#        funcao_objetivo <- data.table::rbindlist(lapply(saida, "[[", "funcao_objetivo"))
#        
#        saida <- list(previsao = previsao, otimizacao = otimizacao, assimilacao = assimilacao, precipitacao = precipitacao,
#            funcao_objetivo = funcao_objetivo)