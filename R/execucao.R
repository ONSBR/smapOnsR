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

    saida <- rodada_encadeada_oficial(entrada$parametros,
        entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
        entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia)

    if (length(unique(entrada$previsao_precipitacao[, cenario])) == 1) {
        executa_visualizador_previsao(saida$previsoes, saida$assimilacao, saida$precipitacao, 
        saida$funcao_objetivo, entrada$vazao)
    }

    saida
}

#' Executa caso oficial com entrada nova
#' 
#' executa rodada oficial do modelo SMAP/ONS com dados de entrada novos
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

executa_caso_oficial_novo <- function(pasta_entrada){
    entrada <- le_arq_entrada_novo(pasta_entrada)

    saida <- rodada_encadeada_oficial(entrada$parametros,
          entrada$inicializacao, entrada$precipitacao_observada, entrada$precipitacao_prevista, entrada$evapotranspiracao_nc, entrada$vazao,
          entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$precipitacao_prevista[, cenario])), entrada$sub_bacias)

    if (length(unique(entrada$precipitacao_prevista[, cenario])) == 1) {
        executa_visualizador_previsao(saida$previsoes, saida$assimilacao, saida$precipitacao, 
        saida$funcao_objetivo, entrada$vazao)
    }

    saida
}

#' Executa caso com evapotranspiraca diaria
#' 
#' executa rodada com evapotranspiracao diaria do modelo SMAP/ONS com dados de entrada novos
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

executa_caso_etp <- function(pasta_entrada){
    entrada <- le_arq_entrada_novo(pasta_entrada)

    saida <- rodada_encadeada_etp(entrada$parametros,
        entrada$inicializacao, entrada$precipitacao_observada, entrada$precipitacao_prevista, entrada$evapotranspiracao_observada, 
        entrada$evapotranspiracao_prevista, entrada$vazao_observada,
        entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$precipitacao_prevista[, cenario])), entrada$sub_bacias)

    saida
}

####RODADA ENCADEADA NOVO FORMATO
#entrada <- le_arq_entrada_novo(pasta_entrada)
#
#set.seed(129852)
#
#cores <- parallel::detectCores()
#cl <- parallel::makeCluster(cores[1] - 1) #not to overload your computer
#
#parallel::clusterExport(cl, c("entrada",
#              "new_modelo_smap_ons", "combina_observacao_previsao", "assimilacao_oficial",
#              "ponderacao_temporal", "funcao_objetivo_assimilacao_oficial",
#              "transforma_NC_serie", "inicializacao_smap", 
#              "calcula_dm", "calcula_nse", "calcula_mape", "rodada_encadeada_oficial"))
#
#numero_sub_bacias <- length(entrada$sub_bacias$nome)
#tasks <- 1:numero_sub_bacias#
#saida <- parallel::parLapply(cl, tasks, function(ibacia) {
#    parametros <- entrada$parametros
#    inicializacao <- entrada$inicializacao
#    precipitacao_observada <- entrada$precipitacao_observada
#    precipitacao_prevista <- entrada$precipitacao_prevista
#    evapotranspiracao_nc <- entrada$evapotranspiracao_nc
#    vazao_observada <- entrada$vazao_observada
#    postos_plu <- entrada$postos_plu
#    datas_rodadas <- entrada$datas_rodadas[1]
#    nome_cenario <- unique(precipitacao_prevista[, cenario])
#    numero_cenarios <- length(nome_cenario)
#    sub_bacias <- entrada$sub_bacias$nome[ibacia]
#
#    rodada_encadeada_oficial(parametros, inicializacao, precipitacao_observada, 
#            precipitacao_prevista, evapotranspiracao_nc, vazao_observada, postos_plu, datas_rodadas, 
#            numero_cenarios, sub_bacias)
#})
#parallel::stopCluster(cl)#
#previsao <- data.table::rbindlist(lapply(saida, "[[", "previsao"))
#otimizacao <- data.table::rbindlist(lapply(saida, "[[", "otimizacao"))
#assimilacao <- data.table::rbindlist(lapply(saida, "[[", "assimilacao"))
#precipitacao <- data.table::rbindlist(lapply(saida, "[[", "precipitacao"))
#funcao_objetivo <- data.table::rbindlist(lapply(saida, "[[", "funcao_objetivo"))
#
#saida <- list(previsao = previsao, otimizacao = otimizacao, assimilacao = assimilacao, precipitacao = precipitacao,
#    funcao_objetivo = funcao_objetivo)
#
######RODADA paralelo ETP
#entrada <- le_arq_entrada_novo(pasta_entrada)
#
#cores <- parallel::detectCores()
##cl <- parallel::makeCluster(cores[1] - 1) #not to overload your computer
#parallel::clusterExport(cl, c("entrada", "rodada_encadeada_etp"))
#
#numero_sub_bacias <- length(entrada$sub_bacias$nome[1])
#
#tasks <- 1:numero_sub_bacias
#saida <- parallel::parLapply(cl, tasks, function(ibacia) {
#    parametros <- entrada$parametros
#    inicializacao <- entrada$inicializacao
#    precipitacao_observada <- entrada$precipitacao_observada
#    precipitacao_prevista <- entrada$precipitacao_prevista
#    evapotranspiracao_observada <- entrada$evapotranspiracao_observada
#    evapotranspiracao_prevista <- entrada$evapotranspiracao_prevista
#    vazao_observada <- entrada$vazao_observada
#    postos_plu <- entrada$postos_plu
#    datas_rodadas <- entrada$datas_rodadas[1]
#    nome_cenario <- unique(precipitacao_prevista[, cenario])
#    numero_cenarios <- length(nome_cenario)
#    sub_bacias <- entrada$sub_bacias$nome[ibacia]
#    
#    rodada_encadeada_etp(parametros, inicializacao, precipitacao_observada,
#            precipitacao_prevista, evapotranspiracao_observada, evapotranspiracao_prevista,
#            vazao_observada, postos_plu, datas_rodadas, numero_cenarios, sub_bacias)
#})
#parallel::stopCluster(cl)
#previsao <- data.table::rbindlist(lapply(saida, "[[", "previsao"))
#otimizacao <- data.table::rbindlist(lapply(saida, "[[", "otimizacao"))
#assimilacao <- data.table::rbindlist(lapply(saida, "[[", "assimilacao"))
#precipitacao <- data.table::rbindlist(lapply(saida, "[[", "precipitacao"))
#funcao_objetivo <- data.table::rbindlist(lapply(saida, "[[", "funcao_objetivo"))
#
#saida <- list(previsao = previsao, otimizacao = otimizacao, assimilacao = assimilacao, precipitacao = precipitacao,
#    funcao_objetivo = funcao_objetivo)
#    
#
#process_sub_bacia <- function(sub_bacia) {
#  # Call rodada_encadeada_etp for the specific sub_bacia
#  resultado <- rodada_encadeada_etp(parametros, inicializacao, precipitacao_observada,
#                                    precipitacao_prevista, evapotranspiracao_observada,
#                                    evapotranspiracao_prevista, vazao_observada, postos_plu,
#                                    datas_rodadas, numero_cenarios, sub_bacia)
#  
#  # Return the result
#  return(resultado)
#}
#
#parametros <- entrada$parametros
#inicializacao <- entrada$inicializacao
#precipitacao_observada <- entrada$precipitacao_observada
#precipitacao_prevista <- entrada$precipitacao_prevista
#evapotranspiracao_observada <- entrada$evapotranspiracao_observada
#evapotranspiracao_prevista <- entrada$evapotranspiracao_prevista
#vazao_observada <- entrada$vazao_observada
#postos_plu <- entrada$postos_plu
#datas_rodadas <- entrada$datas_rodadas[1]
#nome_cenario <- unique(precipitacao_prevista[, cenario])
#numero_cenarios <- length(nome_cenario)
#sub_bacias <- entrada$sub_bacias$nome
## Create a cluster object with the desired number of cores/workers
#
#cores <- parallel::detectCores()
#cl <- parallel::makeCluster(cores[1] - 1) #not to overload your computer
#
## Set up the parallel backend
#parallel::clusterSetRNGStream(cl)
#
## Export the necessary variables and functions to the parallel workers
#parallel::clusterExport(cl, c("parametros", "inicializacao", "precipitacao_observada", "precipitacao_prevista",
#                    "evapotranspiracao_observada", "evapotranspiracao_prevista", "vazao_observada",
#                    "postos_plu", "datas_rodadas", "numero_cenarios", 
#                    "new_modelo_smap_ons", "combina_observacao_previsao", "assimilacao_evapotranspiracao",
#              "ponderacao_temporal", "funcao_objetivo_assimilacao_evapotranspiracao",
#              "transforma_NC_serie", "inicializacao_smap", 
#              "calcula_dm", "calcula_nse", "calcula_mape", "rodada_encadeada_etp",
#                    "process_sub_bacia"))
#
## Use parLapply to process each sub_bacia in parallel
#results <- parallel::parLapply(cl, sub_bacias, process_sub_bacia)
#
## Stop the cluster
#parallel::stopCluster(cl)
#
## Merge the results, assuming each result is a list with the same structure
#saida <- do.call(rbind, lapply(results, "[[", "saida"))
#saida_ajuste_otimizacao <- do.call(rbind, lapply(results, "[[", "ajuste_otimizacao"))
#saida_ajuste_assimilacao <- do.call(rbind, lapply(results, "[[", "ajuste_assimilacao"))
#saida_precipitacao <- do.call(rbind, lapply(results, "[[", "precipitacao"))
#