
#' Totaliza previsoes
#' 
#' Funcao para totalizar as previsoes do SMAP/ONS, atraves de metodos de propagacao
#' 
#' @param previsao data.table previsao contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' @param vazao_observada data.table com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da precipitacao observada}
#'     }
#' @param configuracao data.table com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da precipitacao observada}
#'     }
#' @return previsao_totalizada - data.table contendo as seguintes colunas
#' \itemize{
#'     \item{data_rodada - data em que o caso foi executado}
#'     \item{data_previsao - data da previsao realizada}
#'     \item{nome - nome da UHE}
#'     \item{cenario - nome do cenario}
#'     \item{previsao_distribuida - valor da vazao apos o processo de distribuicao de incrementais agrupadas previstas}
#'     \item{previsao_incremental - valor da vazao incremental prevista}
#'     \item{previsao_total - valor da vazao total prevista, considerando o tempo de viagem entre as UHES}
#'     \item{previsao_total_sem_tv - valor da vazao total prevista, sem copnsiderar o tempo de viagem entre as UHES}
#'     }
#' @examples
#' \dontrun{
#'   zip::unzip(system.file("extdata", "dados_entrada.zip", package = "smapOnsR"), exdir = system.file("extdata", package = "smapOnsR"))
#'
#' pasta_entrada <- system.file("extdata", "caso_completo2", "Arq_Entrada", package = "smapOnsR")
#' pasta_saida <- system.file("extdata", "caso_completo2", "Arq_Saida", package = "smapOnsR")
#'
#' entrada <- le_arq_entrada(pasta_entrada)
#' execucao <- le_execucao(pasta_saida, entrada$datas_rodadas$data)
#' 
#' set.seed(129852)
#' saida <- rodada_sem_assimilacao(entrada$parametros,
#'     entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, entrada$evapotranspiracao, entrada$vazao,
#'     entrada$postos_plu, entrada$datas_rodadas, length(unique(entrada$previsao_precipitacao[, cenario])), entrada$caso$nome_subbacia, execucao)
#'
#' configuracao <- data.table::fread(file.path(system.file("extdata", "arq_entrada_novo", package = "smapOnsR"), "configuracao.csv"))
#' configuracao[, sub_bacia_agrupada := tolower(sub_bacia_agrupada)]
#' configuracao <- configuracao[sub_bacia_agrupada %in% entrada$caso$nome]
#' vazao_observada <- entrada$vazao
#'
#'  previsao_totalizada <- totaliza_previsao(saida$previsao[variavel == "Qcalc"], vazao_observada, configuracao)
#' }
#' @export
totaliza_previsao <- function(previsao, vazao_observada, configuracao) {

    #combina obs e prev
    obs <- data.table::copy(vazao_observada)
    colnames(previsao)[1] <- "data_caso"
    obs[, data_rodada := previsao[, unique(data_caso)]]
    data.table::setnames(obs, "posto", "nome")
    previsao[, variavel := NULL]
    data.table::setnames(previsao, "data_caso", "data_rodada")
    previsao <- combina_observacao_previsao(obs, previsao)
    previsao <- previsao[data_previsao >= (data_rodada - 90)]
    data.table::setorder(previsao, "data_rodada", "nome", "cenario", "data_previsao")

    #distribui incrementais
    # Inicializa uma lista para acumular os data.tables
    resultados <- vector("list", length(configuracao$posto))

    # Loop otimizado
    for (i in seq_along(configuracao$posto)) {
        posto_total <- configuracao$posto[i]
        sub_bacia_agrupada <- configuracao[posto == posto_total, sub_bacia_agrupada]
        fator <- configuracao[posto == posto_total, fator]
        nome_real <- configuracao[posto == posto_total, nome_real]

        # Copia e faz os ajustes no data.table
        previsao_totalizada_subbacia <- data.table::copy(previsao[nome == sub_bacia_agrupada])
        previsao_totalizada_subbacia[, valor := valor * fator]
        previsao_totalizada_subbacia[, nome := nome_real]
        previsao_totalizada_subbacia[, posto := posto_total]

        # Armazena o resultado em uma lista
        resultados[[i]] <- previsao_totalizada_subbacia
    }

    # Junta todos os data.tables de uma vez
    previsao_totalizada <- data.table::rbindlist(resultados)
    data.table::setnames(previsao_totalizada, "valor", "previsao_incremental")
    data.table::setorder(previsao_totalizada, "data_rodada", "nome", "cenario", "data_previsao")

    # propaga postos flu
    if(nrow(configuracao[bacia_smap == "posto_flu"]) > 0) {
        configuracao_postos_plu <- configuracao[bacia_smap == "posto_flu"]
        ordem <- ordem_afluencia(configuracao_postos_plu$posto, configuracao_postos_plu$posto_jusante)
        for (indice_ordem in 1:max(ordem)) {
            indice_configuracao <- which(ordem == indice_ordem)
            for (indice_usina in indice_configuracao) {
                nome_montante <- configuracao_postos_plu[indice_usina, nome_real]
                nome_jusante <- configuracao[posto == configuracao_postos_plu[indice_usina, posto_jusante], nome_real]
                jusante <- previsao_totalizada[nome == nome_jusante]
                montante <- previsao_totalizada[nome == nome_montante]
                if (configuracao_postos_plu[nome_real == nome_montante, n] == 0){
                    tv <- configuracao_postos_plu[nome_real == nome_montante, tv]
                    propagada <- funcaoSmapCpp::propaga_tv_cpp(montante[, previsao_incremental], jusante[, previsao_incremental], tv)
                    previsao_totalizada[nome == nome_jusante, previsao_incremental := propagada]
                } else {
                    n <- configuracao_postos_plu[nome_real == nome_montante, n]
                    coeficientes <- c(0, 0, 0)
                    coeficientes[1] <- configuracao_postos_plu[nome_real == nome_montante, c1]
                    coeficientes[2] <- configuracao_postos_plu[nome_real == nome_montante, c2]
                    coeficientes[3] <- configuracao_postos_plu[nome_real == nome_montante, c3]
                    propagada <- funcaoSmapCpp::propaga_muskingum_cpp(montante[, previsao_incremental], jusante[, previsao_incremental], n, coeficientes)
                    previsao_totalizada[nome == nome_jusante, previsao_incremental := propagada]
                }
            }
        }
    }

    
    # propaga subbacias
    configuracao_sem_postos_plu <- configuracao[!bacia_smap == "posto_flu"]
    previsao_totalizada[, previsao_total := previsao_incremental]
    previsao_totalizada[, previsao_total_sem_tv := previsao_incremental]
    ordem <- ordem_afluencia(configuracao_sem_postos_plu$posto, configuracao_sem_postos_plu$posto_jusante)
    for (indice_ordem in 1:max(ordem)) {
        indice_configuracao <- which(ordem == indice_ordem)
        for (indice_usina in indice_configuracao) {
            nome_montante <- configuracao_sem_postos_plu[indice_usina, nome_real]
            nome_jusante <- configuracao[posto == configuracao_sem_postos_plu[indice_usina, posto_jusante], nome_real]
            if (length(nome_jusante) != 0) {
                jusante <- previsao_totalizada[nome == nome_jusante]
                montante <- previsao_totalizada[nome == nome_montante]
                if (configuracao_sem_postos_plu[nome_real == nome_montante, n] == 0){
                    tv <- configuracao_sem_postos_plu[nome_real == nome_montante, tv]
                    propagada <- funcaoSmapCpp::propaga_tv_cpp(montante[, previsao_total], jusante[, previsao_total], tv)
                    previsao_totalizada[nome == nome_jusante, previsao_total := propagada]
                    previsao_totalizada[nome == nome_jusante, previsao_total_sem_tv := 
                                        montante[, previsao_total_sem_tv] + jusante[, previsao_total_sem_tv]]
                } else {
                    n <- configuracao_sem_postos_plu[nome_real == nome_montante, n]
                    coeficientes <- c(0, 0, 0)
                    coeficientes[1] <- configuracao_sem_postos_plu[nome_real == nome_montante, c1]
                    coeficientes[2] <- configuracao_sem_postos_plu[nome_real == nome_montante, c2]
                    coeficientes[3] <- configuracao_sem_postos_plu[nome_real == nome_montante, c3]
                    propagada <- funcaoSmapCpp::propaga_muskingum_cpp(montante[, previsao_total], jusante[, previsao_total], n, coeficientes)
                    previsao_totalizada[nome == nome_jusante, previsao_total := propagada]
                    previsao_totalizada[nome == nome_jusante, previsao_total_sem_tv := 
                                    montante[, previsao_total_sem_tv] + jusante[, previsao_total_sem_tv]]
                }
            }
        }
    }


    previsao_totalizada[, previsao_inc_tv := previsao_total]
    previsao_totalizada <- previsao_totalizada[data_previsao >= data_rodada]
    for (indice_ordem in 1:max(ordem)) {
        indice_configuracao <- which(ordem == indice_ordem)
        for (indice_usina in indice_configuracao) {
            nome_montante <- configuracao_sem_postos_plu[indice_usina, nome_real]
            nome_jusante <- configuracao[posto == configuracao_sem_postos_plu[indice_usina, posto_jusante], nome_real]
            if (length(nome_jusante) != 0) {
                jusante <- previsao_totalizada[nome == nome_jusante, previsao_inc_tv]
                montante <- previsao_totalizada[nome == nome_montante, previsao_total]

                previsao_totalizada[nome == nome_jusante, 
                            previsao_inc_tv := jusante - montante]
            }
        }
    }

    previsao_totalizada <- previsao_totalizada[nome %in% configuracao[!bacia_smap == "posto_flu", nome_real]]
    
    previsao_totalizada
}