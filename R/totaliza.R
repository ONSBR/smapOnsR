
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
    obs[, data_rodada := previsao[, unique(data_caso)]]
    data.table::setnames(obs, "posto", "nome")
    previsao_caso <- data.table::copy(previsao)
    previsao_caso[, variavel := NULL]
    data.table::setnames(previsao_caso, "data_caso", "data_rodada")
    previsao_caso <- combina_observacao_previsao(obs, previsao_caso)
    data.table::setorder(previsao_caso, "data_rodada", "nome", "cenario", "data_previsao")

    #distribui incrementais
    previsao_totalizada <- data.table::data.table()
    for (posto_total in configuracao$posto) {
        previsao_totalizada_subbacia <- data.table::copy(previsao_caso[nome == configuracao[posto == posto_total, sub_bacia_agrupada]])
        previsao_totalizada_subbacia[nome == configuracao[posto == posto_total, sub_bacia_agrupada], valor := valor * configuracao[posto == posto_total, fator]]
        previsao_totalizada_subbacia[nome == configuracao[posto == posto_total, sub_bacia_agrupada], nome := configuracao[posto == posto_total, nome_real]]   
        previsao_totalizada <- data.table::rbindlist(list(previsao_totalizada, previsao_totalizada_subbacia))
    }
    data.table::setnames(previsao_totalizada, "valor", "previsao_distribuida")
    previsao_totalizada[, previsao_incremental := previsao_distribuida]
    data.table::setorder(previsao_totalizada, "data_rodada", "nome", "cenario", "data_previsao")

    # propaga postos flu
    configuracao_postos_plu <- configuracao[bacia_smap == "posto_flu"]
    ordem <- ordem_afluencia(configuracao_postos_plu$posto, configuracao_postos_plu$posto_jusante)
    for (indice_ordem in 1:max(ordem)) {
        indice_configuracao <- which(ordem == indice_ordem)
        for (indice_usina in indice_configuracao) {
            nome_montante <- configuracao_postos_plu[indice_usina, nome_real]
            nome_jusante <- configuracao[posto == configuracao_postos_plu[indice_usina, posto_jusante], nome_real]
            data_inicio <- max(max(previsao_totalizada[nome == nome_jusante, min(data_previsao)],
                                previsao_totalizada[nome == nome_montante, min(data_previsao)],
                                previsao[, unique(data_caso) - 60]))
            if (configuracao_postos_plu[indice_usina, tv] == 0){
                n <- configuracao_postos_plu[nome_real == nome_montante, n]
                coeficientes <- c(0, 0, 0)
                coeficientes[1] <- configuracao_postos_plu[nome_real == nome_montante, c1]
                coeficientes[2] <- configuracao_postos_plu[nome_real == nome_montante, c2]
                coeficientes[3] <- configuracao_postos_plu[nome_real == nome_montante, c3]
                for (nome_cenario in previsao_totalizada[, unique(cenario)]) {
                    previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_incremental := 
                    propaga_muskingum(previsao_totalizada[nome == nome_montante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_incremental], 
                    previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_incremental], n, coeficientes)]
                }
            } else {
                tv <- configuracao_postos_plu[nome_real == nome_montante, tv]
                for (nome_cenario in previsao_totalizada[, unique(cenario)]) {
                    previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_incremental := 
                    propaga_tv(previsao_totalizada[nome == nome_montante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_incremental], 
                    previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_incremental], tv)]
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
                data_inicio <- max(max(previsao_totalizada[nome == nome_jusante, min(data_previsao)],
                                    previsao_totalizada[nome == nome_jusante, min(data_previsao)],
                                    previsao[, unique(data_caso) - 60]))
                if (configuracao_sem_postos_plu[indice_usina, tv] == 0){
                    n <- configuracao_sem_postos_plu[nome_real == nome_montante, n]
                    coeficientes <- c(0, 0, 0)
                    coeficientes[1] <- configuracao_sem_postos_plu[nome_real == nome_montante, c1]
                    coeficientes[2] <- configuracao_sem_postos_plu[nome_real == nome_montante, c2]
                    coeficientes[3] <- configuracao_sem_postos_plu[nome_real == nome_montante, c3]
                    for (nome_cenario in previsao_totalizada[, unique(cenario)]) {
                        previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total := 
                            propaga_muskingum(previsao_totalizada[nome == nome_montante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total], 
                            previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total], n, coeficientes)]
                        previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total_sem_tv := 
                            previsao_totalizada[nome == nome_montante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total_sem_tv] + 
                            previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total_sem_tv]]
                    }
                } else {
                    tv <- configuracao_sem_postos_plu[nome_real == nome_montante, tv]
                    for (nome_cenario in previsao_totalizada[, unique(cenario)]) {
                        previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total := 
                        propaga_tv(previsao_totalizada[nome == nome_montante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total], 
                        previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total], tv)]
                        previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total_sem_tv := 
                            previsao_totalizada[nome == nome_montante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total_sem_tv] +  
                            previsao_totalizada[nome == nome_jusante & cenario == nome_cenario & data_previsao >= data_inicio, previsao_total_sem_tv]]
                    }
                }
            }
        }
    }
    previsao_totalizada <- previsao_totalizada[nome %in% configuracao[!bacia_smap == "posto_flu", nome_real]]
    previsao_totalizada <- previsao_totalizada[data_previsao >= data_rodada]
    previsao_totalizada
}
