
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

    # --- Otimizacao 1: distribuicao vetorizada via merge ---
    cfg_lookup <- configuracao[, .(posto, nome_real, sub_bacia_agrupada, fator)]
    previsao_totalizada <- merge(
        previsao, cfg_lookup,
        by.x = "nome", by.y = "sub_bacia_agrupada",
        allow.cartesian = TRUE
    )
    previsao_totalizada[, valor := valor * fator]
    previsao_totalizada[, nome := nome_real]
    previsao_totalizada[, c("nome_real", "fator") := NULL]

    # --- Regressao (flag_regressao == 1) ---
    postos_regredidos <- configuracao[regressao > 0 & flag_regressao == 1, nome_real]
    for (nome_regredido in postos_regredidos) {
        posto_reg <- configuracao[nome_real == nome_regredido, posto_regredido]
        nome_base <- configuracao[posto == posto_reg, nome_real]
        regressao <- configuracao[nome_real == nome_regredido, regressao]
        incremental_regredida <- previsao_totalizada[posto == posto_reg, valor]
        previsao_totalizada[nome == nome_regredido, valor := incremental_regredida * regressao]
    }

    data.table::setnames(previsao_totalizada, "valor", "previsao_incremental")
    data.table::setorder(previsao_totalizada, "data_rodada", "nome", "cenario", "data_previsao")

    # --- Otimizacao 2: setkey para lookups binarios ---
    data.table::setkey(previsao_totalizada, nome)

    # --- Otimizacao 3: lookup da configuracao por posto -> nome_real (hash O(1)) ---
    cfg_nome_by_posto <- setNames(configuracao$nome_real, as.character(configuracao$posto))

    # propaga postos flu
    if(nrow(configuracao[bacia_smap == "posto_flu"]) > 0) {
        configuracao_postos_plu <- configuracao[bacia_smap == "posto_flu"]
        ordem <- ordem_afluencia(configuracao_postos_plu$posto, configuracao_postos_plu$posto_jusante)

        # Vetores nomeados para lookups rapidos
        plu_tv <- setNames(configuracao_postos_plu$tv, configuracao_postos_plu$nome_real)
        plu_n <- setNames(configuracao_postos_plu$n, configuracao_postos_plu$nome_real)
        plu_c1 <- setNames(configuracao_postos_plu$c1, configuracao_postos_plu$nome_real)
        plu_c2 <- setNames(configuracao_postos_plu$c2, configuracao_postos_plu$nome_real)
        plu_c3 <- setNames(configuracao_postos_plu$c3, configuracao_postos_plu$nome_real)

        for (indice_ordem in 1:max(ordem)) {
            indice_configuracao <- which(ordem == indice_ordem)
            for (indice_usina in indice_configuracao) {
                nome_montante <- configuracao_postos_plu$nome_real[indice_usina]
                nome_jusante <- cfg_nome_by_posto[as.character(configuracao_postos_plu$posto_jusante[indice_usina])]
                jusante <- previsao_totalizada[.(nome_jusante)]
                montante <- previsao_totalizada[.(nome_montante)]
                if (plu_n[nome_montante] == 0){
                    propagada <- funcaoSmapCpp::propaga_tv_cpp(montante[, previsao_incremental], jusante[, previsao_incremental], plu_tv[nome_montante])
                    previsao_totalizada[.(nome_jusante), previsao_incremental := propagada]
                } else {
                    coeficientes <- c(plu_c1[nome_montante], plu_c2[nome_montante], plu_c3[nome_montante])
                    propagada <- funcaoSmapCpp::propaga_muskingum_cpp(montante[, previsao_incremental], jusante[, previsao_incremental], plu_n[nome_montante], coeficientes)
                    previsao_totalizada[.(nome_jusante), previsao_incremental := propagada]
                }
            }
        }
    }


    # propaga subbacias
    configuracao_sem_postos_plu <- configuracao[!bacia_smap == "posto_flu"]
    previsao_totalizada[, previsao_total := previsao_incremental]
    previsao_totalizada[, previsao_total_sem_tv := previsao_incremental]
    ordem <- ordem_afluencia(configuracao_sem_postos_plu$posto, configuracao_sem_postos_plu$posto_jusante)

    # Vetores nomeados para lookups rapidos
    sem_tv <- setNames(configuracao_sem_postos_plu$tv, configuracao_sem_postos_plu$nome_real)
    sem_n <- setNames(configuracao_sem_postos_plu$n, configuracao_sem_postos_plu$nome_real)
    sem_c1 <- setNames(configuracao_sem_postos_plu$c1, configuracao_sem_postos_plu$nome_real)
    sem_c2 <- setNames(configuracao_sem_postos_plu$c2, configuracao_sem_postos_plu$nome_real)
    sem_c3 <- setNames(configuracao_sem_postos_plu$c3, configuracao_sem_postos_plu$nome_real)
    nomes_reg_flag0 <- configuracao[regressao > 0 & flag_regressao == 0, nome_real]
    reg_posto_regredido <- setNames(
        as.character(configuracao[regressao > 0 & flag_regressao == 0, posto_regredido]),
        configuracao[regressao > 0 & flag_regressao == 0, nome_real]
    )
    reg_regressao <- setNames(
        configuracao[regressao > 0 & flag_regressao == 0, regressao],
        configuracao[regressao > 0 & flag_regressao == 0, nome_real]
    )

    for (indice_ordem in 1:max(ordem)) {
        indice_configuracao <- which(ordem == indice_ordem)
        for (indice_usina in indice_configuracao) {
            nome_montante <- configuracao_sem_postos_plu$nome_real[indice_usina]
            nome_jusante <- cfg_nome_by_posto[as.character(configuracao_sem_postos_plu$posto_jusante[indice_usina])]
            if(nome_montante %in% nomes_reg_flag0) {
                nome_base <- cfg_nome_by_posto[reg_posto_regredido[nome_montante]]
                reg_val <- reg_regressao[nome_montante]

                previsao_totalizada[.(nome_montante), previsao_incremental :=
                            previsao_totalizada[.(nome_base), previsao_total] * reg_val]
                previsao_totalizada[.(nome_montante), previsao_total := previsao_total +
                            previsao_totalizada[.(nome_base), previsao_total] * reg_val]
                previsao_totalizada[.(nome_montante), previsao_total_sem_tv := previsao_total_sem_tv +
                            previsao_totalizada[.(nome_base), previsao_total_sem_tv] * reg_val]
            }
            if (length(nome_jusante) != 0) {
                jusante <- previsao_totalizada[.(nome_jusante)]
                montante <- previsao_totalizada[.(nome_montante)]
                if (sem_n[nome_montante] == 0){
                    propagada <- funcaoSmapCpp::propaga_tv_cpp(montante[, previsao_total], jusante[, previsao_total], sem_tv[nome_montante])
                    previsao_totalizada[.(nome_jusante), previsao_total := propagada]
                    previsao_totalizada[.(nome_jusante), previsao_total_sem_tv :=
                                        montante[, previsao_total_sem_tv] + jusante[, previsao_total_sem_tv]]
                } else {
                    coeficientes <- c(sem_c1[nome_montante], sem_c2[nome_montante], sem_c3[nome_montante])
                    propagada <- funcaoSmapCpp::propaga_muskingum_cpp(montante[, previsao_total], jusante[, previsao_total], sem_n[nome_montante], coeficientes)
                    previsao_totalizada[.(nome_jusante), previsao_total := propagada]
                    previsao_totalizada[.(nome_jusante), previsao_total_sem_tv :=
                                    montante[, previsao_total_sem_tv] + jusante[, previsao_total_sem_tv]]
                }
            }
        }
    }

    previsao_totalizada <- previsao_totalizada[nome %in% configuracao[!bacia_smap == "posto_flu", nome_real]]
    previsao_totalizada <- previsao_totalizada[data_previsao >= data_rodada]
    previsao_totalizada[, previsao_inc_tv := previsao_total]
    for (indice_ordem in 1:max(ordem)) {
        indice_configuracao <- which(ordem == indice_ordem)
        for (indice_usina in indice_configuracao) {
            nome_montante <- configuracao_sem_postos_plu$nome_real[indice_usina]
            nome_jusante <- cfg_nome_by_posto[as.character(configuracao_sem_postos_plu$posto_jusante[indice_usina])]
            if (length(nome_jusante) != 0) {
                jusante <- previsao_totalizada[.(nome_jusante), previsao_inc_tv]
                montante <- previsao_totalizada[.(nome_montante), previsao_total]

                previsao_totalizada[.(nome_jusante),
                            previsao_inc_tv := jusante - montante]
            }
        }
    }

    previsao_totalizada
}