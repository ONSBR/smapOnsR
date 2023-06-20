
#' Funcao de calibracao do modelo SMAP/ONS
#' 
#' Raliza a calibracao de calibracao do SMAP/ONS dado um vetor inicial de pearametros e seus limites superiores e inferiores
#'
#' @param parametros data table com os parametros dos modelos
#'     \itemize{
#'     \item{Nome}{Nome da sub-bacia}
#'     \item{parametro}{nome do parametro}
#'     \item{valor}{valor do parametro}
#'     }
#' @param inicializacao data.table com a inicializacao com as colunas:
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{variavel}{vazao de base inicial}
#'     \item{valor}{valor da variavel}
#'     }
#' @param historico_precipitacao data table com o historico de precipitacao com as colunas:
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @param previsao_precipitacao data.table com a previsao de precipitacao com as colunas:
#'     \itemize{
#'     \item{data_rodada}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da previsao}
#'     }
#' @param historico_etp_NC data.table com o historico de NC deevapotranspiracao com as colunas:
#'     \itemize{
#'     \item{mes}{mes da NC}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da NC de evapotranspiracao observada}
#'     }
#' @param historico_vazao data table com o historico de vazao com as colunas:
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @param postos_plu data table contendo a relacao sub-bacia x postos_plu com as colunas:
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{posto}{nome do posto plu}
#'     \item{valor}{peso do posto plu}
#'     }
#' @param datas_rodadas data table contendo as datas dos casos a serem executados e seus respectivos horizontes:
#'     \itemize{
#'     \item{data}{data do caso}
#'     \item{numero_dias_previsao}{horizonte do caso}
#'     }
#' @param numero_dias_assimilacao numero de dias para o processo de assimilação de dados
#' @param numero_cenarios numero de cenarios a serem gerados
#' @param sub_bacias vetor com o nome das sub-bacias a serem consideradas
#' @importFrom data.table data.table
#' @return saida data table contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_rodada}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{nome}{nome da sub-bacia}
#'     \item{variavel}{nome da variavel}
#'     \item{valor}{valor da variavel}
#'     }
#' @export
#' 
rodada_encadeada_oficial <- function(parametros, inicializacao, historico_precipitacao, 
    previsao_precipitacao, historico_etp_NC, historico_vazao, postos_plu, datas_rodadas, 
    numero_cenarios, sub_bacias) {
    
    numero_sub_bacias <- length(sub_bacias)
    numero_datas <- nrow(datas_rodadas)
    nome_cenario <- unique(previsao_precipitacao[, cenario])

    saida <- data.table::data.table()
    for (ibacia in 1:numero_sub_bacias){

        sub_bacia <- sub_bacias[ibacia]

        EbInic <- inicializacao[nome == sub_bacia & variavel == "Ebin", valor]
        Supin <- inicializacao[nome == sub_bacia & variavel == "Supin", valor]
        TuInic <- inicializacao[nome == sub_bacia & variavel == "Tuin", valor]
        numero_dias_assimilacao <- inicializacao[nome == sub_bacia & variavel == "numero_dias_assimilacao", valor]
        vetor_inicializacao <- array(rep(0, numero_cenarios * 7), c(numero_cenarios, 7))

        modelo <- new_modelo_smap_ons(parametros[Nome == sub_bacia], postos_plu[nome %in% sub_bacia])
        kt <- modelo$kt
        kt_max <- sum(modelo$kt[1:2] > 0)
        kt_min <- sum(modelo$kt[4:63] > 0)
        vetor_modelo <- unlist(modelo)
        area <- attributes(modelo)$area

        for (idata in 1:numero_datas){
            saida_bacia_aux <- data.table::data.table()
            dataRodada <- datas_rodadas[idata, data]
            numero_dias_previsao <- datas_rodadas[data == dataRodada, numero_dias_previsao]
            matriz_precipitacao <- array(rep(0, numero_cenarios * numero_dias_previsao), c(numero_cenarios, numero_dias_previsao))
            matriz_evapotranspiracao <- array(rep(0, numero_cenarios * numero_dias_previsao), c(numero_cenarios, numero_dias_previsao))
            matriz_evapotranspiracao_planicie <- array(rep(0, numero_cenarios * numero_dias_previsao), c(numero_cenarios, numero_dias_previsao))

            vazao <- historico_vazao[data < dataRodada & data >= (dataRodada - numero_dias_assimilacao) 
                          & posto == sub_bacia, valor]
                          
            normal_climatologica <- historico_etp_NC[nome == sub_bacia]

            precipitacao_observada <- historico_precipitacao[nome == sub_bacia &
            data <= dataRodada & data >= (dataRodada - numero_dias_assimilacao - kt_min)]
            previsao_rodada <- previsao_precipitacao[nome == sub_bacia & data_rodada == dataRodada]

            precipitacao <- data.table::data.table(precipitacao_observada)
            precipitacao[, data_rodada := dataRodada]
            precipitacao <- combina_observacao_previsao(precipitacao, previsao_rodada)

            precipitacao_assimilacao <- data.table::data.table(precipitacao[data_previsao < (dataRodada + kt_max) & 
                data_previsao >= (dataRodada - numero_dias_assimilacao - kt_min) & cenario == unique(cenario)[1]])
            colnames(precipitacao_assimilacao)[1] <- "data"
            precipitacao_assimilacao[, cenario := NULL]
            precipitacao_assimilacao[, data_rodada := NULL]

            evapotranspiracao <- transforma_NC_serie(precipitacao_assimilacao[data < dataRodada & data >= (dataRodada - numero_dias_assimilacao)], normal_climatologica) 
            evapotranspiracao_planicie <- evapotranspiracao[, valor] * vetor_modelo[77]
            evapotranspiracao <- evapotranspiracao[, valor] * vetor_modelo[76]

            ajuste <- assimilacao_oficial(vetor_modelo, area, EbInic, TuInic, Supin, precipitacao_assimilacao,
                        evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias = numero_dias_assimilacao)

            if (idata < numero_datas) {
                inicio_proxima_assimilacao <- datas_rodadas[idata + 1, data] - numero_dias_assimilacao - 1
                ajuste$simulacao[, data := seq.Date((dataRodada - numero_dias_assimilacao), dataRodada - 1, 1)]
                EbInic <- ajuste$simulacao[data == inicio_proxima_assimilacao, Eb] * area / 86.4
                Supin <- (ajuste$simulacao[data == inicio_proxima_assimilacao, Ed] +
                          ajuste$simulacao[data == inicio_proxima_assimilacao, Ed2] +
                          ajuste$simulacao[data == inicio_proxima_assimilacao, Ed3]) * area / 86.4
                TuInic <- ajuste$simulacao[data == inicio_proxima_assimilacao, Tu]
            }

            precipitacao[, valor := valor * vetor_modelo[75]]
            for (icenario in 1:length(unique(precipitacao[, cenario]))){
                matriz_precipitacao[icenario,] <- ponderacao_temporal(precipitacao[data_previsao < (dataRodada + numero_dias_previsao+ kt_max) & 
                data_previsao >= (dataRodada - kt_min) & cenario == nome_cenario[icenario], valor], kt, kt_max, kt_min)
            }
            
            colnames(precipitacao)[1] <- "data"
            precipitacao <- precipitacao[cenario == unique(cenario)[1]]
            precipitacao[, data_rodada := NULL]
            precipitacao[, cenario := NULL]
            evapotranspiracao <- transforma_NC_serie(precipitacao[data <= dataRodada + numero_dias_previsao - 1 & data >= dataRodada], normal_climatologica)
            for (icenario in 1:numero_cenarios){
                matriz_evapotranspiracao_planicie[icenario,] <- evapotranspiracao[, valor] * vetor_modelo[77]
                matriz_evapotranspiracao[icenario,] <- evapotranspiracao[, valor] * vetor_modelo[76]
            }

            vetor_inicializacao[, 4] <- ajuste$simulacao[numero_dias_assimilacao, Rsup2]
            vetor_inicializacao[, 5] <- ajuste$simulacao[numero_dias_assimilacao, Rsolo]
            vetor_inicializacao[, 6] <- ajuste$simulacao[numero_dias_assimilacao, Rsup]
            vetor_inicializacao[, 7] <- ajuste$simulacao[numero_dias_assimilacao, Rsub]
            
            simulacao <- rodada_cenarios_dias_cpp(vetor_modelo,
            vetor_inicializacao, area, matriz_precipitacao,
            matriz_evapotranspiracao, matriz_evapotranspiracao_planicie, numero_dias_previsao, numero_cenarios)
            
            for (icenario in 1: numero_cenarios){
                saida_bacia_aux <- rbind(saida_bacia_aux, simulacao[[icenario]])
            }

            saida_bacia_aux[, nome := sub_bacia]
            saida_bacia_aux[, data_caso := dataRodada]
            saida_bacia_aux[, cenario := rep(nome_cenario, each = numero_dias_previsao)]
            saida_bacia_aux[, data_previsao := rep(seq.Date(dataRodada, dataRodada + numero_dias_previsao - 1, by = 1), numero_cenarios)]
            saida <- rbind(saida, saida_bacia_aux)
        }
    }
    saida <- melt(saida, id.vars = c("data_caso", "data_previsao", "cenario", "nome"), variable.name = "variavel",
           value.name = "valor")
    saida
}
