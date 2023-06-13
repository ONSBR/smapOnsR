# FUNCES DE ASSIMILACAO DE DADOS

#' Assimilacao oficial
#' Realiza a assimilacao de dados com a metodologia oficial
#'
#' @param vetor_modelo objeto de classe smap_ons
#' @param area area da sub-bacia
#' @param Supin Escoamento superficial inicial
#' @param Rsup2Inic Escoamento superficial inicial referente ao reservatorio de planicie
#' @param EbInic Escoamento Subterraneo inicial
#' @param TuInic Taxa de umidade inicial do solo
#' @param precipitacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @param evapotranspiracao data table com a evapotranspiracao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @param vazao data table com a vazao a avaliada
#'      \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @param numero_dias numero de dias da assimilacao
#' @param limite_prec limites mínimo e máximo dos pesos utilizados para ponderar a precipitacao durante a etapa de assimilacao
#' @param limite_ebin limites mínimo e máximo da vazao de base inicial
#' @param limite_supin limites mínimo e máximo da vazao superficial inicial
#' @param data_rodada data da rodada
#'
#' @return ajuste lista contendo
#' \itemize{
#'     \item{par}{parametros otimizados send}
#'     \item{value}{valor da funcao objetivo utilizado}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @export
#' 

assimilacao_oficial <- function(vetor_modelo, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias, 
      limite_prec = c(0.5, 2), limite_ebin = c(0.8, 1.2),
      limite_supin = c(0, 2), data_rodada){
    
    kt <- vetor_modelo[12:74]
    kt_max <- sum(kt[1:2] > 0)
    kt_min <- sum(kt[4:63] > 0)
    precipitacao_ponderada <- data.table::data.table(precipitacao)
    precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
    precipitacao_ponderada <- ponderacao_temporal2(precipitacao_ponderada[, valor], kt,
                                                    kt_max, kt_min)

    pesos <- rep(1, numero_dias)
    limite_inferior <- c(rep(limite_prec[1],numero_dias), limite_ebin[1], limite_supin[1])
    limite_superior <- c(rep(limite_prec[2],numero_dias), limite_ebin[2], limite_supin[2])
    limite_inferior[numero_dias] <- 0.9999999999
    limite_superior[numero_dias] <- 1.0000000001
    vetor_parametros <- c(pesos, EbInic, Supin)

    idia <- numero_dias:1
    pesos_funcao_objetivo <- (log(idia + 1) - log(idia)) / log(numero_dias + 1)
    
    ajuste <- stats::optim(par = vetor_parametros, method = "L-BFGS-B",
              lower = limite_inferior, upper = limite_superior,
              fn = funcao_objetivo_assimilacao,
              vetor_modelo = vetor_modelo,
              TuInic = TuInic,
              area = area,
              precipitacao_ponderada = precipitacao_ponderada,
              evapotranspiracao = evapotranspiracao,
              evapotranspiracao_planicie = evapotranspiracao_planicie,
              vazao = vazao,
              numero_dias = numero_dias,
              pesos_funcao_objetivo = pesos_funcao_objetivo,
              control = list(fnscale = 1))
    ajuste
}

#' funcao objetivo de assimilacao do SMAP/ONS

#' @param vetor_modelo vetor resultante de unlist do objeto de classe smap_ons
#' @param vetor_parametros vetor com os parametros:
#' \itemize{
#'     \item{pesos}{pesos da ponderacao da precipitacao}
#'     \item{Ebin}{vazao de base inicial}
#'     \item{Supin}{vazao superficial inicial}
#' }
#' @param pesos_funcao_objetivo vetor de pesos da funcao objetivo
#' @param TuInic umidade do solo inicial
#' @param precipitacao Vetor de precipitacao final (ja corrigido e/ou ponderado)
#' @param evapotranspiracao Vetor de ETo do final(ja corrigido e/ou ponderado)
#' @param vazao data table com a vazao a avaliada
#'      \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' 
#' @param numero_dias numero de dias da assimilacao
#' @param area area da sub-bacia
#' @importFrom data.table data.table
#' @return objetivo valor da funcao objetivo
#' @export

funcao_objetivo_assimilacao <- function(vetor_parametros, vetor_modelo, TuInic,
      precipitacao_ponderada, evapotranspiracao, evapotranspiracao_planicie, vazao, area,
      numero_dias, pesos_funcao_objetivo = rep(1 /length(numero_dias), length(numero_dias))){

  EbInic <- vetor_parametros[(length(vetor_parametros) - 1)]
  Supin <- vetor_parametros[length(vetor_parametros)]
  inicializacao <- smap_ons.inic(vetor_modelo, area, EbInic, TuInic, Supin)
  vetorInicializacao <- unlist(inicializacao)

  precipitacao_ponderada <- precipitacao_ponderada * vetor_parametros[1:(length(vetor_parametros) - 2)]

  simulacao <- rodada_varios_dias_cpp(vetor_modelo,
            vetorInicializacao, area, precipitacao_ponderada,
            evapotranspiracao, evapotranspiracao_planicie, numero_dias)

  objetivo <- calcula_dm(simulacao[, 1], vazao, pesos_funcao_objetivo)
  objetivo
}
