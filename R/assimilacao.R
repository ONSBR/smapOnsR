# FUNCES DE ASSIMILACAO DE DADOS

#' Assimilacao oficial
#' 
#' Realiza a assimilacao de dados com a metodologia oficial
#'
#' @param vetor_modelo objeto de classe smap_ons
#' @param area area da sub-bacia
#' @param Supin Escoamento superficial inicial
#' @param EbInic Escoamento Subterraneo inicial
#' @param TuInic Taxa de umidade inicial do solo
#' @param precipitacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @param evapotranspiracao vetor de evapotranspiracao
#' @param evapotranspiracao_planicie vetor de evapotranspiracao do reservatorio de planicie
#' @param vazao vetor de vazao observada
#' @param numero_dias numero de dias da assimilacao
#' @param limite_prec limites mínimo e máximo dos pesos utilizados para ponderar a precipitacao durante a etapa de assimilacao
#' @param limite_ebin limites mínimo e máximo da vazao de base inicial
#' @param limite_supin limites mínimo e máximo da vazao superficial inicial
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
      limite_supin = c(0, 2)){
    
    kt <- vetor_modelo[12:74]
    kt_max <- sum(kt[1:2] > 0)
    kt_min <- sum(kt[4:63] > 0)
    precipitacao_ponderada <- data.table::data.table(precipitacao)
    precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
    precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt,
                                                    kt_max, kt_min)

    pesos <- rep(1, numero_dias)
    limite_inferior <- c(rep(limite_prec[1],numero_dias), limite_ebin[1] * EbInic, limite_supin[1] * Supin)
    limite_superior <- c(rep(limite_prec[2],numero_dias), limite_ebin[2] * EbInic, limite_supin[2] * Supin)
    limite_inferior[numero_dias] <- 0.9999999999
    limite_superior[numero_dias] <- 1.0000000001
    vetor_variaveis <- c(pesos, EbInic, Supin)

    idia <- numero_dias:1
    pesos_funcao_objetivo <- (log(idia + 1) - log(idia)) / log(numero_dias + 1)
    
    ajuste <- stats::optim(par = vetor_variaveis, method = "L-BFGS-B",
              lower = limite_inferior, upper = limite_superior,
              fn = funcao_objetivo_assimilacao_oficial,
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

  EbInic <- ajuste$par[numero_dias + 1]
  Supin <- ajuste$par[numero_dias + 2]
  inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
  vetor_inicializacao <- unlist(inicializacao)

  precipitacao_ponderada <- precipitacao_ponderada * ajuste$par[1:numero_dias]

  simulacao <- rodada_varios_dias_cpp(vetor_modelo,
            vetor_inicializacao, area, precipitacao_ponderada,
            evapotranspiracao, evapotranspiracao_planicie, numero_dias)

  simulacao <- data.table::data.table(simulacao)

  saida <- list(ajuste = ajuste, simulacao = simulacao)
  saida
}

#' Funcao objetivo da assimilacao oficial
#' 
#' Funcao objetivo de assimilacao de dados oficial do SMAP/ONS
#' @param vetor_modelo vetor resultante de unlist do objeto de classe smap_ons
#' @param vetor_variaveis vetor com os parametros:
#' \itemize{
#'     \item{pesos}{pesos da ponderacao da precipitacao}
#'     \item{Ebin}{vazao de base inicial}
#'     \item{Supin}{vazao superficial inicial}
#' }
#' @param pesos_funcao_objetivo vetor de pesos da funcao objetivo
#' @param TuInic umidade do solo inicial
#' @param precipitacao_ponderada Vetor de precipitacao final (ja corrigido e/ou ponderado)
#' @param evapotranspiracao Vetor de ETo do final(ja corrigido e/ou ponderado)
#' @param evapotranspiracao_planicie Vetor de ETo de planicie do final(ja corrigido e/ou ponderado)
#' @param vazao vetor de vazao observada
#' @param numero_dias numero de dias da assimilacao
#' @param area area da sub-bacia
#' @importFrom data.table data.table
#' @return objetivo valor da funcao objetivo
#' @export

funcao_objetivo_assimilacao_oficial <- function(vetor_variaveis, vetor_modelo, TuInic,
      precipitacao_ponderada, evapotranspiracao, evapotranspiracao_planicie, vazao, area,
      numero_dias, pesos_funcao_objetivo = rep((1 / numero_dias), numero_dias)){

  EbInic <- vetor_variaveis[numero_dias + 1]
  Supin <- vetor_variaveis[numero_dias + 2]
  inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
  vetor_inicializacao <- unlist(inicializacao)

  precipitacao_ponderada <- precipitacao_ponderada * vetor_variaveis[1:numero_dias]

  simulacao <- rodada_varios_dias_cpp(vetor_modelo,
            vetor_inicializacao, area, precipitacao_ponderada,
            evapotranspiracao, evapotranspiracao_planicie, numero_dias)

  objetivo <- calcula_dm(simulacao[, 1], vazao, pesos_funcao_objetivo)
  objetivo
}

# FUNCOES DE ASSIMILACAO DE DADOS COM EVAPOTRANSPIRACAO

#' Assimilacao com evapotranspiracao
#'
#' Realiza a assimilacao de dados considerando serie temporal de evapotranspiracao
#'
#' @param vetor_modelo objeto de classe smap_ons
#' @param area area da sub-bacia
#' @param Supin Escoamento superficial inicial
#' @param EbInic Escoamento Subterraneo inicial
#' @param TuInic Taxa de umidade inicial do solo
#' @param precipitacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @param evapotranspiracao vetor de evapotranspiracao
#' @param evapotranspiracao_planicie vetor de evapotranspiracao do reservatorio de planicie
#' @param vazao vetor de vazao observada
#' @param numero_dias numero de dias da assimilacao
#' @param limite_prec limites mínimo e máximo dos pesos utilizados para ponderar a precipitacao durante a etapa de assimilacao
#' @param limite_etp limites mínimo e máximo dos pesos utilizados para ponderar a precipitacao durante a etapa de assimilacao
#' @param limite_ebin limites mínimo e máximo da vazao de base inicial
#' @param limite_supin limites mínimo e máximo da vazao superficial inicial
#'
#' @return ajuste lista contendo
#' \itemize{
#'     \item{par}{parametros otimizados}
#'     \item{value}{valor da funcao objetivo utilizado}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' @export
#' 

assimilacao_evapotranspiracao <- function(vetor_modelo, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias,
      limite_prec = c(0.5, 2), limite_etp = c(0.5, 2), limite_ebin = c(0.8, 1.2),
      limite_supin = c(0, 2)){
    
    kt <- vetor_modelo[12:74]
    kt_max <- sum(kt[1:2] > 0)
    kt_min <- sum(kt[4:63] > 0)
    precipitacao_ponderada <- data.table::data.table(precipitacao)
    precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
    precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt,
                                                    kt_max, kt_min)

    pesos_prec <- rep(1, numero_dias)
    pesos_etp <- rep(1, numero_dias)
    limite_inferior <- c(rep(limite_prec[1],numero_dias), rep(limite_etp[1],numero_dias), limite_ebin[1] * EbInic, limite_supin[1] * Supin)
    limite_superior <- c(rep(limite_prec[2],numero_dias), rep(limite_etp[2],numero_dias), limite_ebin[2] * EbInic, limite_supin[2] * Supin)
    limite_inferior[numero_dias] <- 0.9999999999
    limite_superior[numero_dias] <- 1.0000000001
    vetor_variaveis <- c(pesos_prec, pesos_etp, EbInic, Supin)

    idia <- numero_dias:1
    pesos_funcao_objetivo <- (log(idia + 1) - log(idia)) / log(numero_dias + 1)

    ajuste <- stats::optim(par = vetor_variaveis, method = "L-BFGS-B",
              lower = limite_inferior, upper = limite_superior,
              fn = funcao_objetivo_assimilacao_evapotranspiracao,
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
    
    EbInic <- ajuste$par[numero_dias * 2 + 1]
    Supin <- ajuste$par[numero_dias * 2 + 2]
    inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
    vetor_inicializacao <- unlist(inicializacao)

    precipitacao_ponderada <- precipitacao_ponderada * ajuste$par[1:numero_dias]
    evapotranspiracao_ponderada <- evapotranspiracao * ajuste$par[(1:numero_dias) * 2]
    evapotranspiracao_evapotranspiracao_planicie_ponderada <- evapotranspiracao_planicie * ajuste$par[(1:numero_dias) * 2]

    simulacao <- rodada_varios_dias_cpp(vetor_modelo,
              vetor_inicializacao, area, precipitacao_ponderada,
              evapotranspiracao_ponderada, evapotranspiracao_evapotranspiracao_planicie_ponderada, numero_dias)

    simulacao <- data.table::data.table(simulacao)

    saida <- list(ajuste = ajuste, simulacao = simulacao)
    saida
}

#' Funcao objetivo da assimilacao com evaptranspiracao
#' 
#' Funcao objetivo de assimilacao de considerando serie temporal de evapotranspiracao
#' @param vetor_modelo vetor resultante de unlist do objeto de classe smap_ons
#' @param vetor_variaveis vetor com os parametros:
#' \itemize{
#'     \item{pesos}{pesos da ponderacao da precipitacao}
#'     \item{Ebin}{vazao de base inicial}
#'     \item{Supin}{vazao superficial inicial}
#' }
#' @param pesos_funcao_objetivo vetor de pesos da funcao objetivo
#' @param TuInic umidade do solo inicial
#' @param precipitacao_ponderada Vetor de precipitacao final (ja corrigido e/ou ponderado)
#' @param evapotranspiracao Vetor de ETo do final(ja corrigido e/ou ponderado)
#' @param evapotranspiracao_planicie Vetor de ETo de planicie do final(ja corrigido e/ou ponderado)
#' @param vazao vetor de vazao observada
#' @param numero_dias numero de dias da assimilacao
#' @param area area da sub-bacia
#' @importFrom data.table data.table
#' @return objetivo valor da funcao objetivo
#' @export

funcao_objetivo_assimilacao_evapotranspiracao <- function(vetor_variaveis, vetor_modelo, TuInic,
      precipitacao_ponderada, evapotranspiracao, evapotranspiracao_planicie, vazao, area,
      numero_dias, pesos_funcao_objetivo = rep((1 / numero_dias), numero_dias)){

  EbInic <- vetor_variaveis[numero_dias + 1]
  Supin <- vetor_variaveis[numero_dias + 2]
  inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
  vetor_inicializacao <- unlist(inicializacao)

  precipitacao_ponderada <- precipitacao_ponderada * vetor_variaveis[1:numero_dias]

  evapotranspiracao <- evapotranspiracao * vetor_variaveis[2 * (1:numero_dias)]
  evapotranspiracao_planicie <- evapotranspiracao_planicie * vetor_variaveis[2 * (1:numero_dias)]

  simulacao <- rodada_varios_dias_cpp(vetor_modelo,
            vetor_inicializacao, area, precipitacao_ponderada,
            evapotranspiracao, evapotranspiracao_planicie, numero_dias)

  objetivo <- calcula_dm(simulacao[, 1], vazao, pesos_funcao_objetivo)
  objetivo
}
