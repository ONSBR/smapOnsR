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
#' @param precipitacao_assimilacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{nome - nome da sub-bacia}
#'     \item{valor - valor da variavel}
#'     }
#' @param evapotranspiracao vetor de evapotranspiracao potencial
#' @param evapotranspiracao_planicie vetor de evapotranspiracao potencial do reservatorio de planicie
#' @param vazao vetor de vazao observada
#' @param numero_dias_assimilacao numero de dias da assimilacao
#' @param limite_prec limites mínimo e máximo dos pesos utilizados para ponderar a precipitacao durante a etapa de assimilacao
#' @param limite_ebin limites mínimo e máximo da vazao de base inicial
#' @param limite_supin limites mínimo e máximo da vazao superficial inicial
#' @param funcao_objetivo funcao objetivo a ser utilizada na assimilacao
#' @param fnscale valor indicando se a funcao deve ser maximizada ou minimizada
#'
#' @examples
#' 
#' # usando dado dummy contido no pacote
#'   sub_bacia <- "baixoig"
#'  data_rodada <- as.Date('2020/01/01')
#'  dias_assimilacao <- 31
#'  numero_dias <- dias_assimilacao
#'  EbInic <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
#'  Supin <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
#'  TuInic <- 0.5
#'
#'  modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome == sub_bacia])
#'  vetor_modelo <- unlist(modelo)
#'  area <- attributes(modelo)$area
#'
#'  vazao <- historico_vazao[data < data_rodada & data >= (data_rodada - dias_assimilacao) 
#'                          & posto == sub_bacia, valor]
#'  normal_climatologica <- historico_etp_NC[nome == sub_bacia]
#'
#'  kt_max <- sum(modelo$kt[1:2] > 0)
#'  kt_min <- sum(modelo$kt[4:63] > 0)
#'  precipitacao <- historico_precipitacao[data < (data_rodada + kt_max) & data >= (data_rodada - dias_assimilacao - kt_min) & posto == 'psatbigu']
#'  precipitacao <- ponderacao_espacial(precipitacao, postos_plu[nome == sub_bacia])
#'
#'  evapotranspiracao <- transforma_NC_serie(precipitacao[data < data_rodada & data >= (data_rodada - dias_assimilacao)], normal_climatologica) 
#'  evapotranspiracao_planicie <- evapotranspiracao[, valor] * vetor_modelo[77]
#'  evapotranspiracao <- evapotranspiracao[, valor] * vetor_modelo[76]
#'
#'  kt <- vetor_modelo[12:74]
#'  precipitacao_ponderada <- data.table::data.table(precipitacao)
#'  precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
#'  precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt, kt_max, kt_min)
#'
#'  pesos <- rep(1, numero_dias)
#'  limite_prec <- c(0.5, 2)
#'  limite_ebin <- c(0.8, 1.2)
#'  limite_supin <- c(0, 2)
#'
#'  vetor_parametros <- c(pesos, EbInic, Supin)
#' \dontrun{
#'  saida <- assimilacao_oficial(vetor_modelo, area, EbInic, TuInic, Supin, precipitacao,
#'      evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias = dias_assimilacao)
#' }
#' @importFrom funcaoSmapCpp rodada_varios_dias_cpp2
#' @return lista contendo
#' \itemize{
#'     \item{par - parametros otimizados send}
#'     \item{value - valor da funcao objetivo utilizado}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @export

assimilacao_oficial <- function(vetor_modelo, area, EbInic, TuInic, Supin, precipitacao_assimilacao,
      evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias_assimilacao,
      limite_prec = c(0.5, 2), limite_ebin = c(0.8, 1.2),
      limite_supin = c(0, 2), funcao_objetivo = calcula_dm, fnscale = 1) {
    
    kt <- vetor_modelo[12:74]
    kt_max <- sum(vetor_modelo[12:13] > 0)
    kt_min <- sum(vetor_modelo[15:74] > 0)
    precipitacao_ponderada <- data.table::data.table(precipitacao_assimilacao)
    precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
    precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt,
                                                    kt_max, kt_min)
    vazao_observada_maxima <- max(vazao)
    pesos <- rep(1, numero_dias_assimilacao)
    limite_inferior <- c(rep(limite_prec[1],numero_dias_assimilacao), limite_ebin[1] * EbInic, limite_supin[1] * vazao_observada_maxima)
    limite_superior <- c(rep(limite_prec[2],numero_dias_assimilacao), limite_ebin[2] * EbInic, limite_supin[2] * vazao_observada_maxima)
    limite_inferior[numero_dias_assimilacao] <- 0.9999999999
    limite_superior[numero_dias_assimilacao] <- 1.0000000001
    limites_iguais <- limite_superior == limite_inferior
    limite_superior[limites_iguais] <- limite_inferior[limites_iguais] + 0.00000001
    vetor_variaveis <- c(pesos, EbInic, Supin)

    idia <- numero_dias_assimilacao:1
    pesos_funcao_objetivo <- (log(idia + 1) - log(idia)) / log(numero_dias_assimilacao + 1)
    
    set.seed(12364810)
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
              numero_dias_assimilacao = numero_dias_assimilacao,
              pesos_funcao_objetivo = pesos_funcao_objetivo,
              funcao_objetivo = funcao_objetivo,
              control = list(fnscale = fnscale, ndeps = rep(0.000001, length(vetor_variaveis)),
              maxit = 1000))

  EbInic <- ajuste$par[numero_dias_assimilacao + 1]
  Supin <- ajuste$par[numero_dias_assimilacao + 2]
  if (Supin < 0) { #L-BFGS-B as vezes fornece valor negativo próximo a 0 ('-1e-17')
    Supin <- 0
  }
  inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
  vetor_inicializacao <- unlist(inicializacao)

  precipitacao_ponderada <- precipitacao_ponderada * ajuste$par[1:numero_dias_assimilacao]

  simulacao <- funcaoSmapCpp::rodada_varios_dias_cpp2(vetor_modelo,
            vetor_inicializacao, area, precipitacao_ponderada,
            evapotranspiracao, evapotranspiracao_planicie, numero_dias_assimilacao)

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
#'     \item{pesos - pesos da ponderacao da precipitacao}
#'     \item{Ebin - vazao de base inicial}
#'     \item{Supin - vazao superficial inicial}
#' }
#' @param pesos_funcao_objetivo vetor de pesos da funcao objetivo
#' @param TuInic percentual de umidade do solo inicial
#' @param precipitacao_ponderada Vetor de precipitacao final (ja corrigido e/ou ponderado)
#' @param evapotranspiracao Vetor de ETo do final(ja corrigido e/ou ponderado)
#' @param evapotranspiracao_planicie Vetor de ETo de planicie do final(ja corrigido e/ou ponderado)
#' @param vazao vetor de vazao observada
#' @param numero_dias_assimilacao numero de dias da assimilacao
#' @param area area da sub-bacia
#' @param funcao_objetivo funcao objetivo a ser utilizada na assimilacao
#' 
#' @examples
#' 
#' # usando dado dummy contido no pacote
#' sub_bacia <- "baixoig"
#' data_rodada <- as.Date('2020/01/01')
#' dias_assimilacao <- 31
#' numero_dias <- dias_assimilacao
#' EbInic <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
#' Supin <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
#' TuInic <- 0.5
#'
#' modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome == sub_bacia])
#' vetor_modelo <- unlist(modelo)
#' area <- attributes(modelo)$area
#'
#' vazao <- historico_vazao[data < data_rodada & data >= (data_rodada - dias_assimilacao) 
#'                         & posto == sub_bacia, valor]
#' normal_climatologica <- historico_etp_NC[nome == sub_bacia]
#'
#' kt_max <- sum(modelo$kt[1:2] > 0)
#' kt_min <- sum(modelo$kt[4:63] > 0)
#' precipitacao <- historico_precipitacao[data < (data_rodada + kt_max) & data >= (data_rodada - dias_assimilacao - kt_min) & posto == 'psatbigu']
#' precipitacao <- ponderacao_espacial(precipitacao, postos_plu[nome == sub_bacia])
#'
#' evapotranspiracao <- transforma_NC_serie(precipitacao[data < data_rodada & data >= (data_rodada - dias_assimilacao)], normal_climatologica) 
#' evapotranspiracao_planicie <- evapotranspiracao[, valor] * vetor_modelo[77]
#' evapotranspiracao <- evapotranspiracao[, valor] * vetor_modelo[76]
#'
#' kt <- vetor_modelo[12:74]
#' precipitacao_ponderada <- data.table::data.table(precipitacao)
#' precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
#' precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt, kt_max, kt_min)
#' 
#' pesos <- rep(1, numero_dias)
#' vetor_parametros <- c(pesos, EbInic, Supin)
#' 
#' fo <- funcao_objetivo_assimilacao_oficial(vetor_parametros, vetor_modelo, TuInic, 
#'      precipitacao_ponderada, evapotranspiracao, evapotranspiracao_planicie, vazao, area,
#'      numero_dias)
#' fo
#' 
#' @importFrom data.table data.table
#' @return objetivo valor da funcao objetivo
#' @export

funcao_objetivo_assimilacao_oficial <- function(vetor_variaveis, vetor_modelo, TuInic,
      precipitacao_ponderada, evapotranspiracao, evapotranspiracao_planicie, vazao, area,
      numero_dias_assimilacao,
      pesos_funcao_objetivo = rep((1 / numero_dias_assimilacao), numero_dias_assimilacao),
      funcao_objetivo = calcula_dm) {

  EbInic <- vetor_variaveis[numero_dias_assimilacao + 1]
  Supin <- vetor_variaveis[numero_dias_assimilacao + 2]
  if (Supin < 0) { #L-BFGS-B as vezes fornece valor negativo proximo a 0 ('-1e-17')
    Supin <- 0
  }
  inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
  vetor_inicializacao <- unlist(inicializacao)

  precipitacao_ponderada <- precipitacao_ponderada * vetor_variaveis[1:numero_dias_assimilacao]

  simulacao <- funcaoSmapCpp::rodada_varios_dias_cpp2(vetor_modelo,
            vetor_inicializacao, area, precipitacao_ponderada,
            evapotranspiracao, evapotranspiracao_planicie, numero_dias_assimilacao)

  objetivo <- funcao_objetivo(simulacao[, 1], vazao, pesos_funcao_objetivo)
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
#' @param precipitacao_assimilacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{nome - nome da sub-bacia}
#'     \item{valor - valor da variavel}
#'     }
#' @param evapotranspiracao vetor de evapotranspiracao potencial
#' @param evapotranspiracao_planicie vetor de evapotranspiracao potencial do reservatorio de planicie
#' @param vazao vetor de vazao observada
#' @param numero_dias_assimilacao numero de dias da assimilacao
#' @param limite_prec limites mínimo e máximo dos pesos utilizados para ponderar a precipitacao durante a etapa de assimilacao
#' @param limite_etp limites mínimo e máximo dos pesos utilizados para ponderar a precipitacao durante a etapa de assimilacao
#' @param limite_ebin limites mínimo e máximo da vazao de base inicial
#' @param limite_supin limites mínimo e máximo da vazao superficial inicial
#' @param funcao_objetivo funcao objetivo a ser utilizada na assimilacao
#' @param fnscale valor indicando se a funcao deve ser maximizada ou minimizada
#'
#' @examples
#' # usando dado dummy contido no pacote
#'  sub_bacia <- "baixoig"
#'  data_rodada <- as.Date('2020/01/01')
#'  dias_assimilacao <- 31
#'  numero_dias <- dias_assimilacao
#'  EbInic <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
#'  Supin <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
#'  TuInic <- 0.5
#'
#'  modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome == sub_bacia])
#'  vetor_modelo <- unlist(modelo)
#'  area <- attributes(modelo)$area
#'
#'  vazao <- historico_vazao[data < data_rodada & data >= (data_rodada - dias_assimilacao) 
#'                          & posto == sub_bacia, valor]
#'  normal_climatologica <- historico_etp_NC[nome == sub_bacia]
#'
#'  kt_max <- sum(modelo$kt[1:2] > 0)
#'  kt_min <- sum(modelo$kt[4:63] > 0)
#'  precipitacao <- historico_precipitacao[data < (data_rodada + kt_max) & data >= (data_rodada - dias_assimilacao - kt_min) & posto == 'psatbigu']
#'  evapotranspiracao <- historico_etp[data < data_rodada & data >= (data_rodada - dias_assimilacao) & posto == sub_bacia]
#'  evapotranspiracao_planicie <- evapotranspiracao[, valor] * vetor_modelo[77]
#'  evapotranspiracao <- evapotranspiracao[, valor] * vetor_modelo[76]
#'
#'  kt <- vetor_modelo[12:74]
#'  precipitacao_ponderada <- data.table::data.table(precipitacao)
#'  precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
#'  precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt, kt_max, kt_min)
#'
#'  pesos_prec <- rep(1, numero_dias)
#'  pesos_etp <- rep(1, numero_dias)
#'  limite_prec <- c(0.5, 2)
#'  limite_etp <- c(0.5, 2)
#'  limite_ebin <- c(0.8, 1.2)
#'  limite_supin <- c(0, 2)
#'
#'  vetor_parametros <- c(pesos_prec, pesos_etp, EbInic, Supin)
#' \dontrun{
#'  saida <- assimilacao_evapotranspiracao(vetor_modelo, area, EbInic, TuInic, Supin, precipitacao,
#'      evapotranspiracao, evapotranspiracao_planicie, vazao, numero_dias = dias_assimilacao)
#' }
#' @return ajuste lista contendo
#' \itemize{
#'     \item{par - parametros otimizados}
#'     \item{value - valor da funcao objetivo utilizado}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @export

assimilacao_evapotranspiracao <- function(vetor_modelo, area, EbInic, TuInic, Supin, 
      precipitacao_assimilacao, evapotranspiracao, evapotranspiracao_planicie,
      vazao, numero_dias_assimilacao,
      limite_prec = c(0.5, 2), limite_etp = c(0.5, 2), limite_ebin = c(0.8, 1.2),
      limite_supin = c(0, 2), funcao_objetivo = calcula_dm, fnscale = 1) {
    
    kt <- vetor_modelo[12:74]
    kt_max <- sum(vetor_modelo[12:13] > 0)
    kt_min <- sum(vetor_modelo[15:74] > 0)
    precipitacao_ponderada <- data.table::data.table(precipitacao_assimilacao)
    precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
    precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt,
                                                    kt_max, kt_min)

    vazao_observada_maxima <- max(vazao)
    pesos_prec <- rep(1, numero_dias_assimilacao)
    pesos_etp <- rep(1, numero_dias_assimilacao)
    limite_inferior <- c(rep(limite_prec[1], numero_dias_assimilacao), rep(limite_etp[1], numero_dias_assimilacao), limite_ebin[1] * EbInic, limite_supin[1] * vazao_observada_maxima)
    limite_superior <- c(rep(limite_prec[2], numero_dias_assimilacao), rep(limite_etp[2], numero_dias_assimilacao), limite_ebin[2] * EbInic, limite_supin[2] * vazao_observada_maxima)
    limite_inferior[numero_dias_assimilacao] <- 0.9999999999
    limite_superior[numero_dias_assimilacao] <- 1.0000000001
    limites_iguais <- limite_superior == limite_inferior
    print(paste0("limite superior: ", limite_superior))
    print(paste0("limite inferior: ", limite_inferior))
    print(paste0("limites iguais: ", limites_iguais))
    limite_superior[limites_iguais] <- limite_inferior[limites_iguais] + 0.000001
    vetor_variaveis <- c(pesos_prec, pesos_etp, EbInic, Supin)

    idia <- numero_dias_assimilacao:1
    pesos_funcao_objetivo <- (log(idia + 1) - log(idia)) / log(numero_dias_assimilacao + 1)

    set.seed(12364810)
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
              numero_dias_assimilacao = numero_dias_assimilacao,
              pesos_funcao_objetivo = pesos_funcao_objetivo,
              funcao_objetivo = funcao_objetivo,
              control = list(fnscale = fnscale, ndeps = rep(0.000001, length(vetor_variaveis)),
              maxit = 1000))
    
    EbInic <- ajuste$par[numero_dias_assimilacao * 2 + 1]
    Supin <- ajuste$par[numero_dias_assimilacao * 2 + 2]
    if (Supin < 0) { #L-BFGS-B as vezes fornece valor negativo próximo a 0 ('-1e-17')
      Supin <- 0
    }
    inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
    vetor_inicializacao <- unlist(inicializacao)

    precipitacao_ponderada <- precipitacao_ponderada * ajuste$par[1:numero_dias_assimilacao]
    evapotranspiracao_ponderada <- evapotranspiracao * ajuste$par[(1:numero_dias_assimilacao) * 2]
    evapotranspiracao_planicie <- evapotranspiracao_planicie * ajuste$par[(1:numero_dias_assimilacao) * 2]

    simulacao <- funcaoSmapCpp::rodada_varios_dias_cpp2(vetor_modelo,
              vetor_inicializacao, area, precipitacao_ponderada,
              evapotranspiracao_ponderada, evapotranspiracao_planicie, numero_dias_assimilacao)

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
#'     \item{pesos - pesos da ponderacao da precipitacao}
#'     \item{Ebin - vazao de base inicial}
#'     \item{Supin - vazao superficial inicial}
#' }
#' @param pesos_funcao_objetivo vetor de pesos da funcao objetivo
#' @param TuInic umidade do solo inicial
#' @param precipitacao_ponderada Vetor de precipitacao final (ja corrigido e/ou ponderado)
#' @param evapotranspiracao Vetor de ETo do final(ja corrigido e/ou ponderado)
#' @param evapotranspiracao_planicie Vetor de ETo de planicie do final(ja corrigido e/ou ponderado)
#' @param vazao vetor de vazao observada
#' @param numero_dias_assimilacao numero de dias da assimilacao
#' @param area area da sub-bacia
#' @param funcao_objetivo funcao objetivo a ser utilizada na assimilacao
#' 
#' @examples
#' # usando dado dummy contido no pacote
#' sub_bacia <- "baixoig"
#' data_rodada <- as.Date('2020/01/01')
#' dias_assimilacao <- 31
#' numero_dias <- dias_assimilacao
#' EbInic <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
#' Supin <- historico_vazao[data == (data_rodada - dias_assimilacao + 1) & posto == sub_bacia, valor] / 2
#' TuInic <- 0.5
#'
#' modelo <- new_modelo_smap_ons(parametros[nome == sub_bacia], postos_plu[nome == sub_bacia])
#' vetor_modelo <- unlist(modelo)
#' area <- attributes(modelo)$area
#'
#' vazao <- historico_vazao[data < data_rodada & data >= (data_rodada - dias_assimilacao) 
#'                         & posto == sub_bacia, valor]
#' normal_climatologica <- historico_etp_NC[nome == sub_bacia]
#'
#' kt_max <- sum(modelo$kt[1:2] > 0)
#' kt_min <- sum(modelo$kt[4:63] > 0)
#' precipitacao <- historico_precipitacao[data < (data_rodada + kt_max) & data >= (data_rodada - dias_assimilacao - kt_min) & posto == 'psatbigu']
#' precipitacao <- ponderacao_espacial(precipitacao, postos_plu[nome == sub_bacia])
#'
#' evapotranspiracao <- historico_etp[data < data_rodada & data >= (data_rodada - dias_assimilacao) & posto == sub_bacia]
#' evapotranspiracao_planicie <- evapotranspiracao[, valor] * vetor_modelo[77]
#' evapotranspiracao <- evapotranspiracao[, valor] * vetor_modelo[76]
#'
#' kt <- vetor_modelo[12:74]
#' precipitacao_ponderada <- data.table::data.table(precipitacao)
#' precipitacao_ponderada[, valor := valor * vetor_modelo[75]]
#' precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt, kt_max, kt_min)
#' 
#' pesos_prec <- rep(1, numero_dias)
#' pesos_etp <- rep(1, numero_dias)
#' vetor_parametros <- c(pesos_prec, pesos_etp, EbInic, Supin)
#' 
#' fo <- funcao_objetivo_assimilacao_evapotranspiracao(vetor_parametros, vetor_modelo, TuInic, 
#'      precipitacao_ponderada, evapotranspiracao, evapotranspiracao_planicie, vazao, area,
#'      numero_dias)
#' fo
#' 
#' @importFrom data.table data.table
#' @return objetivo valor da funcao objetivo
#' @export

funcao_objetivo_assimilacao_evapotranspiracao <- function(vetor_variaveis, vetor_modelo, TuInic,
      precipitacao_ponderada, evapotranspiracao, evapotranspiracao_planicie, vazao, area,
      numero_dias_assimilacao,
      pesos_funcao_objetivo = rep((1 / numero_dias_assimilacao), numero_dias_assimilacao),
      funcao_objetivo = calcula_dm) {

  EbInic <- vetor_variaveis[numero_dias_assimilacao * 2 + 1]
  Supin <- vetor_variaveis[numero_dias_assimilacao * 2 + 2]
  if (Supin < 0) { #L-BFGS-B as vezes fornece valor negativo proximo a 0 ('-1e-17')
    Supin <- 0
  }
  inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
  vetor_inicializacao <- unlist(inicializacao)

  precipitacao_ponderada <- precipitacao_ponderada * vetor_variaveis[1:numero_dias_assimilacao]

  evapotranspiracao <- evapotranspiracao * vetor_variaveis[2 * (1:numero_dias_assimilacao)]
  evapotranspiracao_planicie <- evapotranspiracao_planicie * vetor_variaveis[2 * (1:numero_dias_assimilacao)]

  simulacao <- funcaoSmapCpp::rodada_varios_dias_cpp2(vetor_modelo,
            vetor_inicializacao, area, precipitacao_ponderada,
            evapotranspiracao, evapotranspiracao_planicie, numero_dias_assimilacao)

  objetivo <- funcao_objetivo(simulacao[, 1], vazao, pesos_funcao_objetivo)
  objetivo
}
