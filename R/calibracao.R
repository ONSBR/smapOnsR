#' funcao objetivo de calibracao do SMAP/ONS
#' 
#' Realiza o calculo da funcao objetivo para a calibracao do SMAP/ONS
#'
#' @param vetor_modelo vetor resultante de unlist do objeto de classe smap_ons
#' @param kt_max valor do maximo lag positivo
#' @param kt_min valor do maximo lag maximo negativo
#' @param area area da sub-bacia
#' @param EbInic vazao de base inicial
#' @param TuInic umidade do solo inicial
#' @param Supin vazao superficial inicial
#' @param precipitacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param evapotranspiracao data table com a evapotranspiracao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param vazao data table com a vazao a avaliada
#'      \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param postos_plu data.table com as colunas
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{posto - nome do posto plu}
#'     \item{valor - peso do posto plu}
#'     }
#' @param pesos vetor de pesos a serem utilizados para cada data durante a calibracao
#' @param inicio_objetivo inicio do calculo da funcao objetivo
#' @param fim_objetivo fim do calculo da funcao objetivo
#' @param numero_dias numero de dias da simulacao para a calibracao
#' @param numero_postos_plu numero de posto_plu considerados
#' @param funcao_objetivo funcao objetivo a ser utilizada na calibracao
#' 
#' @examples 
#' nome2 <- "baixoig"
#'  modelo <- new_modelo_smap_ons(parametros[nome == nome2], postos_plu[nome == nome2])
#'  kt_max <- sum(modelo$kt[1:2] > 0)
#'  kt_min <- sum(modelo$kt[4:63] > 0)
#'
#'  EbInic <- 300
#'  TuInic <- 0.50
#'  Supin <- 700
#'
#'  normal_climatologica <- historico_etp_NC[nome == nome2]
#'  precipitacao <- historico_precipitacao[posto %in% postos_plu[nome == nome2, posto]]
#'  precipitacao_ponderada <- ponderacao_espacial(precipitacao, postos_plu[nome == nome2])
#'  data_inicio_objetivo <- as.Date("2011-01-01")
#'  data_fim_objetivo <- as.Date("2021-12-31") - kt_max
#'  evapotranspiracao <- transforma_NC_serie(precipitacao_ponderada[data >= min(data) + kt_min & data <= data_fim_objetivo], normal_climatologica)
#'  vazao <- historico_vazao[data >= data_inicio_objetivo & data <= data_fim_objetivo & posto == nome2]
#'
#'  area <- attributes(modelo)$area
#'  vetor_modelo <- unlist(modelo)
#'  vetor_modelo <- c(vetor_modelo[1:11], vetor_modelo[75:77], 5, 5)
#'  numero_postos_plu <- nrow(postos_plu[nome == nome2])
#'
#'  limite_inferior <- vetor_modelo * 0.01
#'  limite_superior <- vetor_modelo * 10
#'  limite_inferior[8] <- 0.9999999
#'  limite_superior[8] <- 1.0000001
#'  limite_inferior[12] <- 0.8
#'  limite_superior[12] <- 1.2
#'  limite_inferior[13] <- 0.8
#'  limite_superior[13] <- 1.2
#'  limite_inferior[14] <- 0.8
#'  limite_superior[14] <- 1.2
#'  limite_inferior[15:16] <- 0.00001
#'  limite_superior[15:16] <- 50
#'
#'  numero_dias <- nrow(evapotranspiracao)
#'  inicio_objetivo <- evapotranspiracao[data <= data_inicio_objetivo, .N]
#'  fim_objetivo <- evapotranspiracao[data <= data_fim_objetivo, .N]
#'
#' \dontrun{
#'    fo <- funcao_objetivo_calibracao(vetor_modelo, kt_min, kt_max, area, EbInic, TuInic, Supin, precipitacao,
#'      evapotranspiracao, vazao, inicio_objetivo, fim_objetivo, postos_plu[nome == nome2],
#'      pesos = rep(1 / length(vazao[, valor]), length(vazao[, valor])), numero_dias, numero_postos_plu)
#' }
#' @importFrom data.table data.table
#' @importFrom stats dbeta
#' @return objetivo valor da funcao objetivo
#' @export

funcao_objetivo_calibracao <- function(vetor_modelo, kt_min, kt_max, area, EbInic, TuInic, Supin, 
      precipitacao, evapotranspiracao, vazao, inicio_objetivo, fim_objetivo,
      postos_plu, pesos = rep(1 / length(vazao[, valor]), length(vazao[, valor])), numero_dias,
      numero_postos_plu, funcao_objetivo = calcula_dm) {

  kt <- cria_kt(kt_max, kt_min, vetor_modelo[15], vetor_modelo[16])

  inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)
  if (numero_postos_plu > 1) {
    vetor_modelo[17:(16 + numero_postos_plu)] <- vetor_modelo[17:(16 + numero_postos_plu)] / sum(vetor_modelo[17:(16 + numero_postos_plu)])
    postos_plu$valor <- vetor_modelo[17:(16 + numero_postos_plu)]
  }

  precipitacao_ponderada <- ponderacao_espacial(precipitacao, postos_plu)
  
  precipitacao_ponderada[, valor := valor * vetor_modelo[12]]
  precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt, kt_max, kt_min)

  evapotranspiracao_ponderada <- data.table::copy(evapotranspiracao)
  evapotranspiracao_ponderada[, valor := valor * vetor_modelo[13]]
  evapotranspiracao_planicie_ponderada <- data.table::copy(evapotranspiracao)
  evapotranspiracao_planicie_ponderada[, valor := valor * vetor_modelo[14]]

  vetor_inicializacao <- unlist(inicializacao)

  saida <- funcaoSmapCpp::rodada_varios_dias_cpp2(vetor_modelo,
            vetor_inicializacao, area, precipitacao_ponderada,
            evapotranspiracao_ponderada[, valor], evapotranspiracao_planicie_ponderada[, valor], numero_dias)
  
  dat <- data.table::data.table(saida)

  objetivo <- funcao_objetivo(dat[inicio_objetivo:fim_objetivo, Qcalc],
                         vazao[, valor], pesos)
  objetivo
}

#' Funcao de criacao de vetor de kts
#' 
#' funcao para a geracao do vetor de kt atraves de uma distribuicao beta
#'
#' @param kt_max valor do maximo lag positivo
#' @param kt_min valor do maximo lag maximo negativo
#' @param alfa parametro alfa da distribuicao beta
#' @param beta parametro beta da distribuicao beta
#' @importFrom stats dbeta
#' @return kt vetor de kts
#' @export
cria_kt <- function(kt_max, kt_min, alfa, beta){
  numero_kts <- kt_max + kt_min + 1
  quantis <- seq((1 / (numero_kts + 2)), 1, (1 / numero_kts))

  aux <- stats::dbeta(quantis, shape1 = alfa, shape2 = beta)

  kt <- rep(0, 63)
  kt[(3 - kt_max):(3 + kt_min)] <- aux / sum(aux)

  kt
}

#' Funcao de calibracao do modelo SMAP/ONS
#' 
#' Raliza a calibracao de calibracao do SMAP/ONS dado um vetor inicial de pearametros e seus limites superiores e inferiores
#'
#' @param vetor_modelo vetor resultante de unlist do objeto de classe smap_ons
#' @param kt_max valor do maximo lag positivo
#' @param kt_min valor do maximo lag maximo negativo
#' @param area area da sub-bacia
#' @param EbInic vazao de base inicial
#' @param TuInic umidade do solo inicial
#' @param Supin vazao superficial inicial
#' @param precipitacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param evapotranspiracao data table com a evapotranspiracao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param vazao data table com a vazao a avaliada
#'      \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' 
#' @param data_inicio_objetivo data inicial da avaliacao da funcao objetivo
#' @param data_fim_objetivo data final da avaliacao da funcao objetivo
#' @param limite_inferior vetor com o limite inferior dos parametros
#' @param limite_superior vetor com o limite superior dos parametros
#' @param postos_plu data.table com as colunas
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{posto - nome do posto plu}
#'     \item{valor - peso do posto plu}
#'     }
#' @param funcao_objetivo funcao objetivo a ser utilizada na calibracao
#' @param fnscale 1 indica minimizacao da funcao objetivo / -1 maximizacao
#' @param pesos vetor de pesos a serem utilizados para cada data durante a calibracao
#' @examples 
#' nome2 <- "baixoig"
#'  modelo <- new_modelo_smap_ons(parametros[nome == nome2], postos_plu[nome == nome2])
#'  kt_max <- sum(modelo$kt[1:2] > 0)
#'  kt_min <- sum(modelo$kt[4:63] > 0)
#'
#'  EbInic <- 300
#'  TuInic <- 0.50
#'  Supin <- 700
#'
#'  normal_climatologica <- historico_etp_NC[nome == nome2]
#'  precipitacao <- historico_precipitacao[posto %in% postos_plu[nome == nome2, posto]]
#'  precipitacao_ponderada <- ponderacao_espacial(precipitacao, postos_plu[nome == nome2])
#'  data_inicio_objetivo <- as.Date("2011-01-01")
#'  data_fim_objetivo <- as.Date("2021-12-31") - kt_max
#'  evapotranspiracao <- transforma_NC_serie(precipitacao_ponderada[data >= min(data) + kt_min & data <= data_fim_objetivo], normal_climatologica)
#'  vazao <- historico_vazao[data >= data_inicio_objetivo & data <= data_fim_objetivo & posto == nome2]
#'
#'  area <- attributes(modelo)$area
#'  vetor_modelo <- unlist(modelo)
#'  vetor_modelo <- c(vetor_modelo[1:11], vetor_modelo[75:77], 5, 5)
#'  numero_postos_plu <- nrow(postos_plu[nome == nome2])
#'
#'  limite_inferior <- vetor_modelo * 0.01
#'  limite_superior <- vetor_modelo * 10
#'  limite_inferior[8] <- 0.9999999
#'  limite_superior[8] <- 1.0000001
#'  limite_inferior[12] <- 0.8
#'  limite_superior[12] <- 1.2
#'  limite_inferior[13] <- 0.8
#'  limite_superior[13] <- 1.2
#'  limite_inferior[14] <- 0.8
#'  limite_superior[14] <- 1.2
#'  limite_inferior[15:16] <- 0.00001
#'  limite_superior[15:16] <- 50
#'
#'  numero_dias <- nrow(evapotranspiracao)
#'  inicio_objetivo <- evapotranspiracao[data <= data_inicio_objetivo, .N]
#'  fim_objetivo <- evapotranspiracao[data <= data_fim_objetivo, .N]
#'
#' \dontrun{
#'  par <- calibracao(vetor_modelo,  kt_min, kt_max, area, EbInic, TuInic, Supin, precipitacao,
#'      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo,
#'      limite_inferior, limite_superior, postos_plu[nome == nome2])
#' }

#' @importFrom data.table data.table
#' @return ajuste valor da funcao objetivo
#' @export
calibracao <- function(vetor_modelo, kt_min, kt_max, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo,
      limite_inferior, limite_superior, postos_plu, funcao_objetivo = calcula_dm, fnscale = 1,
      pesos = rep(1 / length(vazao[, valor]), length(vazao[, valor]))) {

  if (length(unique(limite_inferior == limite_superior)) == 2) {
    limite_superior[limite_inferior == limite_superior] <- limite_superior[limite_inferior == limite_superior] + 0.000001
  }

  numero_postos_plu <- nrow(postos_plu)
  numero_dias <- nrow(evapotranspiracao)
  inicio_objetivo <- evapotranspiracao[data <= data_inicio_objetivo, .N]
  fim_objetivo <- evapotranspiracao[data <= data_fim_objetivo, .N]

  ajuste <- stats::optim(par = vetor_modelo, method = "L-BFGS-B",
              lower = limite_inferior, upper = limite_superior,
              fn = funcao_objetivo_calibracao,
              kt_min = kt_min,
              kt_max = kt_max,
              area = area,
              EbInic = EbInic, TuInic = TuInic, Supin = Supin,
              precipitacao = precipitacao,
              evapotranspiracao = evapotranspiracao,
              vazao = vazao,
              inicio_objetivo = inicio_objetivo,
              fim_objetivo = fim_objetivo,
              postos_plu = postos_plu,
              pesos = pesos,
              numero_dias = numero_dias,
              numero_postos_plu = numero_postos_plu,
              funcao_objetivo = funcao_objetivo,
              control = list(fnscale = fnscale))
  
  if (numero_postos_plu > 1) {
    ajuste$par[17:(16 + numero_postos_plu)] <- ajuste$par[17:(16 + numero_postos_plu)] / sum(ajuste$par[17:(16 + numero_postos_plu)])
  }
  ajuste
}

#' Funcao de calibracao do modelo SMAP/ONS
#' 
#' Raliza a calibracao de calibracao do SMAP/ONS dado um vetor inicial de pearametros e seus limites superiores e inferiores
#'
#' @param vetor_modelo vetor resultante de unlist do objeto de classe smap_ons
#' @param kt_max valor do maximo lag positivo
#' @param kt_min valor do maximo lag maximo negativo
#' @param area area da sub-bacia
#' @param EbInic vazao de base inicial
#' @param TuInic umidade do solo inicial
#' @param Supin vazao superficial inicial
#' @param precipitacao data table com a precipitacao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param evapotranspiracao data table com a evapotranspiracao a ser ponderada com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' @param vazao data table com a vazao a avaliada
#'      \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da variavel}
#'     }
#' 
#' @param data_inicio_objetivo data inicial da avaliacao da funcao objetivo
#' @param data_fim_objetivo data final da avaliacao da funcao objetivo
#' @param limite_inferior vetor com o limite inferior dos parametros
#' @param limite_superior vetor com o limite superior dos parametros
#' @param postos_plu data.table com as colunas
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{posto - nome do posto plu}
#'     \item{valor - peso do posto plu}
#'     }
#' @param funcao_objetivo funcao objetivo a ser utilizada na calibracao
#' @param fnscale 1 indica minimizacao da funcao objetivo / -1 maximizacao
#' @param pesos vetor de pesos a serem utilizados para cada data durante a calibracao
#' @examples 
#' nome2 <- "baixoig"
#'  modelo <- new_modelo_smap_ons(parametros[nome == nome2], postos_plu[nome == nome2])
#'  kt_max <- sum(modelo$kt[1:2] > 0)
#'  kt_min <- sum(modelo$kt[4:63] > 0)
#'
#'  EbInic <- 300
#'  TuInic <- 0.50
#'  Supin <- 700
#'
#'  normal_climatologica <- historico_etp_NC[nome == nome2]
#'  precipitacao <- historico_precipitacao[posto %in% postos_plu[nome == nome2, posto]]
#'  precipitacao_ponderada <- ponderacao_espacial(precipitacao, postos_plu[nome == nome2])
#'  data_inicio_objetivo <- as.Date("2011-01-01")
#'  data_fim_objetivo <- as.Date("2021-12-31") - kt_max
#'  evapotranspiracao <- transforma_NC_serie(precipitacao_ponderada[data >= min(data) + kt_min & data <= data_fim_objetivo], normal_climatologica)
#'  vazao <- historico_vazao[data >= data_inicio_objetivo & data <= data_fim_objetivo & posto == nome2]
#'
#'  area <- attributes(modelo)$area
#'  vetor_modelo <- unlist(modelo)
#'  vetor_modelo <- c(vetor_modelo[1:11], vetor_modelo[75:77], 5, 5)
#'  numero_postos_plu <- nrow(postos_plu[nome == nome2])
#'
#'  limite_inferior <- vetor_modelo * 0.01
#'  limite_superior <- vetor_modelo * 10
#'  limite_inferior[8] <- 0.9999999
#'  limite_superior[8] <- 1.0000001
#'  limite_inferior[12] <- 0.8
#'  limite_superior[12] <- 1.2
#'  limite_inferior[13] <- 0.8
#'  limite_superior[13] <- 1.2
#'  limite_inferior[14] <- 0.8
#'  limite_superior[14] <- 1.2
#'  limite_inferior[15:16] <- 0.00001
#'  limite_superior[15:16] <- 50
#'
#'  numero_dias <- nrow(evapotranspiracao)
#'  inicio_objetivo <- evapotranspiracao[data <= data_inicio_objetivo, .N]
#'  fim_objetivo <- evapotranspiracao[data <= data_fim_objetivo, .N]
#'
#' \dontrun{
#'  par <- calibracao(vetor_modelo,  kt_min, kt_max, area, EbInic, TuInic, Supin, precipitacao,
#'      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo,
#'      limite_inferior, limite_superior, postos_plu[nome == nome2])
#' }
#' @importFrom data.table data.table
#' @importFrom optimParallel optimParallel
#' @importFrom parallel detectCores makeCluster clusterEvalQ stopCluster
#' @importFrom doParallel registerDoParallel 
#' @return ajuste valor da funcao objetivo
#' @export
calibracao_paralela <- function(vetor_modelo, kt_min, kt_max, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo,
      limite_inferior, limite_superior, postos_plu, funcao_objetivo = calcula_dm, fnscale = 1,
      pesos = rep(1 / length(vazao[, valor]), length(vazao[, valor]))) {

  if (length(unique(limite_inferior == limite_superior)) == 2) {
    limite_superior[limite_inferior == limite_superior] <- limite_superior[limite_inferior == limite_superior] + 0.000001
  }

  numero_postos_plu <- nrow(postos_plu)
  numero_dias <- nrow(evapotranspiracao)
  inicio_objetivo <- evapotranspiracao[data <= data_inicio_objetivo, .N]
  fim_objetivo <- evapotranspiracao[data <= data_fim_objetivo, .N]

  #------------------paralelismo
  cores <- parallel::detectCores()
  cl <- parallel::makeCluster(cores[1] - 1)
  doParallel::registerDoParallel(cl)
  parallel::clusterEvalQ(cl, {library("smapOnsR")})

  ajuste <- optimParallel::optimParallel(par = vetor_modelo, method = "L-BFGS-B",
              lower = limite_inferior, upper = limite_superior,
              fn = funcao_objetivo_calibracao,
              kt_min = kt_min,
              kt_max = kt_max,
              area = area,
              EbInic = EbInic, TuInic = TuInic, Supin = Supin,
              precipitacao = precipitacao,
              evapotranspiracao = evapotranspiracao,
              vazao = vazao,
              inicio_objetivo = inicio_objetivo,
              fim_objetivo = fim_objetivo,
              postos_plu = postos_plu,
              pesos = pesos,
              numero_dias = numero_dias,
              numero_postos_plu = numero_postos_plu,
              funcao_objetivo = funcao_objetivo,
              parallel = list(cl = cl, forward = FALSE, loginfo = TRUE),
              control = list(fnscale = fnscale))

  parallel::stopCluster(cl)
  
  if (numero_postos_plu > 1) {
    ajuste$par[17:(16 + numero_postos_plu)] <- ajuste$par[17:(16 + numero_postos_plu)] / sum(ajuste$par[17:(16 + numero_postos_plu)])
  }
  ajuste
}