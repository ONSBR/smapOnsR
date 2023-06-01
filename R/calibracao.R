
#' funcao objetivo de calibracao do SMAP/ONS
#'
#' @param modelo objeto de classe smap_ons
#' @param EbInic vazao de base inicial
#' @param TuInic umidade do solo inicial
#' @param Supin vazao superficial inicial
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
#' 
#' @param data_inicio_objetivo data inicial da avaliacao da funcao objetivo
#' @param data_fim_objetivo data final da avaliacao da funcao objetivo
#' @importFrom data.table data.table
#' @return objetivo valor da funcao objetivo
#' @export

funcao_objetivo <- function(modelo, EbInic, TuInic, Supin, precipitacao, 
      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo){
  
  kt_min <- sum(modelo$kt[4:63] > 0)
  kt_max <- sum(modelo$kt[1:2] > 0)

  data_inicio <- precipitacao[, min(data)] + kt_min
  data_fim <- precipitacao[, max(data)] + kt_max
  numero_dias <- as.numeric(data_fim - data_inicio - 1)

  inicializacao <- smap_ons.inic(modelo, EbInic, TuInic, Supin)

  precipitacao_ponderada = precipitacao
  precipitacao_ponderada[, valor := valor * modelo$pcof]
  precipitacao_ponderada <- poderacao_temporal(precipitacao_ponderada, modelo, data_inicio, data_fim-2)

  evapotranspiracao_ponderada <- evapotranspiracao
  evapotranspiracao_ponderada[, valor := valor * modelo$ecof]
  evapotranspiracao_planicie_ponderada <- evapotranspiracao_ponderada
  evapotranspiracao_planicie_ponderada[, valor := valor * modelo$ecof]
  
  saida <- matrix(nrow = 0, ncol = 14)
  for (idia in 1:numero_dias) {
    saida <- rodada_diaria(modelo, inicializacao, precipitacao_ponderada[idia, valor],
    evapotranspiracao_ponderada[idia, valor], evapotranspiracao_planicie_ponderada[idia, valor],
    saida, precipitacao_ponderada[idia, data])
    
    inicializacao$RsoloInic <- saida[idia, 2]
    inicializacao$RsupInic <- saida[idia, 3]
    inicializacao$Rsup2Inic <- saida[idia, 4]
    inicializacao$RsubInic <- saida[idia, 5]
  }
  dat <- data.table::data.table(saida)
  dat[ ,data := precipitacao_ponderada[, data]]

  objetivo <- calcula_dm(dat[data >= data_inicio_objetivo & data <= data_fim_objetivo, Qcalc],
                         as.numeric(vazao[data >= data_inicio_objetivo & data <= data_fim_objetivo, valor]))
  objetivo
}


calibracao <- function(modelo, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo,
      limite_inferior, limite_superior){

  ajuste <- stats::optim(par = modelo, method = "L-BFGS-B",
              lower = limite_inferior, upper = limite_superior,
              fn = funcao_objetivo,
              EbInic = EbInic, TuInic = TuInic, Supin = Supin,
              precipitacao = precipitacao,
              evapotranspiracao = evapotranspiracao,
              vazao = vazao,
              data_inicio_objetivo = data_inicio_objetivo,
              data_fim_objetivo = data_fim_objetivo,
              
              control = list(fnscale = -1))
  parametros <- ajuste$par
}
