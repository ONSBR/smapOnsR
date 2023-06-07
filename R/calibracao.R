#' funcao objetivo de calibracao do SMAP/ONS
#'
#' @param modelo vetor resultante de unlist do objeto de classe smap_ons
#' @param area area da sub-bacia
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

funcao_objetivo <- function(modelo, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo){
  
  kt <- modelo[12:74]
  kt_max <- sum(kt[1:2] > 0)
  kt_min <- sum(kt[4:63] > 0)

  data_inicio <- precipitacao[, min(data)] + kt_min
  data_fim <- precipitacao[, max(data)] - kt_max
  numero_dias <- as.numeric(data_fim - data_inicio + 1)

  inicializacao <- smap_ons.inic(modelo, area, EbInic, TuInic, Supin)

  precipitacao_ponderada <- data.table::data.table(precipitacao)
  precipitacao_ponderada[, valor := valor * modelo[75]]
  precipitacao_ponderada <- poderacao_temporal(precipitacao_ponderada, kt, data_inicio, data_fim)

  evapotranspiracao_ponderada <- data.table::data.table(evapotranspiracao[data >= data_inicio & data <= data_fim])
  evapotranspiracao_ponderada[, valor := valor * modelo[76]]
  evapotranspiracao_planicie_ponderada <- data.table::data.table(evapotranspiracao)
  evapotranspiracao_planicie_ponderada[, valor := valor * modelo[77]]
  
  saida <- matrix(nrow = numero_dias, ncol = 14)
  vetorInicializacao <- unlist(inicializacao)

  saida <- rodada_varios_dias_cpp(modelo,
            vetorInicializacao, area, precipitacao_ponderada[, valor],
            evapotranspiracao_ponderada[, valor], evapotranspiracao_planicie_ponderada[, valor], numero_dias)
  
  dat <- data.table::data.table(saida)
  dat[ ,data := precipitacao_ponderada[, data]]

  objetivo <- calcula_dm(dat[data >= data_inicio_objetivo & data <= data_fim_objetivo, Qcalc],
                         vazao[data >= data_inicio_objetivo & data <= data_fim_objetivo, valor])
  objetivo
}

#' funcao de calibracao do SMAP/ONS
#'
#' @param modelo vetor resultante de unlist do objeto de classe smap_ons
#' @param area area da sub-bacia
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
#' @param limite_inferior vetor com o limite inferior dos parametros
#' @param limite_superior vetor com o limite superior dos parametros
#' @importFrom data.table data.table
#' @return objetivo valor da funcao objetivo
#' @export
#' 
calibracao <- function(modelo, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo,
      limite_inferior, limite_superior){
  
  ajuste <- stats::optim(par = modelo, method = "L-BFGS-B",
              lower = limite_inferior, upper = limite_superior,
              fn = funcao_objetivo,
              area = area,
              EbInic = EbInic, TuInic = TuInic, Supin = Supin,
              precipitacao = precipitacao,
              evapotranspiracao = evapotranspiracao,
              vazao = vazao,
              data_inicio_objetivo = data_inicio_objetivo,
              data_fim_objetivo = data_fim_objetivo,
              control = list(fnscale = 1))

  ajuste <- pso::psoptim(par = modelo,
              lower = limite_inferior, upper = limite_superior,
              fn = funcao_objetivo,
              area = area,
              EbInic = EbInic, TuInic = TuInic, Supin = Supin,
              precipitacao = precipitacao,
              evapotranspiracao = evapotranspiracao,
              vazao = vazao,
              data_inicio_objetivo = data_inicio_objetivo,
              data_fim_objetivo = data_fim_objetivo,
              control = list(fnscale = 1))

  ajuste$par
  ajuste
}
