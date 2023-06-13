#' funcao objetivo de calibracao do SMAP/ONS
#' 
#' Realiza o calculo da funcao objetivo para a calibracao do SMAP/ONS
#'
#' @param vetor_modelo vetor resultante de unlist do objeto de classe smap_ons
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
#' @importFrom stats dbeta
#' @return objetivo valor da funcao objetivo
#' @export

funcao_objetivo_calibracao <- function(vetor_modelo, kt_min, kt_max, area, EbInic, TuInic, Supin, 
      precipitacao, evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo){

  kt <- cria_kt(kt_max, kt_min, vetor_modelo[15], vetor_modelo[16])

  numero_dias <- nrow(evapotranspiracao)

  inicializacao <- inicializacao_smap(vetor_modelo, area, EbInic, TuInic, Supin)

  precipitacao_ponderada <- data.table::data.table(precipitacao)
  precipitacao_ponderada[, valor := valor * vetor_modelo[12]]
  precipitacao_ponderada <- ponderacao_temporal(precipitacao_ponderada[, valor], kt, kt_max, kt_min)

  evapotranspiracao_ponderada <- data.table::data.table(evapotranspiracao)
  evapotranspiracao_ponderada[, valor := valor * vetor_modelo[13]]
  evapotranspiracao_planicie_ponderada <- data.table::data.table(evapotranspiracao)
  evapotranspiracao_planicie_ponderada[, valor := valor * vetor_modelo[14]]

  vetorInicializacao <- unlist(inicializacao)

  saida <- rodada_varios_dias_cpp(vetor_modelo,
            vetorInicializacao, area, precipitacao_ponderada,
            evapotranspiracao_ponderada[, valor], evapotranspiracao_planicie_ponderada[, valor], numero_dias)
  
  dat <- data.table::data.table(saida)
  dat[, data := evapotranspiracao_ponderada[, data]]

  objetivo <- calcula_dm(dat[data >= data_inicio_objetivo & data <= data_fim_objetivo, Qcalc],
                         vazao[, valor])
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
calibracao <- function(vetor_modelo, kt_min, kt_max, area, EbInic, TuInic, Supin, precipitacao,
      evapotranspiracao, vazao, data_inicio_objetivo, data_fim_objetivo,
      limite_inferior, limite_superior){
  
  if(length(unique(limite_inferior == limite_superior)) == 2){
    limite_superior[limite_inferior == limite_superior] <- limite_superior[limite_inferior == limite_superior] + 0.000001
  }
  
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
              data_inicio_objetivo = data_inicio_objetivo,
              data_fim_objetivo = data_fim_objetivo,
              control = list(fnscale = 1))
  ajuste
}
