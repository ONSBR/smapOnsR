#----------------------------------#
### FUNÇÕES DE RODADA DO MODELO ####
#----------------------------------#

#' Rodada de um dia do SMAP
#'
#' @param modelo objeto de classe smap_ons
#' @param inicializacao  Objeto resultante da funcao smap.inicializacao
#' @param precipitacao Valor de precipitacao final do dia (ja corrigido e/ou ponderado)
#' @param evapotranspiracao Valor de ETo do final dia (ja corrigido e/ou ponderado)
#' @param Emarg Valor da evaporacao final do dia do reservatorio de planicie (ja corrigido e/ou ponderado)
#' @param saidaAnterior matriz de saida da iteracao passada (primeira iteracao nao atribuir nenhum valor)
#' @param dataD Nomea a linha da matriz de saida com a data fornecida
#'
#' @return Matriz com a vazao calculada e os valores de estado e funcoes de transferencia
#' @export

rodada_diaria <- function(modelo, inicializacao, precipitacao, evapotranspiracao, Emarg, saidaAnterior = matrix(nrow = 0, ncol = 14), dataD = NULL){

  #Coeficientes Ks
  K_kts <- 0.5 ^ (1 / modelo$k_kt)
  K_1ts <- 0.5 ^ (1 / modelo$k1t)
  K_2ts <- 0.5 ^ (1 / modelo$k2t)
  K_2t2s <- 0.5 ^ (1 / modelo$k2t2)
  K_3ts <- 0.5 ^ (1 / modelo$k3t)
  
  #Se nao for fornecido nenhuma matriz no mRBind calcular os valores iniciais de Rsolo e Rsub
  RsoloInic <- inicializacao$RsoloInic
  RsupInic <- inicializacao$RsupInic
  RsubInic <- inicializacao$RsubInic
  Rsup2Inic <- inicializacao$Rsup2Inic

  #Calculo das funções de transferencia ----
  #Eqs. Referentes ao Manual de Metodologia SMAP
  Tu <- RsoloInic / modelo$str #Eq.19 Manual

  if (precipitacao > modelo$ai){
    Es <- ((precipitacao - modelo$ai) ^ 2) / (precipitacao - modelo$ai + (modelo$str - RsoloInic)) #Eq.11
  }else{
    Es <- 0
  }

  if((precipitacao - Es) > evapotranspiracao){
    Er <- evapotranspiracao
  }else{
    Er <- (precipitacao-Es) + (evapotranspiracao - (precipitacao - Es)) *Tu  #Eq.12
  }

  Capc_tmp <- (modelo$capc / 100) * modelo$str

  if(RsoloInic > Capc_tmp){
    Rec <- (modelo$crec / 100) * Tu * (RsoloInic - Capc_tmp) #Eq.13
  }else{
    Rec <- 0
  }

  #4º Reservatorio
  if (RsupInic > modelo$h){
    Marg <- (RsupInic - modelo$h) * (1 - K_1ts) #Eq.14
  }else{
    Marg <- 0
  }

  Ed <- min(c(RsupInic - Marg, modelo$h1)) * (1 - K_2ts) #Eq.15

  Ed2 <- Rsup2Inic * (1 - K_3ts) #Eq.17

  Ed3 <- max(c(RsupInic - modelo$h1 - Marg, 0)) * (1 - K_2t2s) #Eq.16

  Eb <- RsubInic * (1 - K_kts) #Eq.18

  #Calculo das variaveis de estado ----
  Rsolo <- min(c((RsoloInic + precipitacao - Es - Er - Rec), modelo$str))
  Rsub <- RsubInic + Rec - Eb
  Rsup_tmp <- ((RsoloInic + precipitacao - Es - Er - Rec) - modelo$str)
  Rsup <- RsupInic + Es - Marg - Ed - Ed3 + max(c(0, Rsup_tmp))
  Rsup2 <- max(Rsup2Inic + Marg - Ed2 - Emarg, 0)

  #Calculo da vazão ----
  Qcalc <- (Ed + Ed2 + Ed3 + Eb) * attributes(modelo)$area / 86.4

  #Matriz de Saida ----
  matrizSaida_nom <- c("Qcalc", "Rsolo", "Rsup", "Rsup2", "Rsub",
                       "Es", "Er", "Rec", "Marg", "Ed", "Ed2", "Ed3",
                       "Eb", "Tu")
  matrizSaida <- matrix(nrow = 1, ncol = length(matrizSaida_nom))
  colnames(matrizSaida) <- matrizSaida_nom
  matrizSaida[1,1] <- Qcalc
  matrizSaida[1,2] <- Rsolo
  matrizSaida[1,3] <- Rsup
  matrizSaida[1,4] <- Rsup2
  matrizSaida[1,5] <- Rsub
  matrizSaida[1,6] <- Es
  matrizSaida[1,7] <- Er
  matrizSaida[1,8] <- Rec
  matrizSaida[1,9] <- Marg
  matrizSaida[1,10] <- Ed
  matrizSaida[1,11] <- Ed2
  matrizSaida[1,12] <- Ed3
  matrizSaida[1,13] <- Eb
  matrizSaida[1,14] <- Tu

  matrizSaida <- rbind(saidaAnterior, matrizSaida)
  
}

#' Construtor do modelo classe \code{smap_ons}
#' 
#' @param parametros data table com 8910 linhas and 3 colunas:
#' \describe{
#'   \itemize{
#'     \item{nome}{nome da sub-bacia}
#'     \item{parametros}{nome do parametros}
#'     \item{valor}{valor do parametro}
#'     }
#' }
#' 
#" @return objeto de classe \code{smap_ons}
#' @export

new_modelo_smap_ons <- function(parametros){
  #Param. Gerais SMAP
  str <- parametros[parametro == "Str", valor]
  k2t <- parametros[parametro == "K2t", valor]
  crec <- parametros[parametro == "Crec", valor]
  capc <- parametros[parametro == "Capc", valor]
  k_kt <- parametros[parametro == "K_kt", valor]
  h1 <- parametros[parametro == "H1", valor]
  k2t2 <- parametros[parametro == "K2t2", valor]
  area <- parametros[parametro == "Area", valor]
  ai <- parametros[parametro == "Ai", valor]

  #Param. 4 Reserv
  h <- parametros[parametro == "H", valor]
  k1t <- parametros[parametro == "K1t", valor]
  k3t <- parametros[parametro == "K3t", valor]

  #coeficiente temporal
  kt <- parametros[, valor][3:65]
  names(kt) <- parametros[, parametro][3:65]

  #coeficientes de ponderacao
  pcof <- parametros[parametro == "Pcof", valor]
  ecof <- parametros[parametro == "Ecof", valor]
  ecof2 <- parametros[parametro == "Ecof2", valor]

  modelo <- list(str = str, k2t = k2t, crec = crec, capc = capc, k_kt = k_kt,
  h1 = h1, k2t2 = k2t2, ai = ai, h = h, l1t = k1t, k3t = k3t, kt = kt, pcof = pcof,
  ecof = ecof, ecof2 = ecof2)

  attr(modelo, "nome") <- parametros[, unique(Nome)]
  attr(modelo, "area") <- area
  
  class(modelo) <- "smap_ons"
  modelo
}

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
    
    inicializacao$Rsup2Inic <- saida[idia, 2]
    inicializacao$RsupInic <- saida[idia, 3]
    inicializacao$Rsup2Inic <- saida[idia, 4]
    inicializacao$RsubInic <- saida[idia, 5]
  }
  dat <- data.table(saida)
  dat[ ,data := precipitacao_ponderada[, data]]

  objetivo <- calcula_dm(dat[data >= data_inicio_objetivo & data <= data_fim_objetivo, Qcalc],
                         as.numeric(vazao[data >= data_inicio_objetivo & data <= data_fim_objetivo, valor]))
  objetivo
}