#----------------------------------#
### FUNÇÕES DE RODADA DO MODELO ####
#----------------------------------#

#' Rodada de varios dias do SMAP
#'
#' @param modelo objeto de classe smap_ons
#' @param inicializacao  Objeto resultante da funcao smap.inicializacao
#' @param precipitacao Vetor de precipitacao final (ja corrigido e/ou ponderado)
#' @param evapotranspiracao Vetor de ETo do final(ja corrigido e/ou ponderado)
#' @param Emarg Vetor da evaporacao final do reservatorio de planicie (ja corrigido e/ou ponderado)
#' @param numero_dias numero de dias da rodada
#'
#' @return Matriz com a vazao calculada e os valores de estado e funcoes de transferencia
#' @export

rodada_varios_dias <- function(modelo, inicializacao, precipitacao, evapotranspiracao, Emarg, numero_dias){
  
  #Matriz de Saida ----
  matrizSaida_nom <- c("Qcalc", "Rsolo", "Rsup", "Rsup2", "Rsub",
                      "Es", "Er", "Rec", "Marg", "Ed", "Ed2", "Ed3",
                      "Eb", "Tu")
  matrizSaida <- matrix(nrow = numero_dias, ncol = length(matrizSaida_nom))
  colnames(matrizSaida) <- matrizSaida_nom

  RsoloInic <- inicializacao$RsoloInic
  RsupInic <- inicializacao$RsupInic
  RsubInic <- inicializacao$RsubInic
  Rsup2Inic <- inicializacao$Rsup2Inic
  for (idia in 1:numero_dias) {

    #Coeficientes Ks
    K_kts <- 0.5 ^ (1 / modelo$k_kt)
    K_1ts <- 0.5 ^ (1 / modelo$k1t)
    K_2ts <- 0.5 ^ (1 / modelo$k2t)
    K_2t2s <- 0.5 ^ (1 / modelo$k2t2)
    K_3ts <- 0.5 ^ (1 / modelo$k3t)

    #Calculo das funções de transferencia ----
    #Eqs. Referentes ao Manual de Metodologia SMAP
    matrizSaida[idia, 14] <- RsoloInic / modelo$str #Eq.19 Manual

    if (precipitacao[idia] > modelo$ai){
      matrizSaida[idia, 6] <- ((precipitacao[idia] - modelo$ai) ^ 2) / (precipitacao[idia] - modelo$ai + (modelo$str - RsoloInic)) #Eq.11
    }else{
      matrizSaida[idia, 6] <- 0
    }

    if((precipitacao[idia] - matrizSaida[idia, 6]) > evapotranspiracao[idia]){
      matrizSaida[idia, 7] <- evapotranspiracao[idia]
    }else{
      matrizSaida[idia, 7] <- (precipitacao[idia]-matrizSaida[idia, 6]) + (evapotranspiracao[idia] - (precipitacao[idia] - matrizSaida[idia, 6])) * matrizSaida[idia, 14]  #Eq.12
    }

    Capc_tmp <- (modelo$capc / 100) * modelo$str

    if(RsoloInic > Capc_tmp){
      matrizSaida[idia, 8] <- (modelo$crec / 100) * matrizSaida[idia, 14] * (RsoloInic - Capc_tmp) #Eq.13
    }else{
      matrizSaida[idia, 8] <- 0
    }

    #4º Reservatorio
    if (RsupInic > modelo$h){
      matrizSaida[idia, 9] <- (RsupInic - modelo$h) * (1 - K_1ts) #Eq.14
    }else{
      matrizSaida[idia, 9] <- 0
    }

    matrizSaida[idia, 10] <- min(c(RsupInic - matrizSaida[idia, 9], modelo$h1)) * (1 - K_2ts) #Eq.15

    matrizSaida[idia, 11] <- Rsup2Inic * (1 - K_3ts) #Eq.17

    matrizSaida[idia, 12] <- max(c(RsupInic - modelo$h1 - matrizSaida[idia, 9], 0)) * (1 - K_2t2s) #Eq.16

    matrizSaida[idia, 13] <- RsubInic * (1 - K_kts) #Eq.18

    #Calculo das variaveis de estado ----
    matrizSaida[idia, 2] <- min(c((RsoloInic + precipitacao[idia] - matrizSaida[idia, 6] - matrizSaida[idia, 7] - matrizSaida[idia, 8]), modelo$str))
    matrizSaida[idia, 5] <- RsubInic + matrizSaida[idia, 8] - matrizSaida[idia, 13]
    Rsup_tmp <- ((RsoloInic + precipitacao[idia] - matrizSaida[idia, 6] - matrizSaida[idia, 7] - matrizSaida[idia, 8]) - modelo$str)
    matrizSaida[idia, 3] <- RsupInic + matrizSaida[idia, 6] - matrizSaida[idia, 9] - matrizSaida[idia, 10] - matrizSaida[idia, 12] + max(c(0, Rsup_tmp))
    matrizSaida[idia, 4] <- max(Rsup2Inic + matrizSaida[idia, 9] - matrizSaida[idia, 11] - Emarg[idia], 0)

    #Calculo da vazão ----
    matrizSaida[idia, 1] <- (matrizSaida[idia, 10] + matrizSaida[idia, 11] + matrizSaida[idia, 12] + matrizSaida[idia, 13]) * attributes(modelo)$area / 86.4

    RsoloInic <- matrizSaida[idia, 2]
    RsupInic <- matrizSaida[idia, 3]
    Rsup2Inic <- matrizSaida[idia, 4]
    RsubInic <- matrizSaida[idia, 5]
  }

  matrizSaida
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

new_modelo_smap_ons <- function(parametros, postos_plu){
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
  h1 = h1, k2t2 = k2t2, ai = ai, h = h, k1t = k1t, k3t = k3t, kt = kt, pcof = pcof,
  ecof = ecof, ecof2 = ecof2)

  attr(modelo, "nome") <- parametros[, unique(Nome)]
  attr(modelo, "area") <- area
  attr(modelo, "postos_plu") <- postos_plu

  class(modelo) <- "smap_ons"
  modelo
}