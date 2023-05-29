#----------------------------------#
### FUNÇÕES DE RODADA DO MODELO ####
#----------------------------------#

#' Rodada de um dia do SMAP
#'
#' @param parametros data table com 8910 linhas and 3 colunas:
#' \describe{
#'   \itemize{
#'     \item{nome}{nome da sub-bacia}
#'     \item{parametros}{nome do parametros}
#'     \item{valor}{valor do parametro}
#'     }
#' }
#' @param inicializacao  Objeto resultante da funcao smap.inicializacao
#' @param precipitacao Valor de precipitacao final do dia (ja corrigido e/ou ponderado)
#' @param evapotranspiracao Valor de ETo do final dia (ja corrigido e/ou ponderado)
#' @param Emarg Valor da evaporacao final do dia do reservatorio de planicie (ja corrigido e/ou ponderado)
#' @param saidaAnterior matriz de saida da iteracao passada (primeira iteracao nao atribuir nenhum valor)
#' @param dataD Nomea a linha da matriz de saida com a data fornecida
#'
#' @return Matriz com a vazao calculada e os valores de estado e funcoes de transferencia
#' @export

smap_ons.previsao <- function(parametros, inicializacao, precipitacao, evapotranspiracao, Emarg, saidaAnterior = matrix(nrow = 0, ncol = length(matrizSaida_nom)), dataD = NULL){

  #Param. Gerais SMAP
  Str <- parametros[parametro == "Str", valor]
  K2t <- parametros[parametro == "K2t", valor]
  Crec <- parametros[parametro == "Crec", valor]
  Capc <- parametros[parametro == "Capc", valor]
  K_kt <- parametros[parametro == "K_kt", valor]
  H1 <- parametros[parametro == "H1", valor]
  K2t2 <- parametros[parametro == "K2t2", valor]
  area <- parametros[parametro == "Area", valor]
  Ai <- parametros[parametro == "Ai", valor]

  #Param. 4 Reserv
  H <- parametros[parametro == "H", valor]
  K1t <- parametros[parametro == "K1t", valor]
  K3t <- parametros[parametro == "K3t", valor]

  #Coeficientes Ks
  K_kts <- parametros[parametro == "K_kts", valor]
  K_1ts <- parametros[parametro == "K_1ts", valor]
  K_2ts <- parametros[parametro == "K_2ts", valor]
  K_2t2s <- parametros[parametro == "K_2t2s", valor]
  K_3ts <- parametros[parametro == "K_3ts", valor]
  

  #Se nao for fornecido nenhuma matriz no mRBind calcular os valores iniciais de Rsolo e Rsub
  if(missing(saidaAnterior)){
    RsoloInic <- inicializacao$RsoloInic
    RsupInic <- inicializacao$RsupInic
    Rsup2Inic <- inicializacao$Rsup2Inic
    RsubInic <- inicializacao$RsubInic
  }else{
    rb_miss <- FALSE
    tmp <- nrow(saidaAnterior)
    RsoloInic <- saidaAnterior[tmp, 2]
    RsupInic <- saidaAnterior[tmp, 3]
    Rsup2Inic <- saidaAnterior[tmp, 4]
    RsubInic <- saidaAnterior[tmp, 5]
  }

  #Calculo das funções de transferencia ----
  #Eqs. Referentes ao Manual de Metodologia SMAP
  Tu <- RsoloInic / Str #Eq.19 Manual

  if (precipitacao > Ai){
    Es <- ((precipitacao - Ai) ^ 2) / (precipitacao - Ai + (Str - RsoloInic)) #Eq.11
  }else{
    Es <- 0
  }

  if((precipitacao - Es) > evapotranspiracao){
    Er <- evapotranspiracao
  }else{
    Er <- (precipitacao-Es) + (evapotranspiracao - (precipitacao - Es)) *Tu  #Eq.12
  }

  Capc_tmp <- (Capc / 100) * Str

  if(RsoloInic > Capc_tmp){
    Rec <- (Crec / 100) * Tu * (RsoloInic - Capc_tmp) #Eq.13
  }else{
    Rec <- 0
  }

  #4º Reservatorio
  if (RsupInic > H){
    Marg <- (RsupInic - H) * (1 - K_1ts) #Eq.14
  }else{
    Marg <- 0
  }

  Ed <- min(c(RsupInic - Marg, H1)) * (1 - K_2ts) #Eq.15

  Ed2 <- Rsup2Inic * (1 - K_3ts) #Eq.17

  Ed3 <- max(c(RsupInic - H1 - Marg, 0)) * (1 - K_2t2s) #Eq.16

  Eb <- RsubInic * (1 - K_kts) #Eq.18

  #Calculo das variaveis de estado ----
  Rsolo <- min(c((RsoloInic + precipitacao - Es - Er - Rec), Str))
  Rsub <- RsubInic + Rec - Eb
  Rsup_tmp <- ((RsoloInic + precipitacao - Es - Er - Rec) - Str)
  Rsup <- RsupInic + Es - Marg - Ed - Ed3 + max(c(0, Rsup_tmp))
  Rsup2 <- max(Rsup2Inic + Marg - Ed2 - Emarg, 0)

  #Calculo da vazão ----
  Qcalc <- (Ed + Ed2 + Ed3 + Eb) * area / 86.4

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