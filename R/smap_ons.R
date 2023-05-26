#----------------------------------#
### FUNÇÕES DE RODADA DO MODELO ####
#----------------------------------#

#' Rodada de um dia do SMAP
#'
#' @param parametrosSMAP data.table contendo n linhas and 83 colunas:
#' \describe{
#'   \itemize{
#'     \item{nome}{nome da sub-bacia}
#'     \item{area}{area da sub-bacia}
#'     \item{nKt}{número de kts}
#'     \item{kt2}{valor do parametro kt+2}
#'     \item{kt-60}{valor do parametro kt-60}
#'    }
#' }
#' @param inicializacao  Objeto resultante da funcao smap.inicializacao
#' @param precipitacao Valor de precipitacao final do dia (ja corrigido e/ou ponderado)
#' @param evapotranspiracao Valor de ETo do final dia (ja corrigido e/ou ponderado)
#' @param Emarg Valor da evaporacao final do dia do reservatorio de planicie (ja corrigido e/ou ponderado)
#' @param mRbind Matriz com resultado da iteracao passada (primeira iteracao nao atribuir nenhum valor)
#' @param dataD Nomea a linha da matriz de saida com a data fornecida
#'
#' @return Matriz com a vazao calculada e os valores de estado e funcoes de transferencia
#' @export

smap_ons.previsao <- function(parametrosSMAP, inicializacao, precipitacao, evapotranspiracao, Emarg, mRbind, dataD = NULL){

  rb_miss <- TRUE

  #Param. Gerais SMAP
  Str <- parametrosSMAP[1]
  K2t <- parametrosSMAP[2]
  Crec <- parametrosSMAP[3]
  Capc <- parametrosSMAP[4]
  K_kt <- parametrosSMAP[5]
  H1 <- parametrosSMAP[6]
  K2t2 <- parametrosSMAP[7]
  Area <- parametrosSMAP[, Area]
  Ai <- parametrosSMAP[, Ai]

  #Param. 4 Reserv
  H <- parametrosSMAP[8]
  K1t <- parametrosSMAP[9]
  K3t <- parametrosSMAP[10]

  #Param inicializacao
  EbInic <- inicializacao$EbInic
  TuInic <- inicializacao$TuInic
  Supin <- inicializacao$Supin

  #Coeficientes Ks
  K_kts <- 0.5^(1/K_kt)
  K_1ts <- 0.5^(1/K1t)
  K_2ts <- 0.5^(1/K2t)
  K_2t2s <- 0.5^(1/K2t2)
  K_3ts <- 0.5^(1/K3t)
  

  #Se nao for fornecido nenhuma matriz no mRBind calcular os valores iniciais de Rsolo e Rsub
  if(missing(mRbind)){
    RsoloInic <- Str*TuInic
    RsupInic <- (Supin/(1-K_2ts))*86.4/Area
    Rsup2Inic <- inicializacao$Rsup2
    RsubInic <- EbInic/(1-K_kts)*86.4/Area
  }else{
    rb_miss = FALSE
    tmp <- nrow(mRbind)
    RsoloInic <- mRbind[tmp,"RSolo"]
    RsupInic <- mRbind[tmp, "Rsup"]
    Rsup2Inic <- mRbind[tmp, "Rsup2"]
    RsubInic <- mRbind[tmp, "Rsub"]
  }

  #Calculo das funções de transferencia ----
  #Eqs. Referentes ao Manual de Metodologia SMAP
  Tu <- RsoloInic/Str #Eq.19 Manual

  if(precipitacao>Ai){
    Es = ((precipitacao-Ai)^2)/(precipitacao-Ai+(Str-RsoloInic)) #Eq.11
  }else{
    Es = 0
  }

  if((precipitacao-Es)>evapotranspiracao){
    Er = evapotranspiracao
  }else{
    Er = (precipitacao-Es) + ( evapotranspiracao - ( precipitacao-Es ) )*Tu  #Eq.12
  }

  Capc_tmp <- (Capc/100)*Str

  if(RsoloInic > Capc_tmp ){
    Rec = ( Crec/100 )*Tu*( RsoloInic - Capc_tmp ) #Eq.13
  }else{
    Rec = 0
  }

  #4º Reservatorio
  if(RsupInic > H){
    Marg = (RsupInic - H)*(1-K_1ts) #Eq.14
  }else{
    Marg = 0
  }

  Ed = min(c(RsupInic - Marg, H1) )*(1-K_2ts) #Eq.15

  Ed2 = Rsup2Inic * (1 - K_3ts) #Eq.17

  Ed3 = max(c(RsupInic - H1 - Marg, 0) )*(1-K_2t2s) #Eq.16

  Eb = RsubInic * (1 - K_kts) #Eq.18

  #Calculo das variaveis de estado ----
  Rsolo = min(c((RsoloInic + precipitacao - Es - Er - Rec), Str))
  Rsub = RsubInic + Rec - Eb
  Rsup_tmp = ((RsoloInic + precipitacao - Es - Er - Rec) - Str)
  Rsup = RsupInic + Es - Marg - Ed - Ed3 + max(c(0,Rsup_tmp))
  Rsup2 = max(Rsup2Inic + Marg - Ed2 - Emarg,0)

  #Calculo da vazão ----
  Qcalc = (Ed + Ed2 + Ed3 + Eb) * Area / 86.4

  #Matriz de Saida ----
  matrizSaida_nom <- c("Qcalc", "RSolo", "Rsup", "Rsup2", "Rsub",
                       "Es", "Er", "Rec", "Marg", "Ed", "Ed2", "Ed3",
                       "Eb", "Tu")
  matrizSaida <- matrix(nrow = 1, ncol = length(matrizSaida_nom))
  matrizSaida[1,1]<-Qcalc
  matrizSaida[1,2]<-Rsolo
  matrizSaida[1,3]<-Rsup
  matrizSaida[1,4]<-Rsup2
  matrizSaida[1,5]<-Rsub
  matrizSaida[1,6]<-Es
  matrizSaida[1,7]<-Er
  matrizSaida[1,8]<-Rec
  matrizSaida[1,9]<-Marg
  matrizSaida[1,10]<-Ed
  matrizSaida[1,11]<-Ed2
  matrizSaida[1,12]<-Ed3
  matrizSaida[1,13]<-Eb
  matrizSaida[1,14]<-Tu

  colnames(matrizSaida) <- matrizSaida_nom

  if(!is.null(dataD)){
    row.names(matrizSaida)<-as.character(dataD)
  }

  if(rb_miss){
    return(matrizSaida)
  }else{
    m_f <- rbind(mRbind, matrizSaida)
    return(m_f)
  }
}