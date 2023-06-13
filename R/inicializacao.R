#' Funcao de calculo dos valores inicias
#' 
#' Realiza o calculo dos Valores iniciais para rodada do SMAP/ONS
#'
#' Funcao para criar objeto com os valores iniciais de variaveis de estado para rodar o SMAP
#'
#' @param modelo vetor resultante unlist do objeto de classe smap_ons
#' @param area area da sub-bacia
#' @param Supin Escoamento superficial inicial
#' @param Rsup2Inic Escoamento superficial inicial referente ao reservatorio de planicie
#' @param EbInic Escoamento Subterraneo inicial
#' @param TuInic Taxa de umidade inicial do solo
#'
#' @return List com os parametros:
#'     \itemize{
#'     \item{EbInic}{vazao de base inicial}
#'     \item{TuInic}{percentual de umidade do solo inicial}
#'     \item{Supin}{vazao superficial inicial}
#'     \item{Rsup2Inic}{valor do reservatorio de planicie inicial}
#'     \item{RsoloInic}{valor do reservatorio de solo inicial}
#'     \item{RsupInic}{valor do reservatorio superficial inicial}
#'     \item{RsubInic}{valor do reservatorio subterraneo inicial}
#'     }
#' @export
inicializacao_smap <- function(modelo, area, EbInic = 0, TuInic = 0.3, Supin = 100){
  if(Supin < 0){
    stop("Supin deve ser ser positivo")
  }
  
  if((TuInic < 0) | (TuInic > 1)){
    stop("TuInic deve estar entre 0 e 1")
  }
  
  if(EbInic < 0){
    stop("EbInic deve ser positivo")
  }

  RsoloInic <- modelo[1] * TuInic
  RsupInic <- (Supin / (1 - (0.5 ^ (1 / modelo[2])))) * 86.4 / area
  RsubInic <- EbInic / (1 - (0.5 ^ (1 / modelo[5]))) * 86.4 / area
  Rsup2Inic <- 0
  inic <- list(EbInic, TuInic, Supin, Rsup2Inic, RsoloInic, RsupInic, RsubInic)
  names(inic) <- c("EbInic", "TuInic", "Supin", "Rsup2Inic", "RsoloInic", "RsupInic", "RsubInic")
  
  return(inic)
}