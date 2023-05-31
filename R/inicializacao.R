#' Valores iniciais SMAP
#'
#' Funcao para criar objeto com os valores iniciais de variaveis de estado para rodar o SMAP
#'
#' @param Supin Escoamento superficial inicial
#' @param Rsup2Inic Escoamento superficial inicial referente ao reservatorio de planicie
#' @param EbInic Escoamento Subterraneo inicial
#' @param TuInic Taxa de umidade inicial do solo
#'
#' @return List com os parametros
#' @export
smap_ons.inic <- function(parametros, EbInic = 0, TuInic = 0.3, Supin = 100, Rsup2Inic = 0){
  if(Supin<0){
    stop("Supin deve ser >= 0")
  }
  
  if((TuInic < 0) | (TuInic >1)){
    stop("TuInic deve estar entre 0 e 1")
  }
  
  if(Rsup2Inic < 0){
    stop("Rsup2Inic deve ser positivo")
  }
  
  RsoloInic <- parametros[parametro == "Str", valor] * TuInic
  RsupInic <- (Supin / (1 - parametros[parametro == "K_2ts", valor])) * 86.4 / parametros[parametro == "Area", valor]
  RsubInic <- EbInic / (1 - parametros[parametro == "K_kts", valor]) * 86.4 / parametros[parametro == "Area", valor]

  inic <- list(EbInic, TuInic, Supin, Rsup2Inic, RsoloInic, RsupInic, RsubInic)
  names(inic) <- c("EbInic", "TuInic", "Supin", "Rsup2Inic", "RsoloInic", "RsupInic", "RsubInic")
  
  return(inic)
}