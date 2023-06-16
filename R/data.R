#' Historico de PSAT
#'
#' Historico de precipitação das sub-bacias modeladas pelo SMAP/ONS
#' para o periodo de 2010 a 2022
#'
#' @format ## `historico_precipitacao`
#' A data table com 552,258 linhas and 4 colunas:
#' \describe{
#'   \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor de precipitacao observada}
#'  }
#' }
#' @source ONS
"historico_precipitacao"

#' Historico de vazao
#'
#' Historico de vazao das sub-bacias modeladas pelo SMAP/ONS
#' para o periodo de 2010 a 2021
#'
#' @format ## `historico_vazao`
#' A data table com 486,513 linhas and 4 colunas:
#' \describe{
#'   \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor de vazao observada}
#'  }
#' }
#' @source ONS
"historico_vazao"

#' Historico de etp
#'
#' Historico de evapotranspiracao potencial proveniente do era 5 land
#' para o periodo de 1991 a 2022
#'
#' @format ## `historico_etp`
#' A data table com 163,632 linhas and 4 colunas:
#' \describe{
#'   \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor de evapotranspiracao potencial observada}
#'  }
#' }
#' @source ONS
"historico_etp"

#' Parametros
#'
#' Parametros das sub-bacias modeladas pelo SMAP/ONS
#'
#' @format ## `parametros`
#' A data table com 8910 linhas and 3 colunas:
#' \describe{
#'   \itemize{
#'     \item{nome}{nome da sub-bacia}
#'     \item{parametros}{nome do parametros}
#'     \item{valor}{valor do parametro}
#'     }
#' }
#' @source ONS
"parametros"

#' Historico de Normais Climatologicas de etp
#'
#' Historico de Normais Climatologicas de de evapotranspiracao potencial 
#' utilizados oficialmente no SMAP/ONS
#'
#' @format ## `historico_etp`
#' Data table com 1,320 linhas and 4 colunas:
#' \describe{
#'   \itemize{
#'     \item{mes}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor de evapotranspiracao potencial observada}
#'  }
#' }
#' @source ONS
"historico_etp_NC"


#' Postos plu
#' 
#' Relacao de posto plu por sub-bacia
#' 
#' @format ## 'postos_plu'
#' data table com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{posto}{nome do posto plu}
#'     \item{valor}{peso do posto plu}
#'     }
#' @source ONS
"postos_plu"