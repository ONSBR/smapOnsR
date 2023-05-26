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
#' A data table com 239,742 linhas and 4 colunas:
#' \describe{
#'   \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{prec}{valor de vazao observada}
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
#'     \item{prec}{valor de evapotranspiracao potencial observada}
#'  }
#' }
#' @source ONS
"historico_etp"

#' Parametros
#'
#' Parametros das sub-bacias modeladas pelo SMAP/ONS
#'
#' @format ## `parametros`
#' A data table com 110 linha and 83 colunas:
#' \describe{
#'   \itemize{
#'     \item{nome}{nome da sub-bacia}
#'     \item{area}{area da sub-bacia}
#'     \item{nKt}{número de kts}
#'     \item{kt2}{valor do parametro kt+2}
#'     \item{kt-60}{valor do parametro kt-60}
#'      }
#' }
#' @source ONS
"parametros"