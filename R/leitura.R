# LEITURA DO DADO VERIFICADO --------------------------------------------------------

#' le_historico_verificado
#' 
#' Le arquivo de dados verificados, espera uma certa estrutura.
#' 
#' Esta funcao espera um arquivo da forma
#' 
#' | data | POSTO1 | POSTO2 | ... | POSTON |
#' | --- | --- | --- | --- | --- |
#' | 01/01/2020 | XXX | XXX | ... | XXX |
#' | 02/01/2020 | XXX | XXX | ... | XXX |
#' | 02/01/2020 | XXX | XXX | ... | XXX |
#' 
#' O nome da primeira coluna e irrelevante, pois sera trocado para "data" internamente. 
#' Os demais serao utilizados como identificadores de cada registro
#' 
#' @param arq o arquivo a ser lido
#' @importFrom  data.table fread melt setorder
#' @importFrom stats complete.cases
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da precipitacao observada}
#'     }
#' @export 

le_historico_verificado <- function(arq) {

    dat <- data.table::fread(arq)
    dat <- dat[stats::complete.cases(dat[, -1])]
    dat[, data := as.Date(data, format = "%d/%m/%Y")]
    
    dat <- data.table::melt(dat, id.vars = "data", variable.name = "posto",
           value.name = "valor")
    dat[, id := match(dat$posto, unique(dat$posto))]
    colnames(dat)[1] <- "data"

    data.table::setorder(dat, posto, data)
    data.table::setcolorder(dat, c("data", "posto", "id", "valor"))
    dat[, posto := tolower(posto)]
    return(dat)
}

# LEITURA DE ARQUIVOS DE ENTRADA SMAP---------------------------------

#' le_parametros
#' 
#' Leitor de arquivo de parametros
#' 
#' Le arquivo de parametros "sub-bacia_PARAMAETROS.txt".
#' 
#' @param arq o arquivo do tipo "sub-bacia_PARAMETROS.txt"
#' @importFrom  data.table data.table
#' @importFrom utils read.csv
#' @return data.table com as colunas
#'     \itemize{
#'     \item{Nome}{Nome da sub-bacia}
#'     \item{parametro}{nome do parametro}
#'     \item{valor}{valor do parametro}
#'     }
#' @export 
le_parametros <- function(arq) {
    parametros <- read.csv(arq, sep = "'", header = FALSE)

    parametros_smap <- array(rep(0, 82), c(1, 82))
    parametros_smap <- data.table::data.table(parametros_smap)
    colnames(parametros_smap) <- c("Nome", "Area", "nKt", paste0("Kt", 2:-60),
    "Str", "K2t", "Crec", "Ai", "Capc", "K_kt", "K2t2", "H1", "H", "K3t", "K1t",
    "Ecof", "Pcof", "Ecof2", "ktMin", "ktMax")
    
    parametros_smap$Nome <- tolower(sub(".*/", "", sub("_PARAMETROS.txt", "", arq)))
    
    parametros_smap$Area <- as.numeric(parametros[1, 1])
    parametros_smap$nKt <- as.numeric(substr(parametros[2,1], 1, 3))
    aux <- strsplit(trimws(substr(parametros[2,1],4,nchar(parametros[2,1]))),split = " ")

    for (ikt in 1:parametros_smap[, nKt]) {
        if (parametros_smap[1, nKt] > 3) {
            parametros_smap[1, (ikt + 3)] <- as.numeric(aux[[1]][(parametros_smap[1, nKt - ikt + 1])])
        } else{
            parametros_smap[1, (ikt + 4)] <- as.numeric(aux[[1]][(parametros_smap[1, nKt - ikt + 1])])
        }
    }
    for (iparametro in 67:80) {
        parametros_smap[1,iparametro] <- as.numeric(parametros[(iparametro - 64), 1])
    }
    parametros_smap[1, 81] <- sum(parametros_smap[, 7:66] > 0)
    parametros_smap[1, 82] <- sum(parametros_smap[, 4:5] > 0)

    parametros_smap
}

#' le_evapotranspiracao
#' 
#' Leitor de arquivo de normal climatologica de evapotranspiracao
#' 
#' Le arquivo evapotranspiracao utilizado no aplicativo SMAP/ONS
#' 
#' @param arq o arquivo do tipo "subbacia_EVAPOTRANSPIRACAO.txt"
#' @importFrom  data.table data.table
#' @return data.table evapotranspiracao com as colunas
#'     \itemize{
#'     \item{mes}{mes da NC}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da NC de evapotranspiracao observada}
#'     }
#' @export 
le_evapotranspiracao <- function(arq) {
    dat <- data.table::fread(arq)
    aux <- strsplit(arq, split = "/")[[1]]
    sb <- strsplit(aux[length(aux)], split = "_")[[1]]
    dat$posto <- tolower(sb[1])

    colnames(dat) <- c("mes", "valor", "posto")
    data.table::setcolorder(dat, c("mes", "posto", "valor"))

    dat
}


#' Le csv com dados de inicializacao do caso
#' 
#' Realiza a leitura do csv 'inicializacao.csv' com os dados iniciais
#' 
#' @param arq nome do arquivo "inicializacao.csv"
#' @importFrom  data.table data.table
#' @return data.table evapotranspiracao com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{Ebin}{vazao de base inicial}
#'     \item{Supin}{vazao superficial inicial}
#'     \item{Yuin}{umidade do solo inicial}
#'     }
#' @export 
le_inicializacao <- function(arq) {
    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("nome", "Ebin", "Supin", "Tuin"))) {
        stop("o arquivos deve possuir colunas 'nome', 'Ebin', 'Supin', 'Tuin'")
    }

    dat
}

#' Le csv com dados de datas de execucao do caso
#' 
#' Realiza a leitura do csv 'datasRodadas.csv' com as datas e o horizonte de previsao
#' 
#' @param arq nome do arquivo "inicializacao.csv"
#' @importFrom  data.table data.table
#' @return data.table evapotranspiracao com as colunas
#'     \itemize{
#'     \item{mes}{mes da NC}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da NC de evapotranspiracao observada}
#'     }
#' @export 
le_datas_rodada <- function(arq) {
    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("data", "numero_dias_previsao"))) {
        stop("o arquivos deve deve possuir colunas 'data' e 'numero_dias_previsao'")
    }

    dat[, datas := as.Date(datas)]

    dat
}

# FUNCOES DE COMPATIBILIZACAO DE DADOS---------------------------------

#' transforma_NC_serie
#' 
#' transforma normal climatologica de evapotranspiracao em uma serie temporal
#' 
#' 
#' @param serie_temporal serie temporal a ser utilizada como espelho
#' @param normal_climatologica data.table  com NC de evapotranspiracao com as colunas
#'     \itemize{
#'     \item{mes}{mes da NC}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da NC de evapotranspiracao observada}
#'     }
#' @importFrom data.table data.table
#' @importFrom lubridate month
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da serie temporal}
#'     }
#' @export 
transforma_NC_serie <- function(serie_temporal, normal_climatologica) {
    serie_temporal_etp <- data.table::data.table(serie_temporal)
    serie_temporal_etp[, mes := lubridate::month(data)]
    serie_temporal_etp <- merge(serie_temporal_etp, normal_climatologica, by = "mes")
    serie_temporal_etp[, posto.x := NULL]
    serie_temporal_etp[, id.x := NULL]
    serie_temporal_etp[, valor.x := NULL]
    serie_temporal_etp[, mes := NULL]

    colnames(serie_temporal_etp) <- c("data", "posto", "id", "valor")
    data.table::setorder(serie_temporal_etp, posto, data)
    serie_temporal_etp
}


#' Transforma historico de serie temporal em data table de previsao
#'
#' Transforma historico de serie temporal em data table de previsao
#'
#' @param serie_temporal data.table com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da serie_temporal observada}
#'     }
#' @param datas_rodadas data.table com as colunas
#'     \itemize{
#'     \item{data}{data da rodada}
#'     \item{numero_dias_previsao}{numero de dias de previsao}
#'      }
#' @importFrom data.table data.table setcolorder setorder
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data_rodada}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da previsao}
#'     }
#' @export
transforma_historico_previsao <- function(serie_temporal, datas_rodadas) {

    if (any(colnames(datas_rodadas) != c("data", "numero_dias_previsao"))) {
        stop("data table datas_rodadas deve possuir colunas 'data' e 'numero_dias_previsao'")
    }

    if (!inherits(datas_rodadas[, data], "Date")) {
        stop("data table datas_rodadas deve ter sua primeira coluna com classe 'Date'")
    }

    previsao <- data.table::data.table()

    for (i in 1:nrow(datas_rodadas)) {
        data_rodada <- datas_rodadas$data[i]
        numero_dias_previsao <- datas_rodadas$numero_dias_previsao[i]

        for (j in 1:(numero_dias_previsao + 2)){
            data_previsao <- serie_temporal[data == (data_rodada + j), data]
            aux <- serie_temporal[data %in% data_previsao, ]
            colnames(aux)[1] <- "data_previsao"
            aux[, data_rodada := data_rodada]
            aux[, cenario := "historico"]
            data.table::setcolorder(aux, c("data_rodada", "data_previsao", "cenario", "posto", "id", "valor") )

            previsao <- rbind(previsao, aux)
        }
    }

    data.table::setorder(previsao, data_rodada, id, data_previsao)
    return(previsao)

}