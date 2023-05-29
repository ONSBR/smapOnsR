# LEITURA DO DADO VERIFICADO --------------------------------------------------------

#' Leitor De Arquivo De Dados Verificados
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
    return(dat)
}

# LEITURA DE ARQUIVOS DE ENTRADA SMAP

#' Leitor de arquivo de parametros
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
#' @param arq o arquivo do tipo "subbacia_PARAMETROS.txt"
#' @importFrom  data.table data.table
#' @importFrom stats complete.cases
#' @importFrom utils read.csv
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da precipitacao observada}
#'     }
le_parametros <- function(arq) {
    parametros <- read.csv(arq, sep = "'", header = FALSE)

    parametros_smap <- array(rep(0, 87), c(1, 87))
    parametros_smap <- data.table::data.table(parametros_smap)
    colnames(parametros_smap) <- c("Nome", "Area", "nKt", paste0("Kt", 2:-60),
    "Str", "K2t", "Crec", "Ai", "Capc", "K_kt", "K2t2", "H1", "H", "K3t", "K1t",
    "Ecof", "Pcof", "Ecof2", "ktMin", "ktMax", "K_kts", "K_1ts", "K_2ts", "K_2t2s", "K_3ts")
    
    aux <- strsplit(arq, split = "/")[[1]]
    sb <- strsplit(aux[length(aux)], split = "_")[[1]]
    parametros_smap$Nome <- sb[1]
    
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
    parametros_smap$K_kts = 0.5 ^ (1 / K_kt)
    parametros_smap$K_1ts = 0.5 ^ (1 / K1t)
    parametros_smap$K_2ts = 0.5 ^ (1 / K2t)
    parametros_smap$K_2t2s = 0.5 ^ (1 / K2t2)
    parametros_smap$K_3ts = 0.5 ^ (1 / K3t)

    parametros_smap
}
