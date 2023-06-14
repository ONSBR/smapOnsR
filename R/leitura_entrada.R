# LEITURA DE ARQUIVOS DE ENTRADA SMAP---------------------------------

#' le_parametros
#' 
#' Leitor de arquivo de parametros
#' 
#' Le arquivo de parametros "sub-bacia_PARAMETROS.txt".
#' 
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param nome_subbacia nome da sub-bacia
#' @importFrom utils read.csv
#' @return lista contendo os parametros da sub-bacia
#' @export 
le_entrada_parametros <- function(pasta_entrada, nome_subbacia) {

    if (missing("nome_subbacia")) stop("forneca o nome da sub-bacia para a leitura do arquivo 'sub-bacia_parametros.txt'")

    arq <- file.path(pasta_entrada, paste0(nome_subbacia, "_parametros.txt"))

    if (!file.exists(arq)) {
        stop(paste0("nao existe o arquivo ", arq))
    }

    parametros <- read.csv(arq, sep = "'", header = FALSE)

    parametros_smap <- array(rep(0, 82), c(1, 82))
    parametros_smap <- data.table::data.table(parametros_smap)
    colnames(parametros_smap) <- c("Nome", "Area", "nKt", paste0("Kt", 2:-60),
    "Str", "K2t", "Crec", "Ai", "Capc", "K_kt", "K2t2", "H1", "H", "K3t", "K1t",
    "Ecof", "Pcof", "Ecof2", "ktMin", "ktMax")
    
    aux <- strsplit(arq, split = "/")[[1]]
    sb <- strsplit(aux[length(aux)], split = "_")[[1]]
    parametros_smap$Nome <- tolower(sb[1])
    
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
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param nome_subbacia nome da sub-bacia
#' @importFrom  data.table fread setcolorder
#' @return data.table evapotranspiracao com as colunas
#'     \itemize{
#'     \item{mes}{mes da NC}
#'     \item{posto}{nome do posto}
#'     \item{id}{id do posto}
#'     \item{valor}{valor da NC de evapotranspiracao observada}
#'     }
#' @export 
le_entrada_evapotranspiracao <- function(pasta_entrada, nome_subbacia) {

    if (missing("nome_subbacia")) {
        stop("forneca o nome da sub-bacia para a leitura do arquivo 'sub-bacia_EVAPOTRANSPIRACAO.txt'")
    }

    arq <- file.path(pasta_entrada, paste0(nome_subbacia, "_evapotranspiracao.txt"))

    if (!file.exists(arq)) {
        stop(paste0("nao existe o arquivo ", arq))
    }

    dat <- data.table::fread(arq)
    dat$posto <- tolower(nome_subbacia)

    colnames(dat) <- c("mes", "valor", "posto")
    data.table::setcolorder(dat, c("mes", "posto", "valor"))

    dat
}

#' Le arquivo caso.txt
#'
#' Leitor do arquivo de entrada caso.txt contendo quais sub-bacias constam no caso
#' @param pasta_entrada o arquivo do tipo "caso.txt"
#' @importFrom  data.table fread
#' @return caso lista contendo os seguintes parametros
#'     \itemize{
#'     \item{numero_subbacias}{numero de sub-bacias do caso}
#'     \item{nome_subbacia}{vetor com o nome das sub-bacias}
#'     }
#' @export
le_entrada_caso <- function(pasta_entrada) {

    if (missing("pasta_entrada")) stop("forneca o caminho da pasta 'arq_entrada' a leitura do caso")

    arq <- file.path(pasta_entrada, "caso.txt")

    if (!file.exists(arq)) {
        stop("nao existe o arquivo do tipo caso.txt")
    }
    
    dat <- data.table::fread(arq)
    numero_subbacias <- as.numeric(dat[1])
    
    nome_subbacia <- ""
    for (ibacia in 2: nrow(dat)){
        nome_subbacia[ibacia - 1] <- tolower(as.character(dat[ibacia]))
    }

    caso <- list(numero_subbacias = numero_subbacias, nome_subbacia = nome_subbacia)
    caso
}

#' Leitor de arquivo de inicializacao do smap
#' 
#' Le arquivo "sub-bacia_inicializacao.txt" utilizado no aplicativo SMAP/ONS
#' 
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param nome_subbacia nome da sub-bacia
#' @importFrom  data.table fread setcolorder
#' @return data.table entrada_inicializacao com as colunas
#'     \itemize{
#'     \item{valor}{valor do parametro}
#'     \item{parametro}{nome do parametros}
#'     }
#' @export
le_entrada_inicializacao <- function(pasta_entrada, nome_subbacia) {

    if (missing("nome_subbacia")) {
        stop("forneca o nome da sub-bacia para a leitura do arquivo 'sub-bacia_inicializacao.txt'")
    }

    arq <- file.path(pasta_entrada, paste0(nome_subbacia, "_inicializacao.txt"))

    if (!file.exists(arq)) {
        stop(paste0("nao existe o arquivo ", arq))
    }

    dat <- data.table::fread(arq, sep = "'", header = FALSE)

    colnames(dat) <- c("valor", "parametro")

    data.table::setcolorder(dat, c("parametro", "valor"))
    dat$parametro[1] <- "data_rodada"
    dat$parametro[2] <- "numero_dias_assimilacao"
    dat$parametro[3] <- "numero_dias_previsao"

    dat
}

#' Leitor de arquivo de vazao observada do smap/ons
#' 
#' Le arquivo "sub-bacia_inicializacao.txt" utilizado no aplicativo SMAP/ONS
#' 
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param nome_subbacia nome da sub-bacia
#' @importFrom  data.table fread setcolorder
#' @importFrom stats na.omit
#' @return data.table vazao com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da precipitacao observada}
#'     }
#' @export
le_entrada_vazao <- function(pasta_entrada, nome_subbacia) {

    if (missing("nome_subbacia")) {
        stop("forneca o nome da sub-bacia para a leitura do arquivo 'sub-bacia.txt'")
    }

    arq <- file.path(pasta_entrada, paste0(nome_subbacia, ".txt"))

    if (!file.exists(arq)) {
        stop(paste0("nao existe o arquivo ", arq))
    }

    vazao <- data.table::fread(arq, header = FALSE)

    vazao[, V1 := NULL]
    vazao[, V2 := NULL]
    vazao[, V3 := NULL]
    vazao[, V4 := NULL]

    colnames(vazao) <- c("data", "valor")
    vazao[, data := as.Date(data)]
    vazao[, valor := as.numeric(valor)]
    
    vazao <- stats::na.omit(vazao)

    vazao[, posto := nome_subbacia]
    vazao <- data.table::setcolorder(vazao, c("data", "posto", "valor"))

    vazao
}

#' Leitor de arquivo de com os postos plu da sub_bacia
#' 
#' Le arquivo "sub-bacia_postos_plu.txt" utilizado no aplicativo SMAP/ONS
#' 
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param nome_subbacia nome da sub-bacia
#' @importFrom  data.table fread setcolorder
#' @return data.table postos_plu com as colunas
#'     \itemize{
#'     \item{psat}{nome do psat}
#'     \item{valor}{peso do psat}
#'     }
#' @export
le_entrada_posto_plu <- function(pasta_entrada, nome_subbacia) {

    if (missing("nome_subbacia")) {
        stop("forneca o nome da sub-bacia para a leitura do arquivo 'sub-bacia.txt'")
    }

    arq <- file.path(pasta_entrada, paste0(nome_subbacia, "_postos_plu.txt"))

    if (!file.exists(arq)) {
        stop(paste0("nao existe o arquivo ", arq))
    }

    postos_plu <- data.table::fread(arq, header = FALSE)
    postos_plu[, posto := nome_subbacia]
    colnames(postos_plu)[1:2] <- c("psat", "valor")
    
    postos_plu <- data.table::setcolorder(postos_plu, c("posto", "psat", "valor"))
    postos_plu[substr(psat, 1, 1) == "0", psat := substr(psat, 2, 8)]

    postos_plu
}