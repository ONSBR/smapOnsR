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
#' @return lista com o data.table inicializacao com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub-bacia}
#'     \item{variavel}{nome da variavel}
#'     \item{valor}{valor da variavel}
#'     }
#' e data table datas_rodadas com as colunas
#'     \itemize{
#'     \item{data}{data da rodada}
#'     \item{numero_dias_previsao}{numero de dias de previsão}
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

    inicializacao <- data.table::data.table(nome = nome_subbacia, Ebin = dat[4, V1], Supin = dat[5, V1], Tuin = dat[6, V1], numero_dias_assimilacao = (as.numeric(dat[2, V1]) - 1))
    inicializacao <- data.table::melt(inicializacao, id.vars = "nome", variable.name = "variavel",
           value.name = "valor")
    inicializacao[, valor := as.numeric(valor)]

    if (any(inicializacao[, valor] < 0)) {
        stop(paste0(" variavel ", inicializacao[valor < 0, variavel]," negativa para a sub-bacia ", nome_subbacia))
    }

    datas_rodadas <- data.table::data.table(data = dat[1, valor], numero_dias_previsao = dat[3, valor])

    saida <- list(inicializacao = inicializacao, datas_rodadas = datas_rodadas)
    saida
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

    if (any(vazao[, valor] < 0)) {
        stop(paste0(" vazao observada da data ", vazao[valor < 0, data]," negativa para a sub-bacia ", nome_subbacia))
    }

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
#'     \item{nome}{nome da sub_bacia}
#'     \item{posto}{nome do posto plu}
#'     \item{valor}{peso do posto plu}
#'     }
#' @export
le_entrada_posto_plu <- function(pasta_entrada, nome_subbacia) {

    if (missing("nome_subbacia")) {
        stop("forneca o nome da sub-bacia para a leitura do arquivo '_postos_plu.txt'")
    }

    arq <- file.path(pasta_entrada, paste0(nome_subbacia, "_postos_plu.txt"))

    if (!file.exists(arq)) {
        stop(paste0("nao existe o arquivo ", arq))
    }

    postos_plu <- data.table::fread(arq, header = FALSE)
    postos_plu[, nome := nome_subbacia]
    colnames(postos_plu)[1:2] <- c("posto", "valor")

    postos_plu <- data.table::setcolorder(postos_plu, c("nome", "posto", "valor"))
    postos_plu[, posto := tolower(posto)]
    postos_plu[substr(posto, 1, 1) == "0", posto := substr(posto, 2, 8)]

    postos_plu
}

#' Leitor de arquivo de precipitacao observada do smap/ons
#' 
#' Le arquivo "nome_posto_plu_c.txt" utilizado no aplicativo SMAP/ONS
#' 
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param posto_plus data.table postos_plu com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{posto}{nome do posto plu}
#'     \item{valor}{peso do posto plu}
#'     }
#' @importFrom  data.table fread setcolorder
#' @importFrom stats na.omit
#' @return data.table vazao com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da precipitacao observada}
#'     }
#' @export
le_entrada_precipitadao <- function(pasta_entrada, postos_plu) {

    for (iposto in seq_len(postos_plu)){
        arq <- file.path(pasta_entrada, paste0(postos_plu[iposto, posto], "_c.txt"))

        if (!file.exists(arq)) {
            arq <- file.path(pasta_entrada, paste0("0", postos_plu[iposto, posto], "_c.txt"))
            if (!file.exists(arq)) {
                stop(paste0("nao existe o arquivo ", arq))
            }
        }

        precipitacao <- data.table::fread(arq, header = FALSE)

        precipitacao[, V1 := NULL]
        precipitacao[, V3 := NULL]

        colnames(precipitacao) <- c("data", "valor")
        precipitacao[, data := as.Date(data, format = "%d/%m/%Y")]
        precipitacao[, valor := as.numeric(valor)]
        
        precipitacao <- stats::na.omit(precipitacao)

        precipitacao[, posto := postos_plu[iposto, posto]]
        precipitacao <- data.table::setcolorder(precipitacao, c("data", "posto", "valor"))

        if (any(precipitacao[, valor] < 0)) {
            stop(paste0(" precipitacao observada da data ", precipitacao[valor < 0, data]," negativa para a sub-bacia ", nome_subbacia))
        }
    }

    precipitacao
}

#' Leitor de arquivo de modelos de previsao de precipitacao do smap/ons
#' 
#' Le arquivo "modelos_precipitacao.txt" utilizado no aplicativo SMAP/ONS
#' 
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @importFrom  data.table fread
#' @return modelos_precipitacao lista contendo os seguintes parametros
#'     \itemize{
#'     \item{numero_cenario}{numero de cenarios do caso}
#'     \item{nome_cenario}{data table contendo os seguintes parametros}
#'     \itemize{
#'     \item{primeira parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{nome_cenario_2}{segunda parte do nome dos cenarios de precipitacao considerados o caso}
#'      }
#'     }
#' @export
le_entrada_modelos_precipitacao <- function(pasta_entrada) {

    if (missing("pasta_entrada")) stop("forneca o caminho da pasta 'arq_entrada' a leitura do caso")

    arq <- file.path(pasta_entrada, "modelos_precipitacao.txt")

    if (!file.exists(arq)) {
        stop("nao existe o arquivo do tipo modelos_precipitacao.txt")
    }

    dat <- data.table::fread(arq)
    numero_cenarios <- as.numeric(dat[1])

    nome_cenario <- ""
    for (icenario in 2:nrow(dat)){
        nome_cenario[icenario - 1] <- tolower(as.character(dat[icenario]))
    }

    nome_cenario_1 <- unlist(strsplit(nome_cenario, split = "_"))[2 * (1:numero_cenarios) - 1]
    nome_cenario_2 <- unlist(strsplit(nome_cenario, split = "_"))[2 * (1:numero_cenarios)]
    nome_cenario <- data.table::data.table(nome_cenario_1 = nome_cenario_1, nome_cenario_2 = nome_cenario_2)
    modelos_precipitacao <- list(numero_cenarios = numero_cenarios, nome_cenario = nome_cenario)
    
    modelos_precipitacao
}

#' Leitor de arquivo de com os pontos de grade de previsao da sub_bacia
#' 
#' Le arquivo "sub-bacia_nome_cenario.txt" utilizado no aplicativo SMAP/ONS
#' 
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param nome_subbacia nome da sub-bacia
#' @param modelos_precipitacao lista contendo os seguintes parametros
#'     \itemize{
#'     \item{nome_cenario_1}{primeira parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{nome_cenario_2}{segunda parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{numero_cenario}{numero de cenarios do caso}
#'     }
#' @importFrom  data.table fread setcolorder data.table
#' @return data.table pontos_grade com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{nome_cenario_1}{primeira parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{nome_cenario_2}{segunda parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{latitude}{latitude do ponto de grade do cenário}
#'     \item{longitude}{longitude do ponto de grade do cenário}
#'     }
#' @export
le_entrada_pontos_grade <- function(pasta_entrada, nome_subbacia, modelos_precipitacao) {

    if (missing("nome_subbacia")) {
        stop("forneca o nome da sub-bacia para a leitura do arquivo de pontos de grade")
    }

    pontos_grade <- data.table::data.table()
    for (cenario in unique(modelos_precipitacao$nome_cenario[, nome_cenario_1])){
        arq <- file.path(pasta_entrada, paste0(nome_subbacia, "_", cenario, ".txt"))

        if (!file.exists(arq)) {
            stop(paste0("nao existe o arquivo ", arq))
        }

        aux <- data.table::fread(arq)
        aux_pontos_grade <- modelos_precipitacao$nome_cenario
        aux_pontos_grade[, latitude := aux[1, "1"]]
        aux_pontos_grade[, longitude := aux[1, V1]]
        aux_pontos_grade[, nome := nome_subbacia]
        
        if (colnames(aux)[2] > 1) {
            for (iponto in 2:colnames(aux)[2]) {
                aux_pontos_grade2 <- modelos_precipitacao$nome_cenario
                aux_pontos_grade2[, latitude := aux[iponto, "1"]]
                aux_pontos_grade2[, longitude := aux[iponto, V1]]
                aux_pontos_grade2[, nome := nome_subbacia]
                rbind(aux_pontos_grade, aux_pontos_grade2)
            }
        }
        pontos_grade <- rbind(pontos_grade, aux_pontos_grade)
    }
    pontos_grade <- data.table::setcolorder(pontos_grade, c("nome", "nome_cenario_1", "nome_cenario_2", "latitude", "longitude"))
    
    pontos_grade
}

#' Le arquivo bat.conf
#'
#' Leitor do arquivo de entrada bat.txt contendo para receber qual padrão de arquivo de previsao está 
#' @param pasta_entrada o arquivo do tipo "caso.txt"
#' @importFrom  data.table fread
#' @return formato_previsao numero do tipo de arquivo de previsao
#' @export
le_entrada_bat_conf <- function(pasta_entrada) {

    if (missing("pasta_entrada")) stop("forneca o caminho da pasta 'arq_entrada' a leitura do caso")

    arq <- file.path(pasta_entrada, "bat.conf")

    if (!file.exists(arq)) {
        stop("nao existe o arquivo do tipo bat.conf")
    }
    
    dat <- data.table::fread(arq, sep = "=")
    
    if (length(dat[V1 == "arqPrev", V1]) == 0) {
        formato_previsao <- 0
    } else {
        formato_previsao <- dat[V1 == "arqPrev", V1]
    }

    formato_previsao
}

#' Leitor de arquivo de previsao de precipitacao do smap/ons
#' 
#' Le arquivo "'nome_cenario'_p_'data_caso'_'data_previsao'.txt" utilizado no aplicativo SMAP/ONS
#' 
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param posto_plus data.table postos_plu com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{posto}{nome do posto plu}
#'     \item{valor}{peso do posto plu}
#'     }
#' @importFrom  data.table fread setcolorder
#' @importFrom stats na.omit
#' @return data.table vazao com as colunas
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da precipitacao observada}
#'     }
#' @export
le_entrada_previsao_precipitacao_0 <- function(pasta_entrada, data_caso, data_previsao,
 pontos_grade) {

    arq <- file.path(pasta_entrada, paste0(postos_plu[iposto, posto], "_c.txt"))

    if (!file.exists(arq)) {
        arq <- file.path(pasta_entrada, paste0("0", postos_plu[iposto, posto], "_c.txt"))
        if (!file.exists(arq)) {
            stop(paste0("nao existe o arquivo ", arq))
        }
    }

    precipitacao <- data.table::fread(arq, header = FALSE)

    precipitacao[, V1 := NULL]
    precipitacao[, V3 := NULL]

    colnames(precipitacao) <- c("data", "valor")
    precipitacao[, data := as.Date(data, format = "%d/%m/%Y")]
    precipitacao[, valor := as.numeric(valor)]
    
    precipitacao <- stats::na.omit(precipitacao)

    precipitacao[, posto := postos_plu[iposto, posto]]
    precipitacao <- data.table::setcolorder(precipitacao, c("data", "posto", "valor"))

    if (any(precipitacao[, valor] < 0)) {
        stop(paste0(" precipitacao observada da data ", precipitacao[valor < 0, data]," negativa para a sub-bacia ", nome_subbacia))
    }
}
