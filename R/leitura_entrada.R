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
    pattern <- paste0(nome_subbacia, "_parametros.txt")
    arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)
    arq <- file.path(pasta_entrada, arq)

    if (length(arq) == 0) {
        stop(paste0("nao existe o arquivo ", pattern))
    }

    parametros <- read.csv(arq, sep = "'", header = FALSE)

    parametros_smap <- array(rep(0, 82), c(1, 82))
    parametros_smap <- data.table::data.table(parametros_smap)
    colnames(parametros_smap) <- c("Nome", "Area", "nKt", paste0("Kt", 2:-60),
    "Str", "K2t", "Crec", "Ai", "Capc", "K_kt", "K2t2", "H1", "H", "K3t", "K1t",
    "Ecof", "Pcof", "Ecof2", "ktMin", "ktMax")
    
    aux <- strsplit(arq, split = "/")[[1]]
    parametros_smap$Nome <- tolower(sub(".*/", "", sub("_parametros.txt", "", nome_subbacia)))
    parametros_smap$Area <- as.numeric(parametros[1, 1])
    if (!is.character(parametros[2, 1])) stop(paste0("Nao existe valores de kt declarados no arquivo ", arq))
    aux <- unlist(strsplit(parametros[2, 1], "\\s+"))
    parametros_smap$nKt <- as.numeric(aux[1])
    if(as.numeric(aux[1]) != length(aux) - 1) stop(paste0("Numero de kt diferente do total de kt declarado no arquivo ", arq))    
    if (is.na(parametros_smap$nKt)) stop(paste0("Parametro de numero de kt com valor nao numerico no arquivo ", arq))    
    if (parametros_smap$nKt < 3) stop(paste0("Parametro de numero de kt com valor inferior a 3 no arquivo ", arq))    
    if (parametros_smap$nKt != trunc(parametros_smap$nKt)) stop(paste0("Parametro de numero de kt com valor decimal no arquivo ", arq))    

    for (ikt in 1:parametros_smap[, nKt]) {
        if (parametros_smap[1, nKt] > 3) {
            parametros_smap[1, (ikt + 3)] <- as.numeric(aux[parametros_smap$nKt - ikt + 2])
        } else {
            parametros_smap[1, (ikt + 4)] <- as.numeric(aux[parametros_smap$nKt - ikt + 2])
        }
    }

    for (iparametro in 67:80) {
        parametros_smap[1, iparametro] <- as.numeric(parametros[(iparametro - 64), 1])
    }

    parametros_smap[1, 81] <- sum(parametros_smap[, 7:66] > 0)
    parametros_smap[1, 82] <- sum(parametros_smap[, 4:5] > 0)
    if (sum(parametros_smap[, 4:66]) < 0.995) stop(paste0("Somatorio dos kts inferior a 0.995 no arquivo ", arq))
    if (sum(parametros_smap[, 4:66]) > 1.005) stop(paste0("Somatorio dos kts superior a 1.005 no arquivo ", arq))
    parametros_smap[, limite_superior_ebin := as.numeric(parametros[17, 1])]
    parametros_smap[, limite_inferior_ebin := as.numeric(parametros[18, 1])]
    parametros_smap[, limite_superior_prec := as.numeric(parametros[19, 1])]
    parametros_smap[, limite_inferior_prec := as.numeric(parametros[20, 1])]
    parametros_smap <- data.table::melt(parametros_smap, id.vars = "Nome", variable.name = "parametro",
           value.name = "valor")

    if (any(is.na(parametros_smap[, valor]))) stop(paste0("Parametro ", parametros_smap[is.na(valor), parametro], " com valor nao numerico no arquivo ", arq))
    if (any(parametros_smap[, valor] < 0)) stop(paste0("Parametro ", parametros_smap[valor < 0, parametro], " com valor negativo no arquivo ", arq))
    
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
#'     \item{valor}{valor da NC de evapotranspiracao observada}
#'     }
#' @export
le_entrada_evapotranspiracao <- function(pasta_entrada, nome_subbacia) {

    if (missing("nome_subbacia")) {
        stop("forneca o nome da sub-bacia para a leitura do arquivo 'sub-bacia_EVAPOTRANSPIRACAO.txt'")
    }

    pattern <- paste0(nome_subbacia, "_evapotranspiracao.txt")

    arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)

    arq <- file.path(pasta_entrada, arq)

    if (length(arq) == 0) {
        stop(paste0("nao existe o arquivo ", pattern))
    }

    dat <- data.table::fread(arq)

    if (ncol(dat) < 2) stop(paste0("O arquivo ", arq, " deve possuir 2 colunas"))

    dat$posto <- tolower(nome_subbacia)

    colnames(dat) <- c("mes", "valor", "nome")

    if (!is.numeric(dat[, mes])) stop(paste0("Valor nao numerico para mes no arquivo ", arq))

    if (!is.numeric(dat[, valor])) stop(paste0("Valor nao numerico para evapotranspiracao no arquivo ", arq))

    if ((any(dat[, mes] <= 0)) || (any(dat[, mes] > 12)) ) stop(paste0("Valor invalido para mes no arquivo ", arq))

    if (any(dat[, valor] <= 0) ) stop(paste0("Valor negativo para evapotranspiracao no arquivo ", arq))

    duplicados <- dat[duplicated(mes) | duplicated(mes, fromLast = TRUE), mes]

    if (length(duplicados) > 0) stop(paste0("O mes ", unique(duplicados), " esta duplicado no arquivo ", arq))

    data.table::setcolorder(dat, c("mes", "nome", "valor"))

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

    if (missing("pasta_entrada")) stop("forneca o caminho da pasta 'arq_entrada' para a leitura do caso")

    arquivo <- list.files(path = pasta_entrada, pattern = "caso.txt", ignore.case = TRUE)
    arquivo <- file.path(pasta_entrada, arquivo)
    if (!file.exists(arquivo)) stop("nao existe o arquivo do tipo caso.txt")
    
    dat <- data.table::fread(arquivo, sep = "'", header = FALSE)
    numero_subbacias <- as.numeric(dat[1, V1])

    if (is.na(numero_subbacias)) stop("Valor nao numerico de sub-bacias no arquivo caso.txt")
    
    if (numero_subbacias < 0) stop("Numero negativo de sub-bacias no arquivo caso.txt")

    if (numero_subbacias == 0) stop("Valor nulo de sub-bacias no arquivo caso.txt")

    dat <- na.omit(dat)

    if (numero_subbacias != (nrow(dat) - 1)) stop("Numero de sub-bacias diferente da quantidade declarada no arquivo caso.txt")
    
    duplicados <- dat[duplicated(V1) | duplicated(V1, fromLast = TRUE), V1]

    if (length(duplicados) > 0) stop(paste0("A sub-bacia ", unique(duplicados), " esta duplicada no arquivo caso.dat"))

    nome_subbacia <- ""
    for (ibacia in 2: nrow(dat)){
        nome_subbacia[ibacia - 1] <- tolower(as.character(dat[ibacia, V1]))
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
#'     \item{numero_dias_previsao}{numero de dias de previsao}
#'     }
#' @export
le_entrada_inicializacao <- function(pasta_entrada, nome_subbacia) {

    if (missing("nome_subbacia")) {
        stop("forneca o nome da sub-bacia para a leitura do arquivo 'sub-bacia_inicializacao.txt'")
    }

    pattern <- paste0(nome_subbacia, "_inicializacao.txt")

    arquivo <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)

    arq <- file.path(pasta_entrada, arquivo)

    if (length(arq) == 0) {
        stop(paste0("nao existe o arquivo ", pattern))
    }

    dat <- data.table::fread(arq, sep = "'", header = FALSE)

    inicializacao <- data.table::data.table(nome = nome_subbacia, Ebin = dat[4, V1], Supin = dat[5, V1], Tuin = dat[6, V1], numero_dias_assimilacao = (as.numeric(dat[2, V1]) - 1))
    inicializacao[, Ebin := as.numeric(Ebin)]
    inicializacao[, Supin := as.numeric(Supin)]
    inicializacao[, Tuin := as.numeric(Tuin)]
    inicializacao[, numero_dias_assimilacao := as.numeric(numero_dias_assimilacao)]
    inicializacao <- data.table::melt(inicializacao, id.vars = "nome", variable.name = "variavel",
           value.name = "valor")

    if (any(is.na(inicializacao[, valor] ))) {
        stop(paste0("variavel ", inicializacao[is.na(valor), variavel]," nao existente para a sub-bacia ", nome_subbacia))
    }

    if (inicializacao[variavel == "numero_dias_assimilacao", valor] <= 0) {
        stop(paste0("variavel Numero de dias de assimilacao menor que 2 para a sub-bacia ", nome_subbacia))
    }

    if (any(inicializacao[, valor] < 0)) {
        stop(paste0("variavel ", inicializacao[valor < 0, variavel]," negativa para a sub-bacia ", nome_subbacia))
    }

    if (inicializacao[variavel == "Tuin", valor] > 100) {
        stop(paste0("variavel Tuin maior que 100 para a sub-bacia ", nome_subbacia))
    }

    if (!grepl("^\\d{2}/\\d{2}/\\d{4}$", dat[1, V1])) {
        stop("Formato invalido de data para o arquivo ", arquivo)
    } else {
        datas_rodadas <- data.table::data.table(data = as.Date(dat[1, V1], format = "%d/%m/%Y"), numero_dias_previsao = as.numeric(dat[3, V1]))
        if (is.na(datas_rodadas$data)) stop("Formato invalido de data para o arquivo ", arquivo)
    }    

    if (any(is.na(datas_rodadas[, numero_dias_previsao] ))) {
        stop(paste0("Numero de dias de previsao nao existente para a sub-bacia ", nome_subbacia))
    }

    if (any(datas_rodadas[, numero_dias_previsao] <= 0)) {
        stop(paste0("Numero de dias de previsao menor ou igual a zero para a sub-bacia ", nome_subbacia))
    }

    saida <- list(inicializacao = inicializacao, datas_rodadas = datas_rodadas)
    saida
}

#' Leitor de arquivo de vazao observada do smap/ons
#' 
#' Le arquivo "sub-bacia.txt" utilizado no aplicativo SMAP/ONS
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

    pattern <- paste0(nome_subbacia, ".txt")
        
    arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)

    arq <- file.path(pasta_entrada, arq)

    if (length(arq) == 0) {
        stop(paste0("nao existe o arquivo ", pattern))
    }

    vazao <- data.table::fread(arq, header = FALSE)

    vazao[, V1 := NULL]
    vazao[, V2 := NULL]
    vazao[, V3 := NULL]
    vazao[, V4 := NULL]

    colnames(vazao) <- c("data", "valor")
    vazao[, data := as.Date(data)]
    vazao <- vazao[!grepl("-", valor)]
    vazao[, valor := as.numeric(valor)]

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

    pattern <- paste0(nome_subbacia, "_postos_plu.txt")

    arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)

    arq <- file.path(pasta_entrada, arq)

    if (length(arq) == 0) {
        stop(paste0("nao existe o arquivo ", pattern))
    }

    aux <- read.table(arq, header = FALSE, sep = ";")
    numero_postos <- as.numeric(aux$V1[1])
    
    if (is.na(numero_postos)) stop(paste0("Valor nao numerico de numero de postos plu no arquivo ", arq))
    
    if (numero_postos < 0) stop(paste0("Numero negativo de postos plu no arquivo ", arq))

    if (numero_postos == 0) stop(paste0("Valor nulo de postos plu no arquivo ", arq))

    postos_plu <- data.table::fread(arq, header = FALSE, blank.lines.skip = TRUE, sep = " ")
    
    if (ncol(postos_plu) != 2) stop(paste0("Arquivo ", arq, " com menos de 2 colunas"))

    if (numero_postos != nrow(postos_plu)) stop(paste0("Numero de postos plu diferente do declarado no arquivo ", arq))

    postos_plu[, nome := nome_subbacia]
    colnames(postos_plu)[1:2] <- c("posto", "valor")
    postos_plu[, valor := as.numeric(valor)]

    if (any(is.na(postos_plu[, valor]))) stop(paste0("Valor nao numerico de KE no arquivo ", arq))

    if (any(postos_plu[, valor] < 0)) stop(paste0("Valor negativo de KE no arquivo ", arq))

    if(postos_plu[, sum(valor)] > 1.005) stop(paste0("Somatorio do KE maior que 1.005 no arquivo ", arq))

    if(postos_plu[, sum(valor)] < 0.995) stop(paste0("Somatorio do KE menor que 0.995 no arquivo ", arq))

    if (any(nchar(postos_plu[, posto]) > 8)) stop(paste0("Nome do posto plu com mais de 8 caracteres no arquivo ", arq))

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
#' @param postos_plu data.table postos_plu com as colunas
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
le_entrada_precipitacao <- function(pasta_entrada, postos_plu) {

    precipitacao <- data.table::data.table()
    for (iposto in 1:nrow(postos_plu)){
        pattern <- paste0(postos_plu[iposto, posto], "_c.txt")
        arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)
        arq <- file.path(pasta_entrada, arq)
        if (length(arq) == 0) {
            pattern <- paste0("0", postos_plu[iposto, posto], "_c.txt")
            arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)
            arq <- file.path(pasta_entrada, arq)
            if (length(arq) == 0) {
                stop(paste0("nao existe o arquivo ", pattern))
            }
        }

        precipitacao_aux <- data.table::fread(arq, header = FALSE)

        precipitacao_aux[, V1 := NULL]
        precipitacao_aux[, V3 := NULL]

        colnames(precipitacao_aux) <- c("data", "valor")
        precipitacao_aux[, data := as.Date(data, format = "%d/%m/%Y")]
        precipitacao_aux[, valor := as.numeric(valor)]
        
        precipitacao_aux <- stats::na.omit(precipitacao_aux)

        precipitacao_aux[, posto := postos_plu[iposto, posto]]

        if (any(precipitacao_aux[, valor] < 0)) {
            stop(paste0(" precipitacao observada da data ", precipitacao_aux[valor < 0, data]," negativa para a sub-bacia ", nome_subbacia))
        }

        precipitacao <- rbind(precipitacao, precipitacao_aux)
    }
    precipitacao <- data.table::setcolorder(precipitacao, c("data", "posto", "valor"))

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

    pattern <- "modelos_precipitacao.txt"  

    arquivo <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)

    arq <- file.path(pasta_entrada, arquivo)

    if (length(arq) == 0) {
        stop("nao existe o arquivo do tipo modelos_precipitacao.txt")
    }

    dat <- data.table::fread(arq, header = FALSE)
    numero_cenarios <- as.numeric(dat[1])
    
    if (is.na(numero_cenarios)) stop("Valor nao numerico de cenarios no arquivo modelos_precipitacao.txt")
    
    if (numero_cenarios < 0) stop("Numero negativo de cenarios no arquivo modelos_precipitacao.txt")

    if (numero_cenarios == 0) stop("Valor nulo de cenarios no arquivo modelos_precipitacao.txt")

    dat <- na.omit(dat)
    dat <- dat[!apply(dat, 1, function(row) all(is.na(row) | row == ""))]

    if (numero_cenarios != (nrow(dat) - 1)) stop("Numero de cenarios diferente da quantidade declarada no arquivo modelos_precipitacao.txt")
    
    duplicados <- dat[duplicated(V1) | duplicated(V1, fromLast = TRUE), V1]

    if (length(duplicados) > 0) stop(paste0("A sub-bacia ", unique(duplicados), " esta duplicada no arquivo modelos_precipitacao.dat"))


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
#' @importFrom utils read.table
#' @return data.table pontos_grade com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{nome_cenario_1}{primeira parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{nome_cenario_2}{segunda parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{latitude}{latitude do ponto de grade do cenario}
#'     \item{longitude}{longitude do ponto de grade do cenario}
#'     }
#' @export
le_entrada_pontos_grade <- function(pasta_entrada, nome_subbacia, modelos_precipitacao) {

    if (missing("nome_subbacia")) {
        stop("forneca o nome da sub-bacia para a leitura do arquivo de pontos de grade")
    }

    pontos_grade <- data.table::data.table()
    for (cenario in unique(modelos_precipitacao$nome_cenario[, nome_cenario_1])){
        pattern <- paste0(nome_subbacia, "_", cenario, ".txt")
        
        arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)

        arq <- file.path(pasta_entrada, arq)

        if (length(arq) == 0) {
            stop(paste0("nao existe o arquivo ", pattern))
        }
        aux <- readLines(arq)
        aux <- aux[nchar(aux) > 0]
        aux <- trimws(aux)
        aux <- strsplit(aux, "\\s+")
        
        aux_pontos_grade <- data.table::copy(modelos_precipitacao$nome_cenario)
        aux_pontos_grade[, longitude := as.numeric(aux[[2]][1])]
        aux_pontos_grade[, latitude := as.numeric(aux[[2]][2])]
        aux_pontos_grade[, nome := nome_subbacia]

        if(is.na(as.numeric(aux[[1]][1]))) stop(paste0("Valor nao numerico de Numero de pontos de grade no arquivo ", arq))

        if(as.numeric(aux[[1]][1]) <= 0) stop(paste0("Valor menor ou igual a zero de pontos de grade no arquivo ", arq))

        if(as.numeric(aux[[1]][1]) != (length(aux) - 1)) stop(paste0("Numero de pontos de grade diferente da quantidade declarada no arquivo ", arq))

        if (as.numeric(aux[[1]][1]) > 1) {
            for (iponto in 3:(as.numeric(aux[[1]][1]) + 1)) {
                aux_pontos_grade2 <- data.table::copy(modelos_precipitacao$nome_cenario)
                aux_pontos_grade2[, longitude := as.numeric(aux[[iponto]][1])]
                aux_pontos_grade2[, latitude := as.numeric(aux[[iponto]][2])]
                aux_pontos_grade2[, nome := nome_subbacia]
                aux_pontos_grade <- rbind(aux_pontos_grade, aux_pontos_grade2)
            }
        }
        pontos_grade <- rbind(pontos_grade, aux_pontos_grade)
    }
    pontos_grade <- data.table::setcolorder(pontos_grade, c("nome", "nome_cenario_1", "nome_cenario_2", "latitude", "longitude"))
    
    if (any(is.na(pontos_grade[, latitude] ))) {
        stop(paste0("Valor nao numerico de latitude no arquivo ", arq))
    }

    if (any(is.na(pontos_grade[, longitude] ))) {
        stop(paste0("Valor nao numerico de longitude no arquivo ", arq))
    }

    if (any(abs(pontos_grade[, longitude]) >= 100)) {
        stop(paste0("Valor de longitude maior ou igual a 100 no arquivo ", arq))
    }

    if (any(abs(pontos_grade[, latitude]) >= 100)) {
        stop(paste0("Valor de latitude maior ou igual a 100 no arquivo ", arq))
    }

    pontos_grade
}

#' Le arquivo bat.conf
#'
#' Leitor do arquivo de entrada bat.txt contendo para receber qual padrao de arquivo de previsao esta 
#' @param pasta_entrada o arquivo do tipo "caso.txt"
#' @importFrom  data.table fread
#' @return formato_previsao numero do tipo de arquivo de previsao
#' @export
le_entrada_bat_conf <- function(pasta_entrada) {

    if (missing("pasta_entrada")) stop("forneca o caminho da pasta 'arq_entrada' a leitura do caso")

    pattern <- "bat.conf"
    arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)
    arq <- file.path(pasta_entrada, arq)

    if (length(arq) == 0) {
        stop("nao existe o arquivo do tipo bat.conf")
    }
    
    dat <- data.table::fread(arq, sep = "=")
    
    if (length(dat[V1 == "arqPrev", V1]) == 0) {
        formato_previsao <- 0
    } else {
        formato_previsao <- as.numeric(dat[V1 == "arqPrev", V2])
    }

    formato_previsao
}

#' Leitor de arquivo de previsao de precipitacao do smap/ons
#' 
#' Le arquivo "precipitacao_prevista_p_'data_caso'.txt" utilizado no aplicativo SMAP/ONS
#'
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param pontos_grade data.table com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{nome_cenario_1}{primeira parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{nome_cenario_2}{segunda parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{latitude}{latitude do ponto de grade do cenario}
#'     \item{longitude}{longitude do ponto de grade do cenario}
#'     }
#' @param datas_rodadas data table com as colunas
#'     \itemize{
#'     \item{data}{data da rodada}
#'     \item{numero_dias_previsao}{numero de dias de previsao}
#'     }
#' @importFrom  data.table fread setcolorder setorder
#' @importFrom stats na.omit
#' @return previsao_precipitacao data.table com as colunas
#'     \itemize{
#'     \item{data_rodada}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{nome}{nome da sub-bacia}
#'     \item{valor}{valor da previsao}
#'     }
#' @export
le_entrada_previsao_precipitacao_2 <- function(pasta_entrada, datas_rodadas, pontos_grade) {

    pattern <- paste0("precipitacao_prevista_p", substr(datas_rodadas$data,9,10), substr(datas_rodadas$data,6,7), substr(datas_rodadas$data,3,4), ".dat")
    arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)
    arq <- file.path(pasta_entrada, arq)

    if (length(arq) == 0) {
        stop(paste0("nao existe o arquivo ", pattern))
    }

    previsao_precipitacao <- data.table::fread(arq, header = FALSE)
    colnames(previsao_precipitacao)[1:3] <- c("cenario", "longitude", "latitude")
    previsao_precipitacao[, cenario := tolower(cenario)]
    colnames(previsao_precipitacao)[4:ncol(previsao_precipitacao)] <- as.character(seq.Date(datas_rodadas$data + 1, datas_rodadas$data + ncol(previsao_precipitacao) - 3, 1))
    previsao_precipitacao[, nome_cenario_1 := strsplit(cenario, split = "_")[[1]][1]]
    previsao_precipitacao[, nome_cenario_2 := strsplit(cenario, split = "_")[[1]][2]]
    
    previsao_precipitacao <- merge(previsao_precipitacao, pontos_grade, by = c("latitude", "longitude", "nome_cenario_1", "nome_cenario_2"))

    previsao_precipitacao[, nome_cenario_1 := NULL]
    previsao_precipitacao[, nome_cenario_2 := NULL]
    previsao_precipitacao[, latitude := NULL]
    previsao_precipitacao[, longitude := NULL]

    previsao_precipitacao <- data.table::melt(previsao_precipitacao, id.vars = c("nome", "cenario"), variable.name = "data_previsao",
           value.name = "valor")

    previsao_precipitacao[, data_rodada := datas_rodadas$data]

    previsao_precipitacao[, data_rodada := as.Date(data_rodada)]
    previsao_precipitacao[, data_previsao := as.Date(data_previsao)]
    previsao_precipitacao[, valor := as.numeric(valor)]
    
    previsao_precipitacao <- stats::na.omit(previsao_precipitacao)

    previsao_precipitacao <- data.table::setcolorder(previsao_precipitacao, c("data_rodada","data_previsao", "cenario", "nome", "valor"))

    data.table::setorder(previsao_precipitacao, nome, data_rodada, data_previsao, cenario)

    if (any(previsao_precipitacao[, valor] < 0)) {
        stop(paste0(" previsao de precipitacao da data ", previsao_precipitacao[valor < 0, data_previsao]," negativa para a sub-bacia ", previsao_precipitacao[valor < 0, nome]))
    }

    previsao_precipitacao
}

#' Leitor de arquivo de previsao de precipitacao do smap/ons
#' 
#' Le arquivo "'nome_cenario'_p_'data_caso'a'data_previsao'.txt" utilizado no aplicativo SMAP/ONS
#'
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param pontos_grade data.table com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{nome_cenario_1}{primeira parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{nome_cenario_2}{segunda parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{latitude}{latitude do ponto de grade do cenario}
#'     \item{longitude}{longitude do ponto de grade do cenario}
#'     }
#' @param datas_rodadas data table com as colunas
#'     \itemize{
#'     \item{data}{data da rodada}
#'     \item{numero_dias_previsao}{numero de dias de previsao}
#'     }
#' @param nome_cenario nome do cenario a ser simulado
#' @importFrom  data.table fread setcolorder setorder
#' @importFrom stats na.omit
#' @return previsao_precipitacao data.table com as colunas
#'     \itemize{
#'     \item{data_rodada}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{nome}{nome da sub-bacia}
#'     \item{valor}{valor da previsao}
#'     }
#' @export
le_entrada_previsao_precipitacao_1 <- function(pasta_entrada, datas_rodadas, pontos_grade, nome_cenario) {

    data_final <- as.character(datas_rodadas$data + datas_rodadas$numero_dias_previsao)

    pattern <- paste0(nome_cenario, "_p", substr(datas_rodadas$data,9,10), substr(datas_rodadas$data,6,7), substr(datas_rodadas$data,3,4),"a",
     substr(data_final,9,10), substr(data_final,6,7), substr(data_final,3,4), ".dat")
    arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)
    arq <- file.path(pasta_entrada, arq)

    if (length(arq) == 0) {
        stop(paste0("nao existe o arquivo ", pattern))
    }

    previsao_precipitacao <- data.table::fread(arq, header = FALSE, colClasses = "double")
    colnames(previsao_precipitacao)[1:2] <- c("longitude", "latitude")
    colnames(previsao_precipitacao)[3:ncol(previsao_precipitacao)] <- as.character(seq.Date(datas_rodadas$data + 1, datas_rodadas$data + ncol(previsao_precipitacao) - 2, 1))
    previsao_precipitacao$cenario <- nome_cenario
    previsao_precipitacao[, cenario := tolower(cenario)]
    
    nome_1 <- strsplit(nome_cenario, split = "_")[[1]][1]
    nome_2 <- strsplit(nome_cenario, split = "_")[[1]][2]

    previsao_precipitacao <- merge(previsao_precipitacao, pontos_grade[nome_cenario_1 == nome_1 & nome_cenario_2 == nome_2], by = c("latitude", "longitude"))

    previsao_precipitacao[, nome_cenario_1 := NULL]
    previsao_precipitacao[, nome_cenario_2 := NULL]
    previsao_precipitacao[, latitude := NULL]
    previsao_precipitacao[, longitude := NULL]

    previsao_precipitacao <- data.table::melt(previsao_precipitacao, id.vars = c("nome", "cenario"), variable.name = "data_previsao",
           value.name = "valor")

    previsao_precipitacao[, data_rodada := datas_rodadas$data]

    previsao_precipitacao[, data_rodada := as.Date(data_rodada)]
    previsao_precipitacao[, data_previsao := as.Date(data_previsao)]
    previsao_precipitacao[, valor := as.numeric(valor)]
    
    previsao_precipitacao <- stats::na.omit(previsao_precipitacao)

    previsao_precipitacao <- data.table::setcolorder(previsao_precipitacao, c("data_rodada","data_previsao", "cenario", "nome", "valor"))

    data.table::setorder(previsao_precipitacao, nome, data_rodada, data_previsao, cenario)

    if (any(previsao_precipitacao[, valor] < 0)) {
        stop(paste0(" previsao de precipitacao da data ", previsao_precipitacao[valor < 0, data_previsao]," negativa para a sub-bacia ", previsao_precipitacao[valor < 0, nome]))
    }

    previsao_precipitacao
}

#' Leitor de arquivo de previsao de precipitacao do smap/ons
#' 
#' Le arquivo "'nome_cenario'_p_'data_caso'a'data_previsao'.txt" utilizado no aplicativo SMAP/ONS
#'
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @param pontos_grade data.table com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{nome_cenario_1}{primeira parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{nome_cenario_2}{segunda parte do nome dos cenarios de precipitacao considerados o caso}
#'     \item{latitude}{latitude do ponto de grade do cenario}
#'     \item{longitude}{longitude do ponto de grade do cenario}
#'     }
#' @param data_previsao data da previsao
#' @param datas_rodadas data table com as colunas
#'     \itemize{
#'     \item{data}{data da rodada}
#'     \item{numero_dias_previsao}{numero de dias de previsao}
#'     }
#' @param nome_cenario nome do cenario a ser simulado
#' @importFrom  data.table fread setcolorder setorder
#' @importFrom stats na.omit
#' @return previsao_precipitacao data.table com as colunas
#'     \itemize{
#'     \item{data_rodada}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{nome}{nome da sub-bacia}
#'     \item{valor}{valor da previsao}
#'     }
#' @export
le_entrada_previsao_precipitacao_0 <- function(pasta_entrada, datas_rodadas, data_previsao, pontos_grade, nome_cenario) {

    pattern <- paste0(nome_cenario, "_p", substr(datas_rodadas$data,9,10), substr(datas_rodadas$data,6,7), substr(datas_rodadas$data,3,4),"a",
    substr(data_previsao,9,10), substr(data_previsao,6,7), substr(data_previsao,3,4), ".dat")
    arq <- list.files(path = pasta_entrada, pattern = pattern, ignore.case = TRUE)
    arq <- file.path(pasta_entrada, arq)
    previsao_precipitacao <- data.table::data.table()
    if (length(arq) == 0) {
        if (data_previsao == datas_rodadas$data + 1) stop(paste0("Nao existe o arquivo ", pattern))
        pattern2 <- paste0(nome_cenario, "_p", substr(datas_rodadas$data,9,10), substr(datas_rodadas$data,6,7), substr(datas_rodadas$data,3,4))
        arquivos <- list.files(pasta_entrada)
        arquivos <- grep(pattern2, tolower(arquivos), value = TRUE) 
        for (data_seguinte in (data_previsao + 1):(datas_rodadas$data + datas_rodadas$numero_dias_previsao - 1)){
            data <- as.Date(data_seguinte)
            pattern2 <- paste0(nome_cenario, "_p", substr(datas_rodadas$data,9,10), substr(datas_rodadas$data,6,7), substr(datas_rodadas$data,3,4),"a",
                    substr(data,9,10), substr(data,6,7), substr(data,3,4), ".dat")
            if(file.exists(file.path(pasta_entrada, pattern2))) stop(paste0("Nao existe o arquivo ", pattern))
        }
    } else {
        previsao_precipitacao <- data.table::fread(arq, header = FALSE)
        previsao_precipitacao[, V1 := as.numeric(V1)]
        if (any(is.na(previsao_precipitacao[, V1]))) stop(paste0("Valor nao numerico de longitude no arquivo ", arq))
        if (any(abs(previsao_precipitacao[, V1]) >= 100)) stop(paste0("Valor de longitude com 3 inteiros no arquivo ", arq))
        previsao_precipitacao[, V2 := as.numeric(V2)]
        if (any(is.na(previsao_precipitacao[, V2]))) stop(paste0("Valor nao numerico de latitude no arquivo ", arq))
        if (any(abs(previsao_precipitacao[, V2]) >= 100)) stop(paste0("Valor de latitude com 3 inteiros no arquivo ", arq))
        previsao_precipitacao[, V3 := as.numeric(V3)]
        if (any(is.na(previsao_precipitacao[, V3]))) stop(paste0("Valor nao numerico de previsao de precipitacao no arquivo ", arq))
        if (any(previsao_precipitacao[, V3] < 0)) stop(paste0("Valor negativo de previsao de precipitacao no arquivo ", arq))

        colnames(previsao_precipitacao)[1:2] <- c("longitude", "latitude")
        colnames(previsao_precipitacao)[3] <- as.character(data_previsao)
        previsao_precipitacao$cenario <- nome_cenario
        previsao_precipitacao[, cenario := tolower(cenario)]
        
        nome_1 <- strsplit(nome_cenario, split = "_")[[1]][1]
        nome_2 <- strsplit(nome_cenario, split = "_")[[1]][2]

        previsao_precipitacao <- merge(previsao_precipitacao, pontos_grade[nome_cenario_1 == nome_1 & nome_cenario_2 == nome_2], by = c("latitude", "longitude"))
        if(nrow(previsao_precipitacao) != nrow(pontos_grade)) stop(paste0("Numero de pontos de grade declarado anteriormente e diferente no arquivo ", arq))

        previsao_precipitacao[, nome_cenario_1 := NULL]
        previsao_precipitacao[, nome_cenario_2 := NULL]
        previsao_precipitacao[, latitude := NULL]
        previsao_precipitacao[, longitude := NULL]

        previsao_precipitacao <- data.table::melt(previsao_precipitacao, id.vars = c("nome", "cenario"), variable.name = "data_previsao",
            value.name = "valor")

        previsao_precipitacao[, data_rodada := datas_rodadas$data]

        previsao_precipitacao[, data_rodada := as.Date(data_rodada)]
        previsao_precipitacao[, data_previsao := as.Date(data_previsao)]
        previsao_precipitacao[, valor := as.numeric(valor)]
        
        previsao_precipitacao <- stats::na.omit(previsao_precipitacao)

        previsao_precipitacao <- data.table::setcolorder(previsao_precipitacao, c("data_rodada","data_previsao", "cenario", "nome", "valor"))

        data.table::setorder(previsao_precipitacao, nome, data_rodada, data_previsao, cenario)

        if (any(previsao_precipitacao[, valor] < 0)) {
            stop(paste0(" previsao de precipitacao da data ", previsao_precipitacao[valor < 0, data_previsao]," negativa para a sub-bacia ", previsao_precipitacao[valor < 0, nome]))
        }
    }

    previsao_precipitacao
}

#' Leitor de arquivo arquivos de entrada do modelo SMAP/ONS
#' 
#' Le arquivo "'nome_cenario'_p_'data_caso'_'data_previsao'.txt" utilizado no aplicativo SMAP/ONS
#'
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @importFrom  data.table rbindlist
#' @return saida lista com as variaveis: parametros data table com os parametros dos modelos
#'     \itemize{
#'     \item{Nome}{Nome da sub-bacia}
#'     \item{parametro}{nome do parametro}
#'     \item{valor}{valor do parametro}
#'     }
#' inicializacao data.table com a inicializacao com as colunas:
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{variavel}{vazao de base inicial}
#'     \item{valor}{valor da variavel}
#'     }
#' precipitacao data table com o historico de precipitacao com as colunas:
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' previsao_precipitacao data.table com a previsao de precipitacao com as colunas:
#'     \itemize{
#'     \item{data_rodada}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da previsao}
#'     }
#' evapotranspiracao data.table com o historico de NC deevapotranspiracao com as colunas:
#'     \itemize{
#'     \item{mes}{mes da NC}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da NC de evapotranspiracao observada}
#'     }
#' vazao data table com o historico de vazao com as colunas:
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' postos_plu data table contendo a relacao sub-bacia x postos_plu com as colunas:
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{posto}{nome do posto plu}
#'     \item{valor}{peso do posto plu}
#'     }
#' datas_rodadas data table contendo as datas dos casos a serem executados e seus respectivos horizontes:
#'     \itemize{
#'     \item{data}{data do caso}
#'     \item{numero_dias_previsao}{horizonte do caso}
#'     }
#' caso lista contendo os seguintes parametros
#'     \itemize{
#'     \item{numero_subbacias}{numero de sub-bacias do caso}
#'     \item{nome_subbacia}{vetor com o nome das sub-bacias}
#'     }
#' @export
le_arq_entrada <- function(pasta_entrada) {

    caso <- le_entrada_caso(pasta_entrada)
    tipo_previsao <- le_entrada_bat_conf(pasta_entrada)
    modelos_precipitacao <- le_entrada_modelos_precipitacao(pasta_entrada)

    parametros <- data.table::rbindlist(lapply(caso$nome_subbacia, function(sub_bacia) {
  le_entrada_parametros(pasta_entrada, sub_bacia)
        }))

    evapotranspiracao <- data.table::rbindlist(lapply(caso$nome_subbacia, function(sub_bacia) {
  le_entrada_evapotranspiracao(pasta_entrada, sub_bacia)
        }))

    inicializacao <- data.table::rbindlist(lapply(caso$nome_subbacia, function(sub_bacia) {
  result <- le_entrada_inicializacao(pasta_entrada, sub_bacia)
  result[[1]]
        }))
    inicializacao[variavel == "Tuin", valor := valor / 100] 
    
    datas_rodadas <- data.table::rbindlist(lapply(caso$nome_subbacia, function(sub_bacia) {
  result <- le_entrada_inicializacao(pasta_entrada, sub_bacia)
  result[[2]]
        }))

    datas_rodadas <- unique(datas_rodadas)

    vazao <- data.table::rbindlist(lapply(caso$nome_subbacia, function(sub_bacia) {
  le_entrada_vazao(pasta_entrada, sub_bacia)
        }))

    postos_plu <- data.table::rbindlist(lapply(caso$nome_subbacia, function(sub_bacia) {
  le_entrada_posto_plu(pasta_entrada, sub_bacia)
        }))

    precipitacao <- data.table::rbindlist(lapply(caso$nome_subbacia, function(sub_bacia) {
  le_entrada_precipitacao(pasta_entrada, postos_plu[nome %in% sub_bacia])
        }))

    precipitacao <- data.table::rbindlist(lapply(caso$nome_subbacia, function(sub_bacia) {
  ponderacao_espacial(precipitacao, postos_plu[nome %in% sub_bacia])
        }))

    pontos_grade <- data.table::rbindlist(lapply(caso$nome_subbacia, function(sub_bacia) {
  le_entrada_pontos_grade(pasta_entrada, sub_bacia, modelos_precipitacao)
        }))

    if (tipo_previsao == 2) {
        previsao_precipitacao <- le_entrada_previsao_precipitacao_2(pasta_entrada, datas_rodadas, pontos_grade)
    } else if (tipo_previsao == 1) {
        cenarios <- data.table::as.data.table(paste0(unique(pontos_grade$nome_cenario_1),"_",unique(pontos_grade$nome_cenario_2)))
        previsao_precipitacao <- data.table::rbindlist(lapply(cenarios$V1, function(nome_cenario) {
            le_entrada_previsao_precipitacao_1(pasta_entrada, datas_rodadas, pontos_grade, nome_cenario)
            }))
    } else {
        datas <- data.table::as.data.table(seq.Date(datas_rodadas$data + 1, datas_rodadas$data + datas_rodadas$numero_dias_previsao, 1))
        cenarios <- data.table::as.data.table(paste0(unique(pontos_grade$nome_cenario_1),"_",unique(pontos_grade$nome_cenario_2)))
        previsao_precipitacao <- data.table::rbindlist(lapply(cenarios$V1, function(nome_cenario) {
            data.table::rbindlist(lapply(datas$V1, function(data_previsao) {
                le_entrada_previsao_precipitacao_0(pasta_entrada, datas_rodadas, data_previsao, pontos_grade, nome_cenario)
            }))
        }))
    }
    
    previsao_precipitacao <- completa_previsao(previsao_precipitacao, datas_rodadas)
    previsao_precipitacao <- previsao_precipitacao[, mean(valor), by = .(data_rodada, data_previsao, cenario, nome)]
    colnames(previsao_precipitacao)[5] <- "valor"

    saida <- list(parametros = parametros, vazao = vazao, precipitacao = precipitacao, evapotranspiracao = evapotranspiracao,
        previsao_precipitacao = previsao_precipitacao, postos_plu = postos_plu, inicializacao = inicializacao,
        datas_rodadas = datas_rodadas, caso = caso)

    saida
}
