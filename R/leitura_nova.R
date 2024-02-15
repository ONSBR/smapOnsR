# LEITURA DE ARQUIVOS DE ENTRADA NOVOS SMAP---------------------------------

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
#'     \item{nome - nome da sub-bacia}
#'     \item{parametro - nome do parametro}
#'     \item{valor - valor do parametro}
#'     }
#' @export 
le_parametros <- function(arq) {

    if (!file.exists(arq)) {
        stop("o arquivo de parametros nao existe.")
    }

    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("nome", "parametro", "valor"))) {
        stop("o arquivos deve deve possuir colunas 'nome', 'parametro' e 'valor'")
    }

    dat[, valor := as.numeric(valor)]

    if (any(is.na(dat$valor))) {
        stop("o parametro ", dat[is.na(dat$valor), parametro], " da sub-bacia ", 
        dat[is.na(dat$valor), nome], " no arquivo ", arq, " possui valor nao numerico")
    }
    
    if (any(is.na(dat$nome) | dat$nome == "")) {
        stop("a coluna 'nome' do arquivo ", arq, " possui valores vazios")
    }

    if (any(dat$valor < 0)) {
        stop("o parametro ", dat[dat$valor < 0, parametro], " da sub-bacia ", 
        dat[dat$valor < 0, nome], " no arquivo ", arq, " possui valor negativo")
    }

    if (any(dat$parametro == "Area" & dat$valor == 0)) {
        stop("a coluna 'valor' do arquivo ", arq, " possui valor igual a zero associados ao parametro 'Area'")
    }
    
    if (any(duplicated(dat[, .(nome, parametro)]))) {
        stop("o parametro ", dat[duplicated(dat[, .(nome, parametro)]), parametro], " da sub-bacia ", 
        dat[duplicated(dat[, .(nome, parametro)]), nome], " no arquivo ", arq, " esta duplicado")
    }

    parametros <- c("Area", "Str", "K2t", "Crec", "Ai", "Capc", 
    "K_kt", "K2t2", "H1", "H", "K3t", "K1t", "Ecof", "Pcof", "Ecof2")

    teste <- dat[, setdiff(parametros, parametro), by = c("nome")]

    if (nrow(teste) != 0) {
        stop(paste0("falta o parametro ", teste$V1, " para a sub-bacia ", teste$nome, " no arquivo ", arq, "\n"))
    }

    teste <- dat[substr(parametro, 1, 2) == "Kt", sum(valor), by = c("nome")]

    if (max(teste$V1) > 1.005) {
        stop(paste0("o somatorio dos Kts da sub-bacia ", teste[V1 > 1.005, nome], ", e maior que 1.005 no arquivo ", arq, "\n"))
    }

    if (min(teste$V1) < 0.995) {
        stop(paste0("o somatorio dos Kts da sub-bacia ", teste[V1 < 0.995, nome], ", e menor que 0.995 no arquivo ", arq, "\n"))
    }

    teste <- dat[substr(parametro, 1, 2) == "Kt", length(valor), by = "nome"]
    if (any(teste$V1 != 63)) {
        stop(paste0("o somatorio dos Kts da sub-bacia ", teste[V1 != 63, nome], 
        "nao possui o valor para os 63 Kts no arquivo ", arq, ".\n"))
    }

    dat
}

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
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da precipitacao observada}
#'     }
#' @export 

le_historico_verificado <- function(arq) {

    if (!file.exists(arq)) {
        stop("o arquivo de historico ", arq, " nao existe.")
    }

    dat <- data.table::fread(arq, sep = ";")
    if (colnames(dat)[1] != "data"){
        stop("a primeira coluna do arquivo ", arq, " deve ter o nome 'data' e conter as datas do registro historico.")
    }
    dat[, data := as.Date(data, format = "%d/%m/%Y")]
    
    if (any(duplicated(dat[, data]))) {
         stop(paste0("a data ", dat[duplicated(data), data], ", esta duplicada no arquivo ", arq, ".\n"))
    }

    dat <- data.table::melt(dat, id.vars = "data", variable.name = "posto",
          value.name = "valor")
    colnames(dat)[1] <- "data"

    data.table::setorder(dat, posto, data)
    data.table::setcolorder(dat, c("data", "posto", "valor"))
    dat[, valor := as.numeric(valor)]

    if (any(is.na(dat$data))) {
        stop("a coluna 'data' do arquivo ", arq, " possui valores nao numericos", ".\n")
    }

    if (any(is.na(dat$valor))) {
        stop("a sub-bacia ", dat[is.na(valor), posto], " possui valor nao numerico para a data ",
        dat[is.na(valor), data], " no arquivo ", arq, ".\n")
    }

    if (any(dat$valor < 0)) {
        stop("a sub-bacia ", dat[valor < 0, posto], " possui valor negativo para a data ",
        dat[valor < 0, data], " no arquivo ", arq, ".\n")
    }

    return(dat)
}

#' Le csv com os dados de precipitacao prevista
#' 
#' Realiza a leitura do csv 'precipitacao_prevista.csv' com os dados iniciais
#' 
#' @param arq nome do arquivo "precipitacao_prevista.csv"
#' @importFrom  data.table data.table as.data.table
#' @importFrom arrow read_parquet
#' @return data.table com a precipitacao prevista com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada do modelo que gerou a previsao}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - codigo do cenario}
#'     \item{nome - nome da sub bacia}
#'     \item{valor - valor da previsao de precipitacao}
#'     }
#' @export 
le_precipitacao_prevista <- function(arq) {

    if (!file.exists(arq)) {
        stop("o arquivo de precipitacao prevista nao existe.")
    }

    extensao <- strsplit(arq, split = "\\.")[[1]][2]
    if (extensao == "parquet") {
        dat <- data.table::as.data.table(arrow::read_parquet(arq))
    } else {
        dat <- data.table::fread(arq)
    }

    if (any(colnames(dat) != c("data_rodada", "data_previsao", "cenario", "nome", "valor"))) {
        stop("o arquivo deve possuir as seguintes colunas 'data_rodada', 'data_previsao', 'cenario', 'nome', 'valor'")
    }

    dat[, valor := as.numeric(valor)]
    dat[, data_previsao := as.Date(data_previsao, format = "%d/%m/%Y")]
    dat[, data_rodada := as.Date(data_rodada, format = "%d/%m/%Y")]

    if (any(is.na(dat$valor))) {
        stop(paste0("a sub-bacia ", dat[is.na(valor), nome], " possui valor nao numerico para a data de previsao ",
        dat[is.na(valor), data_previsao], " do caso de ", dat[is.na(valor), data_previsao],
        " no arquivo ", arq, ".\n"))
    }

    if (any(is.na(dat$data_previsao))) {
        stop(paste0("a coluna 'data_previsao' do arquivo ", arq, " possui valores nao numericos para a sub-bacia ", dat[is.na(data_previsao), unique(nome)], ".\n"))
    }

    if (any(is.na(dat$data_rodada))) {
        stop(paste0("a coluna 'data_rodada' do arquivo ", arq, " possui valores nao numericos para a sub-bacia ", dat[is.na(data_rodada), unique(nome)], ".\n"))
    }

    if (any(dat$valor < 0)) {
        stop(paste0("a sub-bacia ", dat[valor < 0, nome], " possui valor negativo para a data de previsao ",
        dat[valor < 0, data_previsao], " do caso de ", dat[valor < 0, data_previsao],
        " no arquivo ", arq, ".\n"))
    }
       
    teste_completo <- data.table::data.table()
    for (idata in 1:dat[, length(unique(data_rodada))]) {
        data <- dat[, unique(data_rodada)[idata]]
        datas_previstas <- dat[data_rodada == data, unique(data_previsao)]
        teste <- dat[data_rodada == data, setdiff(datas_previstas, data_previsao), by = c("data_rodada", "nome", "cenario")]
        teste_completo <- data.table::rbindlist(list(teste_completo, teste))
    }

    if (nrow(teste_completo) != 0) {
        teste_completo[, V1 := data.table::as.IDate(V1)]
        stop(paste0("falta a data de previsao ", teste_completo$V1, " para o cenario ", 
        teste_completo$cenario, " da sub-bacia ", teste_completo$nome, 
        " do caso de ", teste_completo$data_rodada, "\n"))
    }

    dat
}

#' Le csv com dados de inicializacao do caso
#' 
#' Realiza a leitura do csv 'inicializacao.csv' com os dados iniciais
#' 
#' @param arq nome do arquivo "inicializacao.csv"
#' @importFrom  data.table data.table
#' @return data.table com a inicializacao com as colunas
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' @export 
le_inicializacao <- function(arq) {

    if (!file.exists(arq)) {
        stop("o arquivo 'inicializacao.csv' nao existe.")
    }
    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("nome", "variavel", "valor"))) {
        stop("o arquivo deve possuir as seguintes colunas 'nome', 'variavel', 'valor'")
    }
    dat[, valor := as.numeric(valor)]
    
    missing_ajusta_precipitacao <- dat[!nome %in% dat[variavel == "ajusta_precipitacao", unique(nome)], 
                                    .(nome = unique(nome), variavel = "ajusta_precipitacao", valor = 0)]
    dat <- rbind(dat, missing_ajusta_precipitacao)
    
    if (any(dat$valor < 0)) {
        stop(paste0("a sub-bacia ", dat[valor < 0, nome], " possui valor negativo para a variavel ",
        dat[valor < 0, variavel], " no arquivo ", arq, ".\n"))
    }

    if (any(dat$variavel == 'ajusta_precipitacao' & dat$valor > 1)) {
        stop(paste0("a sub-bacia ", dat[variavel == "ajusta_precipitacao" & valor < 0, nome], 
        " possui valor maior que 1 para a variavel 'ajusta_precipitacao' no arquivo ", arq, ".\n"))
    }

    if (any(is.na(dat$valor))) {
        stop(paste0("a sub-bacia ", dat[is.na(valor), nome], " possui valor negativo para a variavel ",
        dat[is.na(valor), variavel], " no arquivo ", arq, ".\n"))
    }

    if (any(dat$variavel == 'Tuin' & dat$valor > 100)) {
        stop("o arquivo ", arq, " possui valores maiores do que 100 para a variavel 'Tuin'")
    }

    if (any(dat$variavel == 'numero_dias_assimilacao' & dat$valor < 2)) {
        stop("o arquivo ", arq, " possui valor menor que 2 para a variavel 'numero_dias_assimilacao'")
    }

    if (any(!(dat$variavel %in% c("Ebin", "Supin", "Tuin", "numero_dias_assimilacao", "limite_inferior_ebin",
     "limite_superior_ebin", "limite_superior_prec", "limite_inferior_prec",
     "funcao_objetivo", "ajusta_precipitacao")))) {
        stop("o arquivo ", arq, " possui valores diferentes de 'Ebin', 'Supin', 'Tuin', 
        'numero_dias_assimilacao', 'limite_inferior_ebin', 'limite_superior_ebin', 'limite_superior_prec',
        'limite_inferior_prec', 'funcao_objetivo', 'ajusta_precipitacao' na coluna 'variavel'")
    }

    if (any(duplicated(dat[, .(variavel, nome)]))) {
        stop("o arquivo ", arq, " possui valores duplicados para a sub-bacia ",
        dat[duplicated(dat[, .(variavel, nome)]), nome], 
        " para a variavel ", dat[duplicated(dat[, .(variavel, nome)]), variavel])
    }

    variaveis <- c("Ebin", "Tuin", "Supin", "numero_dias_assimilacao", "limite_inferior_ebin",
     "limite_superior_ebin", "limite_superior_prec", "limite_inferior_prec")

    teste <- dat[, setdiff(variaveis, variavel), by = c("nome")]

    if (nrow(teste) != 0) {
        stop(paste0("falta a variavel ", teste$V1, " para a sub-bacia ", teste$nome, " no arquivo ", arq, "\n"))
    }

    data.table::setorder(dat, "nome")
    dat
}

#' Le csv com dados de datas de execucao do caso
#' 
#' Realiza a leitura do csv 'datasRodadas.csv' com as datas e o horizonte de previsao
#' 
#' @param arq nome do arquivo "inicializacao.csv"
#' @importFrom  data.table fread
#' @return data table contendo as datas dos casos a serem executados e seus respectivos horizontes:
#'     \itemize{
#'     \item{data - data do caso}
#'     \item{numero_dias_previsao - horizonte do caso}
#'     }
#' @export 
le_datas_rodada <- function(arq) {

    if (!file.exists(arq)) {
        stop("o arquivo 'datasRodadas.csv' nao existe.")
    }

    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("data", "numero_dias_previsao"))) {
        stop("o arquivo deve possuir colunas 'data' e 'numero_dias_previsao'")
    }

    dat[, data := as.Date(data, format = "%d/%m/%Y")]
    dat[, numero_dias_previsao := as.numeric(numero_dias_previsao)]

    if (any(is.na(dat$data))) {
        stop("a coluna 'data' do arquivo ", arq, " possui valores vazios")
    }

    if (any(duplicated(dat$data))) {
        stop("a coluna 'data' do arquivo ", arq, " possui valores de data iguais.")
    }

    if (any(is.na(dat$numero_dias_previsao) | dat$numero_dias_previsao == "")) {
        stop("a coluna 'numero_dias_previsao' do arquivo ", arq, " possui valores nao numericos")
    }

    data.table::setorder(dat, data, numero_dias_previsao)
    if (any(dat$numero_dias_previsao < 0)) {
        stop("a coluna 'numero_dias_previsao' do arquivo ", arq, " possui valores negativos")
    }

    if (any(dat$numero_dias_previsao == 0)) {
        stop("a coluna 'numero_dias_previsao' do arquivo ", arq, " possui valores iguais a zero")
    }

    dat[, data := as.Date(data, format = "%d/%m/%Y")]

    dat
}

#' Le csv com o nome dos arquivos do caso
#' 
#' Realiza a leitura do csv 'arquivos.csv' com os arquivos a serem lidos
#' 
#' @param pasta_entrada caminho da pasta com os arquivos de entrada
#' @importFrom  data.table fread
#' @return data table contendo as datas dos casos a serem executados e seus respectivos horizontes:
#'     \itemize{
#'     \item{data - data do caso}
#'     \item{numero_dias_previsao - horizonte do caso}
#'     }
#' @export 
le_arquivos <- function(pasta_entrada) {

    arq <- file.path(pasta_entrada, "arquivos.csv")

    lista_arqs <- list(
        "SUB_BACIAS", "PARAMETROS", "VAZAO_OBSERVADA",
        "EVAPOTRANSPIRACAO_NC", "EVAPOTRANSPIRACAO_OBSERVADA",
        "POSTOS_PLUVIOMETRICOS", "PRECIPITACAO_OBSERVADA",
        "INICIALIZACAO", "DATAS_RODADAS", "PRECIPITACAO_PREVISTA",
        "EVAPOTRANSPIRACAO_PREVISTA")

    if (!file.exists(arq)) {
        stop("o arquivo 'arquivos.csv' nao existe na pasta ", pasta_entrada)
    }

    dat <- data.table::fread(arq)

    for (arquivo in dat$arquivo) {
        if (!(arquivo %in% lista_arqs))
            stop("O valor '", arquivo,"' informado na coluna 'arquivo' do arquivo ", arq, " nao e valido. Informe um dos seguintes valores: ", paste0(lista_arqs, collapse = ", "))
    }

    if (any(colnames(dat) != c("arquivo", "nome_arquivo"))) {
        stop("o arquivo ", arq," deve possuir colunas 'arquivo' e 'nome_arquivo'")
    }

    if (any(is.na(dat$nome_arquivo) | dat$nome_arquivo == "")) {
        stop("o nome do arquivo ", dat[dat$nome_arquivo == "", arquivo], " nao foi declarado no arquivo ", arq)
    }

    if ((any(dat[, arquivo] == "EVAPOTRANSPIRACAO_NC")) & 
        (any(dat[, arquivo] == "EVAPOTRANSPIRACAO_OBSERVADA"))) {
        stop(paste0("Na lista de arquivos a evapotranspiracao potencial foi informada de duas formas distintas"))
    }

    if ((any(dat[, arquivo] == "EVAPOTRANSPIRACAO_NC")) & 
        (any(dat[, arquivo] == "EVAPOTRANSPIRACAO_PREVISTA"))) {
        stop(paste0("Na lista de arquivos a evapotranspiracao potencial foi informada como normal climatologica
        , porem, foi incluido arquivo de previsao de avapotranspiracao potencial"))
    }

    dat
}

#' Le csv com a relacao de postos plu x sub_bacia
#' 
#' Realiza a leitura do csv 'postos_plu.csv' csv com a relacao de postos plu x sub_bacia
#' 
#' @param arq nome do arquivo contendo a relacao de postos plu x sub-bacia
#' @importFrom  data.table fread
#' @return data table contendo as datas dos casos a serem executados e seus respectivos horizontes:
#'     \itemize{
#'     \item{data - data do caso}
#'     \item{numero_dias_previsao - horizonte do caso}
#'     }
#' @export 
le_postos_plu <- function(arq) {

    if (!file.exists(arq)) {
        stop(paste0("nao existe o arquivo ", arq))
    }

    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("nome", "posto", "valor"))) {
        stop("o arquivo ", arq," deve deve possuir colunas 'nome', 'posto' e 'valor'")
    }
    dat[, valor := as.numeric(valor)]

    if (any(is.na(dat$valor))) {
        stop(paste0("o posto pluviometrico ", dat[is.na(dat$valor), posto], " possui valor nao numerico.\n"))
    }

    if (any(dat$valor < 0)) {
        stop(paste0("o posto pluviometrico ", dat[dat$valor < 0, posto], " possui valor negativo.\n"))
    }

    if (any(dat$valor > 1)) {
        stop(paste0("o posto pluviometrico ", dat[dat$valor > 1, posto], " possui valor maior que 1.\n"))
    }

    if (any(is.na(dat$nome) | dat$nome == "")) {
        stop("no arquivo ", arq, " a coluna 'nome' possui valor vazio para o posto ", 
        dat[(is.na(dat$nome) | dat$nome == ""), posto], ".\n")
    }

    if (any(is.na(dat$posto) | dat$posto == "")) {
        stop("no arquivo ", arq, " a coluna 'posto' possui valor vazio para a sub-bacia ", 
        dat[(is.na(dat$posto) | dat$posto == ""), nome], ".\n")
    }

    if (any(duplicated(dat[, .(nome, posto)]))) {
        stop("o posto ", dat[duplicated(dat[, .(nome, posto)]), posto], " da sub-bacia ", 
        dat[duplicated(dat[, .(nome, posto)]), nome], " no arquivo ", arq, " esta duplicado.\n")
    }

    if (any(dat[, sum(valor), by = "nome"]$V1 > 1.005)){
        soma <- dat[, .(valor = sum(valor)), by = nome]
        stop(paste0("Somatorio dos Kes no arquivo ", arq, 
        " maior que 1.005 para a subbacia ", soma[valor > 1.005, nome]), ".\n")
    }

    if (any(dat[, sum(valor), by = "nome"]$V1 < 0.995)){
        soma <- dat[, .(valor = sum(valor)), by = nome]
        stop(paste0("Somatorio dos Kes no arquivo ", arq, 
        " menor que 0.995 para a subbacia ", soma[valor < 0.995, nome]), ".\n")
    }

    dat
}

#' Leitura de csv com NC de evapotranspiracao
#' 
#' Le arquivo csv contendo as normais climatologias evapotranspiracao utilizado no aplicativo SMAP/ONS
#' 
#' @param arq nome do arquivo contendo a relacao de postos plu x sub-bacia
#' @importFrom  data.table fread setcolorder
#' @return data.table evapotranspiracao com as colunas
#'     \itemize{
#'     \item{mes - mes da NC}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da NC de evapotranspiracao observada}
#'     }
#' @export
le_evapotranspiracao_nc <- function(arq) {

    if (!file.exists(arq)) {
        stop(paste0("nao existe o arquivo ", arq))
    }

    dat <- data.table::fread(arq)
    dat[, valor := as.numeric(valor)]
    dat[, mes := as.numeric(mes)]
    if (any(colnames(dat) != c("mes", "nome", "valor"))) {
        stop("o arquivo ", arq," deve deve possuir colunas 'mes', 'nome' e 'valor'")
    }

    if (any(is.na(dat$valor))) {
        stop(paste0("a sub-bacia ", dat[is.na(valor), nome], " possui valor  de evapotranspiracao potencial nao numerico para o mes ",
        dat[is.na(valor), mes], " no arquivo ", arq, ".\n"))
    }

    if (any(dat$valor < 0)) {
        stop(paste0("a sub-bacia ", dat[valor < 0, nome], " possui valor de evapotranspiracao potencial negativo para o mes ",
        dat[valor < 0, mes], " no arquivo ", arq, ".\n"))
    }

    if (any(is.na(dat$mes))) {
        stop(paste0("a sub-bacia ", dat[is.na(mes), nome], " possui valor nao numerico na coluna mes",
        " no arquivo ", arq, ".\n"))
    }

    if (any(dat$mes > 12)) {
        stop("a coluna 'mes' do arquivo ", arq, " possui valores maiores que 12")
    }

    if (any(dat$mes < 1)) {
        stop("a coluna 'mes' do arquivo ", arq, " possui valores menores que 1")
    }
    data.table::setcolorder(dat, c("mes", "nome", "valor"))

    dat
}

#' Le arquivos de entrada em novo formato
#' 
#' Realiza a leitura dos arquivos de entrada em novo formato
#' 
#' @param pasta_entrada nome da pasta com os arquivos de entrada
#' @return saida lista contendo os seguintes data tables:
#' 
#' @export
le_arq_entrada_novo <- function(pasta_entrada) {
    arquivos <- le_arquivos(pasta_entrada)

    if (any(arquivos[, arquivo] == "SUB_BACIAS")) {
        sub_bacias <- data.table::fread(file.path(pasta_entrada,arquivos[arquivo == "SUB_BACIAS", nome_arquivo]), sep = ";")
    } else {
        stop("nao existe o arquivo de sub-bacias")
    }

    if (any(arquivos[, arquivo] == "PARAMETROS")) {
        parametros <- le_parametros(file.path(pasta_entrada,arquivos[arquivo == "PARAMETROS", nome_arquivo])) #[nome %in% sub_bacias$nome]
        if (!all(sub_bacias$nome %in% parametros[, nome])) {
            stop(paste0("Falta a sub-bacia ", sub_bacias[!nome %in% parametros$nome, nome], " no arquivo ", arquivos[arquivo == "PARAMETROS", nome_arquivo], ".\n"))
        }
    } else {
        stop("nao existe o arquivo de parametros")
    }

    if (any(arquivos[, arquivo] == "INICIALIZACAO")) {
        inicializacao <-  le_inicializacao(file.path(pasta_entrada,arquivos[arquivo == "INICIALIZACAO", nome_arquivo]))
        inicializacao[variavel == "Tuin", valor := valor / 100]
        if (!all(sub_bacias$nome %in% inicializacao$nome)) {
            stop(paste0("Falta a sub-bacia ", sub_bacias[!nome %in% inicializacao$nome, nome], " no arquivo ", arquivos[arquivo == "INICIALIZACAO", nome_arquivo], ".\n"))
        }
    } else {
        stop("nao existe o arquivo de estados iniciais do smap")
    }

    if (any(arquivos[, arquivo] == "DATAS_RODADAS")) {
        datas_rodadas <- le_datas_rodada(file.path(pasta_entrada,arquivos[arquivo == "DATAS_RODADAS", nome_arquivo]))
    } else{
        stop("nao existe o arquivo de datas a serem executadas")
    }

    if (any(arquivos[, arquivo] == "VAZAO_OBSERVADA")) {
        vazao_observada <- le_historico_verificado(file.path(pasta_entrada, arquivos[arquivo == "VAZAO_OBSERVADA", nome_arquivo]))[posto %in% sub_bacias$nome]
        if (!all(sub_bacias$nome %in% vazao_observada$posto)) {
            stop(paste0("Falta a sub-bacia ", sub_bacias[!nome %in% vazao_observada$posto, nome], " no arquivo ", arquivos[arquivo == "VAZAO_OBSERVADA", nome_arquivo], ".\n"))
        }
        lapply(sub_bacias$nome, function(nome_subbacia) {
            data_inicio <- datas_rodadas[, min(data)] - inicializacao[nome == nome_subbacia & variavel == "numero_dias_assimilacao", valor] + 1
            data_fim <- datas_rodadas[, max(data)] - 1
            if (vazao_observada[, min(data)] > data_inicio)
            stop(stop(paste0("O historico de vazao da sub-bacia ", nome_subbacia, " deve comecar na data ", data_inicio)))
            if (vazao_observada[, max(data)] < data_fim)
            stop(stop(paste0("O historico de vazao da sub-bacia ", nome_subbacia, " deve terminar na data ", data_fim)))
        })
    } else {
        stop("nao existe o arquivo de vazoes observadas")
    }

    evapotranspiracao_observada <- data.table::data.table()
    evapotranspiracao_nc <- data.table::data.table()
    evapotranspiracao_prevista <- data.table::data.table()
    
    if (any(arquivos[, arquivo] == "EVAPOTRANSPIRACAO_NC")) {
        evapotranspiracao_observada <- data.table::data.table()
        evapotranspiracao_nc <- le_evapotranspiracao_nc(file.path(pasta_entrada, arquivos[arquivo == "EVAPOTRANSPIRACAO_NC", nome_arquivo]))
        if (!all(sub_bacias$nome %in% evapotranspiracao_nc$nome)) {
            stop(paste0("Falta a sub-bacia ", sub_bacias[!nome %in% evapotranspiracao_nc$nome, nome], " no arquivo ", arquivos[arquivo == "EVAPOTRANSPIRACAO_NC", nome_arquivo], ".\n"))
        }
    }

    if (any(arquivos[, arquivo] == "EVAPOTRANSPIRACAO_OBSERVADA")) {
        evapotranspiracao_observada <- le_historico_verificado(file.path(pasta_entrada,arquivos[arquivo == "EVAPOTRANSPIRACAO_OBSERVADA", nome_arquivo]))[posto %in% sub_bacias$nome]
        if (!all(sub_bacias$nome %in% evapotranspiracao_observada$posto)) {
            stop(paste0("Falta a sub-bacia ", sub_bacias[!nome %in% evapotranspiracao_observada$posto, nome], " no arquivo ", arquivos[arquivo == "EVAPOTRANSPIRACAO_OBSERVADA", nome_arquivo], ".\n"))
        }
        lapply(sub_bacias$nome, function(nome_subbacia) {
            data_inicio <- datas_rodadas[, min(data)] - inicializacao[nome == nome_subbacia & variavel == "numero_dias_assimilacao", valor] + 1
            data_fim <- datas_rodadas[, max(data)] - 1
            if (evapotranspiracao_observada[, min(data)] > data_inicio)
            stop(stop(paste0("O historico de vazao da sub-bacia ", nome_subbacia, " deve comecar na data ", data_inicio)))
            if (evapotranspiracao_observada[, max(data)] < data_fim)
            stop(stop(paste0("O historico de vazao da sub-bacia ", nome_subbacia, " deve terminar na data ", data_fim)))
        })
    }

    if (any(arquivos[, arquivo] == "POSTOS_PLUVIOMETRICOS")) {
        postos_plu <- le_postos_plu(file.path(pasta_entrada, arquivos[arquivo == "POSTOS_PLUVIOMETRICOS", nome_arquivo]))[nome %in% sub_bacias$nome]
        if (!all(sub_bacias$nome %in% postos_plu$nome)) {
            stop(paste0("Falta a sub-bacia ", sub_bacias[!nome %in% postos_plu$nome, nome], " no arquivo ", arquivos[arquivo == "POSTOS_PLUVIOMETRICOS", nome_arquivo], ".\n"))
        }
    } else{
        stop("nao existe o arquivo de postos pluviometricos")
    }

    if (any(arquivos[, arquivo] == "PRECIPITACAO_OBSERVADA")) {
        precipitacao_observada <- le_historico_verificado(file.path(pasta_entrada,arquivos[arquivo == "PRECIPITACAO_OBSERVADA", nome_arquivo]))[posto %in% postos_plu$posto]
        if (!all(sub_bacias$nome %in% postos_plu[precipitacao_observada[, unique(posto)] %in% posto, nome])) {
            stop(paste0("Falta a sub-bacia ", sub_bacias[!nome %in% postos_plu[precipitacao_observada[, unique(posto)] %in% posto, nome], nome], " no arquivo ", arquivos[arquivo == "PRECIPITACAO_OBSERVADA", nome_arquivo], ".\n"))
        }
        lapply(sub_bacias$nome, function(nome_subbacia) {
            kt <- parametros[nome == nome_subbacia & substr(parametro, 1, 2) == "Kt", valor]
            ktMin <- sum(kt[4:63] > 0)
            data_inicio <- datas_rodadas[, min(data)] - inicializacao[nome == nome_subbacia & variavel == "numero_dias_assimilacao", valor] + 1
            data_fim <- datas_rodadas[, max(data)]
            if (precipitacao_observada[, min(data)] > (data_inicio - ktMin))
                stop(paste0("O historico de precipitacao verificada da sub-bacia ", nome_subbacia, " deve comecar na data ", data_inicio - ktMin))
            if (precipitacao_observada[, max(data)] < (data_fim))
                stop(paste0("O historico de precipitacao verificada da sub-bacia ", nome_subbacia, " deve terminar na data ", data_fim + ktMax))
        })
        } else {
        stop("nao existe o arquivo de precipitacao observada")
    }
    
    precipitacao_observada <- data.table::rbindlist(lapply(sub_bacias$nome, function(sub_bacia) {
  ponderacao_espacial(precipitacao_observada, postos_plu[nome %in% sub_bacia])
        }))

    if (any(arquivos[, arquivo] == "PRECIPITACAO_PREVISTA")) {
        precipitacao_prevista <- le_precipitacao_prevista(file.path(pasta_entrada,arquivos[arquivo == "PRECIPITACAO_PREVISTA", nome_arquivo]))
        data.table::setnames(precipitacao_prevista , "nome", "posto")
        if (!all(postos_plu$posto %in% unique(precipitacao_prevista$posto))) {
            stop(paste0("Falta a previsao do posto pluviometrico ", 
            postos_plu[!posto %in% unique(precipitacao_prevista$posto), posto], 
            " no arquivo ", arquivos[arquivo == "PRECIPITACAO_PREVISTA", nome_arquivo], ".\n"))
        }
        precipitacao_prevista <- data.table::rbindlist(lapply(sub_bacias$nome, function(sub_bacia) {
            ponderacao_espacial_previsao(precipitacao_prevista, postos_plu[nome %in% sub_bacia])
        }))
        precipitacao_prevista <- completa_previsao(precipitacao_prevista, datas_rodadas)
        precipitacao_prevista <- precipitacao_prevista[, mean(valor), by = .(data_rodada, data_previsao, cenario, nome)]
        colnames(precipitacao_prevista)[5] <- "valor"
    } else {
        warning("nao existe arquivo de previsao de precipitacao, serao utilizados dados historicos")
        precipitacao_prevista <- data.table::copy(precipitacao_observada)
        colnames(precipitacao_prevista)[2] <- "nome"
        precipitacao_prevista <- transforma_historico_previsao(precipitacao_prevista, datas_rodadas)
    }

    if (any(arquivos[, arquivo] == "EVAPOTRANSPIRACAO_NC")) {

    } else {
        if (any(arquivos[, arquivo] == "EVAPOTRANSPIRACAO_PREVISTA")) {
            evapotranspiracao_prevista <- le_precipitacao_prevista(file.path(pasta_entrada,arquivos[arquivo == "EVAPOTRANSPIRACAO_PREVISTA", nome_arquivo]))
            if (!all(sub_bacias$nome %in% evapotranspiracao_prevista$nome)) {
                stop(paste0("Falta a sub-bacia ", sub_bacias[!nome %in% evapotranspiracao_prevista$nome, nome], " no arquivo ", arquivos[arquivo == "EVAPOTRANSPIRACAO_PREVISTA", nome_arquivo], ".\n"))
            }
        } else {
            warning("nao existe arquivo de previsao de evapotranspiracao, serao utilizados dados historicos")
            
            datas_rodadas[, numero_dias_previsao := (numero_dias_previsao - 2)]
            evapotranspiracao_prevista <- data.table::copy(evapotranspiracao_observada)
            colnames(evapotranspiracao_prevista)[2] <- "nome"
            evapotranspiracao_prevista <- transforma_historico_previsao(evapotranspiracao_prevista, datas_rodadas)
            datas_rodadas[, numero_dias_previsao := (numero_dias_previsao + 2)]
        }
    }

    saida <- list(sub_bacias = sub_bacias, vazao_observada = vazao_observada, evapotranspiracao_observada = evapotranspiracao_observada,
    postos_plu = postos_plu, precipitacao_observada = precipitacao_observada, inicializacao = inicializacao,
    datas_rodadas = datas_rodadas, precipitacao_prevista = precipitacao_prevista, evapotranspiracao_prevista = evapotranspiracao_prevista,
    parametros = parametros, evapotranspiracao_nc = evapotranspiracao_nc)

    saida
}