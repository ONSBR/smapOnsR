# LEITURA DO DADO VERIFICADO --------------------------------------------------------
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
#'     \item{nome}{nome do posto}
#'     \item{valor}{valor da NC de evapotranspiracao observada}
#'     }
#' @export
le_evapotranspiracao <- function(arq) {
    dat <- data.table::fread(arq)
    aux <- strsplit(arq, split = "/")[[1]]
    sb <- strsplit(aux[length(aux)], split = "_")[[1]]
    dat$posto <- tolower(sb[1])

    colnames(dat) <- c("mes", "valor", "nome")
    data.table::setcolorder(dat, c("mes", "nome", "valor"))

    dat
}

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
#'     \item{Nome}{Nome da sub-bacia}
#'     \item{parametro}{nome do parametro}
#'     \item{valor}{valor do parametro}
#'     }
#' @export 
le_parametros <- function(arq) {
    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("Nome", "parametro", "valor"))) {
        stop("o arquivos deve deve possuir colunas 'Nome', 'parametro' e 'valor'")
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
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da precipitacao observada}
#'     }
#' @export 

le_historico_verificado <- function(arq) {

    dat <- data.table::fread(arq)
    dat <- dat[stats::complete.cases(dat[, -1])]
    dat[, data := as.Date(data, format = "%d/%m/%Y")]
    
    dat <- data.table::melt(dat, id.vars = "data", variable.name = "posto",
          value.name = "valor")
    colnames(dat)[1] <- "data"

    data.table::setorder(dat, posto, data)
    data.table::setcolorder(dat, c("data", "posto", "valor"))
    dat[, posto := tolower(posto)]
    return(dat)
}

#' Le csv com dados de inicializacao do caso
#' 
#' Realiza a leitura do csv 'inicializacao.csv' com os dados iniciais
#' 
#' @param arq nome do arquivo "inicializacao.csv"
#' @importFrom  data.table data.table
#' @return data.table com a inicializacao com as colunas
#'     \itemize{
#'     \item{nome}{nome da sub_bacia}
#'     \item{variavel}{nome da variavel}
#'     \item{valor}{valor da variavel}
#'     }
#' @export 
le_inicializacao <- function(arq) {
    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("nome", "variavel", "valor"))) {
        stop("o arquivos deve possuir colunas 'nome', 'variavel', 'valor'")
    }

    dat[, nome := tolower(nome)]

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
#'     \item{data}{data do caso}
#'     \item{numero_dias_previsao}{horizonte do caso}
#'     }
#' @export 
le_datas_rodada <- function(arq) {
    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("data", "numero_dias_previsao"))) {
        stop("o arquivos deve deve possuir colunas 'data' e 'numero_dias_previsao'")
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
#'     \item{data}{data do caso}
#'     \item{numero_dias_previsao}{horizonte do caso}
#'     }
#' @export 
le_arquivos <- function(pasta_entrada) {

    arq <- file.path(pasta_entrada, "arquivos.csv")

    if (!file.exists(arq)) {
        stop("nao existe o arquivo do tipo arquivos.csv")
    }

    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("arquivo", "nome_arquivo"))) {
        stop("o arquivos deve deve possuir colunas 'arquivo' e 'nome_arquivo'")
    }

    dat
}

#' Le csv com a relaca de postos plu x sub_bacia
#' 
#' Realiza a leitura do csv 'postos_plu.csv' csv com a relacao de postos plu x sub_bacia
#' 
#' @param arq nome do arquivo contendo a relacao de postos plu x sub-bacia
#' @importFrom  data.table fread
#' @return data table contendo as datas dos casos a serem executados e seus respectivos horizontes:
#'     \itemize{
#'     \item{data}{data do caso}
#'     \item{numero_dias_previsao}{horizonte do caso}
#'     }
#' @export 
le_postos_plu <- function(arq) {

    if (!file.exists(arq)) {
        stop("nao existe o arquivo do tipo arquivos.csv")
    }

    dat <- data.table::fread(arq)

    if (any(colnames(dat) != c("nome", "posto", "valor"))) {
        stop("o arquivos deve deve possuir colunas 'nome', 'posto' e 'valor'")
    }

    dat[, nome := tolower(nome)]

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
le_arq_entrada_novo <- function(pasta_entrada){
    arquivos <- le_arquivos(pasta_entrada)

    sub_bacias <- data.table::fread(file.path(pasta_entrada,arquivos[arquivo == "SUB_BACIAS", nome_arquivo]))

    parametros <- le_parametros(file.path(pasta_entrada,arquivos[arquivo == "PARAMETROS", nome_arquivo]))[Nome %in% sub_bacias$nome]

    vazao_observada <- le_historico_verificado(file.path(pasta_entrada,arquivos[arquivo == "VAZAO_OBSERVADA", nome_arquivo]))[posto %in% sub_bacias$nome]

    evapotranspiracao_observada <- le_historico_verificado(file.path(pasta_entrada,arquivos[arquivo == "EVAPOTRANSPIRACAO_OBSERVADA", nome_arquivo]))[posto %in% sub_bacias$nome]

    postos_plu <- le_postos_plu(file.path(pasta_entrada, arquivos[arquivo == "POSTOS_PLUVIOMETRICOS", nome_arquivo]))

    precipitacao_observada <- le_historico_verificado(file.path(pasta_entrada,arquivos[arquivo == "PRECIPITACAO_OBSERVADA", nome_arquivo]))[posto %in% postos_plu$posto]

    inicializacao <-  le_inicializacao(file.path(pasta_entrada,arquivos[arquivo == "INICIALIZACAO", nome_arquivo]))[nome %in% sub_bacias$nome]

    datas_rodadas <- le_datas_rodada(file.path(pasta_entrada,arquivos[arquivo == "DATAS_RODADAS", nome_arquivo]))

    precipitacao_observada <- data.table::rbindlist(lapply(sub_bacias$nome, function(sub_bacia) {
  ponderacao_espacial(precipitacao_observada, postos_plu[nome %in% sub_bacia])
        }))

    precipitacao_prevista <- data.table::copy(precipitacao_observada)
    colnames(precipitacao_prevista)[2] <- "nome"
    precipitacao_prevista <- transforma_historico_previsao(precipitacao_prevista, datas_rodadas)

    datas_rodadas[, numero_dias_previsao := (numero_dias_previsao - 2)]
    evapotranspiracao_prevista <- data.table::copy(evapotranspiracao_observada)
    colnames(evapotranspiracao_prevista)[2] <- "nome"
    evapotranspiracao_prevista <- transforma_historico_previsao(evapotranspiracao_prevista, datas_rodadas)
    datas_rodadas[, numero_dias_previsao := (numero_dias_previsao + 2)]

    saida <- list(sub_bacias = sub_bacias, vazao_observada = vazao_observada, evapotranspiracao_observada = evapotranspiracao_observada,
    postos_plu = postos_plu, precipitacao_observada = precipitacao_observada, inicializacao = inicializacao,
    datas_rodadas = datas_rodadas, precipitacao_prevista = precipitacao_prevista, evapotranspiracao_prevista = evapotranspiracao_prevista,
    parametros = parametros)

    saida
}