#' Le Arquivos _AJUSTE
#' 
#' Leitor de arquivos _AJUSTE da assimilacao de dados
#' 
#' @param arq arquivo AJUSTE a ser lido
#' @param data data de referencia da rodada. Se omitido, sera usada a maior data no arquivo + 1
#' @param subbacia nome da subbacia sendo lida. Se omitido sera inferido do nome do arquivo
#' 
#' @return data.table do arquivo arrumado
#'     \itemize{
#'         \item{dia_assimilacao} numero de dias atras da variavel assimilada com respeito a rodada
#'         \item{tu} variavel TU da assimilacao
#'         \item{escoamento_base} escoamento de base da assimilacao
#'         \item{escoamento_superficial} escoamento superficial da assimilacao
#'         \item{vazao_calculada} vazao calculada da assimilacao
#'         \item{data_previsao} dia referencia de execucao da previsao
#'         \item{subbacia} nome da subbacia da qual estes dados dizem respeito
#' }
#' @export

le_ajuste <- function(arq, data, subbacia) {
    dat <- fread(arq)
    colnames(dat) <- c("dia_assimilacao", "tu", "eb", "sup", "qcalc")
    dat[, dia_assimilacao := as.Date(dia_assimilacao, format = "%d/%m/%Y")]

    if(missing("data")) data <- dat[.N, dia_assimilacao] + 1
    if(missing("subbacia")) subbacia <- sub(".*/", "", sub("_AJUSTE.txt", "", arq))

    dat[, data_previsao := data]
    dat[, dia_assimilacao := as.numeric(data - dia_assimilacao)]
    dat[, subbacia := subbacia]
    dat <- tail(dat, 32)

    return(dat)
}

#' Le Arquivos execucao
#' 
#' Leitor dos arquivos execucao da assimilacao de dados
#' 
#' @param pasta_saida pasta com os arquivos de saida
#' @param data data de referencia da rodada. Se omitido, sera inferido pelo nome do arquivo
#' 
#' @return Lista de dois data.tables: um de assimilacao e outro de previsao. O primeiro de 
#'     assimilacao:
#'     \itemize{
#'         \item{subbacia} nome da subbacia da qual estes dados dizem respeito
#'         \item{data} dia referencia de execucao da assimilacao
#'         \item{dia_assimilacao} numero de dias atras da variavel assimilada com respeito a rodada
#'         \item{qcal} vazao calculada da assimilacao
#'         \item{rsolo} variavel Rsolo da assimilacao
#'         \item{rsub} variavel Rsub da assimilacao
#'         \item{rsup} variavel Rsup da assimilacao
#'         \item{peso_chuva} pesos das precs passadas na assimilacao 
#'         \item{ebin} valor de ebin obtido na assimilacao
#'         \item{supin} valor de supin obtido na assimilacao
#'         \item{funcao_objetivo} valor da funcao objetivo obtida (valores repetidos por subbacia)
#'         \item{data_caso} data do caso
#'     }
#' @export
le_execucao <- function(pasta_saida, data) {
    arq <- list.files(path = pasta_saida, pattern = "execucao.txt", ignore.case = TRUE, full.names = TRUE)
    
    datstr <- readLines(arq)

    ini <- grep("^Sub bacia:", datstr)
    fim <- ini + (ini[2] - ini[1] - 1)

    dats <- mapply(ini, fim, FUN = function(i, f) {
        datstr_i <- datstr[i:f]

        subbacia <- sub("Sub bacia: ", "", datstr_i[1])

        linvars <- grep("^\\[[[:digit:]]+\\]", datstr_i)
        funcao_objetivo <- as.numeric(strsplit(datstr_i[3], ":")[[1]][2])
        ebin <- as.numeric(strsplit(datstr_i[4], ":")[[1]][2])
        supin <- as.numeric(strsplit(datstr_i[7], ":")[[1]][2])
        vars <- datstr_i[linvars]
        vars <- sub("^\\[[[:digit:]]+\\]", "", vars)
        vars <- gsub("[\\(|\\)]", "", vars)
        vars <- strsplit(vars, ",")
        vars <- lapply(vars, as.numeric)
        names(vars) <- datstr_i[linvars - 1]
        names(vars) <- tolower(names(vars))
        names(vars) <- sub(" ", "_", names(vars))
        names(vars) <- trimws(names(vars))

        vars <- data.table::as.data.table(vars[1:5])
        vars[, subbacia := tolower(subbacia)]
        vars[, data_caso := data]
        vars[, ebin := ebin]
        vars[, supin := supin]
        vars[, funcao_objetivo := funcao_objetivo]
        vars[, dia_assimilacao := rev(seq_len(.N))]
        vars <- vars[qcal != 0]
        vars <- vars[, .(subbacia, dia_assimilacao, qcal, rsolo, rsub,
            rsup, peso_chuva, ebin, supin, funcao_objetivo)]
        setcolorder(vars, c("subbacia", "dia_assimilacao", "qcal", "rsolo", "rsub",
            "rsup", "peso_chuva", "ebin", "supin", "funcao_objetivo"))
        
        out <- list(vars)

        return(out)

    }, SIMPLIFY = FALSE)

    dats <- lapply(seq(1), function(i) lapply(dats, "[[", i))
    dats <- lapply(dats, rbindlist)[[1]]

    dats
}

#' Le Arquivos PREVISAO
#' 
#' Leitor dos arquivos de previsao de cada rodada diaria
#' 
#' @param pasta_saida pasta com os arquivos de saida
#' @param data data de referencia da rodada. Se omitido, sera inferido pelo nome do arquivo
#' 
#' @return data.table contendo a dado de previsao arrumado
#'     \itemize{
#'         \item{data_caso} data do caso executado
#'         \item{data_previsao} dia alvo da previsao
#'         \item{cenario} cenario de precipitacao utilizado
#'         \item{nome} nome da subbacia da qual estes dados dizem respeito
#'         \item{variavel} nome da variavel prevista
#'         \item{valor} valor da variavel prevista
#'     }
#' @export
le_previsao <- function(pasta_saida, data) {

    arq <- list.files(path = pasta_saida, pattern = "previsao.txt", ignore.case = TRUE, full.names = TRUE)

    dat <- fread(arq)
    dat <- melt(dat, id.vars = 1:2, variable.name = "data_previsao", value.name = "vazao")
    dat[, data_previsao := as.Date(data_previsao, format = "%d/%m/%Y")]
    dat[, data_caso := data]
    dat[, variavel := "Qcalc"]
    colnames(dat) <- tolower(colnames(dat))
    setcolorder(dat, c("data_caso", "data_previsao", "modelo", "subbacia" , "variavel", "vazao"))
    colnames(dat)[3:4] <- c("cenario", "nome")
    colnames(dat)[6] <- c("valor")
    dat[, nome := tolower(nome)]
    dat[, cenario := tolower(cenario)]
    setorder(dat, nome, data_caso, cenario, data_previsao)

    return(dat)
}
