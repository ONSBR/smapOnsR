#' Escreve arquivos de previsao
#' 
#' Funcao de escrita dos arquivos oficiais de previsao por cenario e por sub-bacia
#' 
#' @param pasta_saida pasta com os arquivos de saida
#' @param previsao data.table contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
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
escreve_previsao <- function(pasta_saida = "arq_saida", previsao) {
    unique_nomes <- unique(previsao$nome)
    unique_cenarios <- unique(previsao$cenario)
    for (nome_subbacia in unique_nomes){
        for (nome_cenario in unique_cenarios) {
            tabela <- previsao[cenario == nome_cenario & nome == nome_subbacia & 
            variavel == "Qcalc", .(data_previsao, valor)]
            tabela[, data_previsao := format(data_previsao, "%d/%m/%Y")]
            tabela[, valor := sprintf("%09.2f", valor)]
            nome_arquivo <- paste0(nome_subbacia, "_", nome_cenario, "_previsao.txt")
            write.table("Data       Qcal", file = file.path(pasta_saida, nome_arquivo), quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)
            write.table(tabela, file = file.path(pasta_saida, nome_arquivo), quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE, append = TRUE)
        }
    }
}

#' Escreve arquivos de ajuste
#' 
#' Funcao de escrita dos arquivos oficiais de ajuste
#' 
#' @param pasta_saida pasta com os arquivos de saida
#' @param previsao data.table contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
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
escreve_ajuste <- function(pasta_saida = "arq_saida", assimilacao) {
    unique_nomes <- unique(assimilacao$nome)
    for (nome_subbacia in unique_nomes) {
        tabela <- assimilacao[nome == nome_subbacia & variavel == "Qbase", .(data_assimilacao)]
        tabela[, TU := assimilacao[nome == nome_subbacia & variavel == "Tu", valor]]
        tabela[, EB := assimilacao[nome == nome_subbacia & variavel == "Qbase", valor]]
        tabela[, SUP := assimilacao[nome == nome_subbacia & (variavel == "Qsup1" | 
        variavel == "Qsup2" | variavel == "Qplan"), sum(valor), by = data_assimilacao]$V1]
        tabela[, Qajustada := assimilacao[nome == nome_subbacia & variavel == "Qcalc", valor]]
        tabela[, data_assimilacao := format(data_assimilacao, "%d/%m/%Y")]
        tabela[, TU := sprintf("%06.2f", TU * 100)]
        tabela[, EB := sprintf("%09.2f", EB)]
        tabela[, SUP := sprintf("%09.2f", SUP)]
        tabela[, Qajustada := sprintf("%09.2f", Qajustada)]
        nome_arquivo <- paste0(nome_subbacia, "_ajuste.txt")
        write.table("Data       TU     EB        SUP       Qajustada", file = file.path(pasta_saida, nome_arquivo), quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE)
        write.table(tabela, file = file.path(pasta_saida, nome_arquivo), quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE, append = TRUE)
    }
}

#cores <- parallel::detectCores()
#cl <- parallel::makeCluster(cores[1] - 1) #not to overload your computer
#doParallel::registerDoParallel(cl)
#parallel::clusterEvalQ(cl, {library("smapOnsR")})
#
#bacias <- list.dirs("inst/extdata/caso_oficial", recursive = FALSE)
#bacias <- c(bacias[1:4], bacias[6:7])
#foreach::foreach(bacia = bacias) %dopar% {
##for (bacia in bacias){
#    dir_bacia <- file.path(bacia, "ARQ_ENTRADA")
#    saida <- executa_caso_oficial(dir_bacia)
#    pasta_saida <- file.path(bacia, "ARQ_SAIDA_R")
#    dir.create(pasta_saida)
#    escreve_ajuste(pasta_saida, saida$assimilacao)
#    escreve_previsao(pasta_saida, saida$previsao)
#}
#parallel::stopCluster(cl)
