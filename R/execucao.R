#' Executa caso oficial
#' 
#' executa rodada oficial do modleo SMAP/ONS
#'
#' @param pasta_caso pasta do caso contendo as pastas "Arq_Entrada" e "Arq_Saida"
#' @examples 
#'\dontrun{
#' zip::unzip(system.file("extdata", "dados_entrada.zip", package = "smapOnsR"), exdir = system.file("extdata", package = "smapOnsR"))
#' pasta_entrada <- system.file("extdata", "Arq_Entrada1", package = "smapOnsR")
#' saida <- executa_caso_oficial(pasta_entrada)
#' }
#' @return Escreve os arquivos "sub-bacia_ajuste.txt" e "sub-bacia_cenario_previsao.txt" no formato oficial
#' @export

executa_caso_oficial <- function(pasta_caso){
    pasta_entrada <- file.path(pasta_caso, "Arq_Entrada")
    pasta_saida <- file.path(pasta_caso, "Arq_Saida")
    if (!dir.exists(pasta_entrada)) {
        stop("nao existe a pasta de entrada ", pasta_entrada)
    }
    if (!dir.exists(pasta_saida)) {
        stop("nao existe a pasta de saida ", pasta_saida)
    }
    entrada <- le_arq_entrada(pasta_entrada)

    saida <- rodada_encadeada_oficial(entrada$parametros,
        entrada$inicializacao, entrada$precipitacao, entrada$previsao_precipitacao, 
        entrada$evapotranspiracao, entrada$vazao,
        entrada$postos_plu, entrada$datas_rodadas, entrada$caso$nome_subbacia)

    escreve_previsao(pasta_saida, saida$previsao)
    escreve_ajuste(pasta_saida, saida$assimilacao)
    write.table(saida$otimizacao, file = file.path(pasta_saida, "otimizacao.csv"),
                row.names = FALSE, quote = FALSE, sep = ";")
    write.table(saida$funcao_objetivo, file = file.path(pasta_saida, "funcao_objetivo.csv"),
                row.names = FALSE, quote = FALSE, sep = ";")
}

#' Executa caso oficial com entrada nova
#' 
#' executa rodada oficial do modelo SMAP/ONS com dados de entrada novos
#'
#' @param pasta_entrada caminho da pasta  "arq_entrada"
#' @return saida lista com o data.table previsao contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#'  otimizacao data table com as colunas:
#' \itemize{
#'     \item{otimizacao - valor das variaveis da otimizacao}
#'     \item{nome - nome da sub-bacia}
#'     \item{data_caso - data do caso}
#'     }
#'  assimilacao data table com as colunas:
#' \itemize{
#'     \item{data_caso - data da rodada}
#'     \item{data_assimilacao - data da assimilacao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' precipitacao data table contendo precipitacao observada e previsata com as colunas:
#' \itemize{
#'     \item{data_previsao - data da precipitacao}
#'     \item{data_rodada - data da rodada}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' @export
executa_caso_novo <- function(pasta_entrada){
    entrada <- le_arq_entrada_novo(pasta_entrada)

    if (nrow(entrada$evapotranspiracao_prevista) == 0) {
        saida <- rodada_encadeada_oficial(entrada$parametros,
            entrada$inicializacao, entrada$precipitacao_observada, entrada$precipitacao_prevista, entrada$evapotranspiracao_nc, entrada$vazao,
            entrada$postos_plu, entrada$datas_rodadas, entrada$sub_bacias$nome)
    } else {
        saida <- rodada_encadeada_etp(entrada$parametros,
            entrada$inicializacao, entrada$precipitacao_observada, entrada$precipitacao_prevista, entrada$evapotranspiracao_observada, 
            entrada$evapotranspiracao_prevista, entrada$vazao_observada,
            entrada$postos_plu, entrada$datas_rodadas, entrada$sub_bacias$nome)
    }

    saida
}

#' Executa caso oficial com entrada nova e aprimoramentos
#' 
#' executa rodada oficial do modelo SMAP/ONS com dados de entrada novos
#' e aprimoramentos
#'
#' @param pasta_caso caminho da pasta contendo as pastas "Arq_Entrada" e
#' "Arq_Saida"
#' @export
executa_caso_oficial_v2 <- function(pasta_caso){

    pasta_entrada <- file.path(pasta_caso, "Arq_Entrada")
    pasta_saida <- file.path(pasta_caso, "Arq_Saida")
    if (!dir.exists(pasta_entrada)) {
        stop("nao existe a pasta de entrada ", pasta_entrada)
    }
    if (!dir.exists(pasta_saida)) {
        stop("nao existe a pasta de saida ", pasta_saida)
    }

    entrada <- le_arq_entrada_novo(pasta_entrada)

    if (nrow(entrada$evapotranspiracao_prevista) == 0) {
        saida <- rodada_encadeada_oficial(entrada$parametros,
            entrada$inicializacao, entrada$precipitacao_observada, entrada$precipitacao_prevista,
            entrada$evapotranspiracao_nc, entrada$vazao,
            entrada$postos_plu, entrada$datas_rodadas, entrada$sub_bacias$nome)
    } else {
        saida <- rodada_encadeada_pmur_etp(entrada$parametros,
            entrada$inicializacao, entrada$precipitacao_observada, entrada$precipitacao_prevista,
            entrada$evapotranspiracao_observada,
            entrada$evapotranspiracao_prevista, entrada$vazao_observada,
            entrada$postos_plu, entrada$datas_rodadas, entrada$sub_bacias$nome)
    }

    write.table(saida$previsao, file = file.path(pasta_saida, 
                "previsao.csv"), row.names = FALSE, quote = FALSE, sep = ";")
    write.table(saida$otimizacao, file = file.path(pasta_saida, 
                "otimizacao.csv"), row.names = FALSE, quote = FALSE, sep = ";")
    write.table(saida$funcao_objetivo, file = file.path(pasta_saida, 
                "funcao_objetivo.csv"), row.names = FALSE, quote = FALSE, sep = ";")
    write.table(saida$assimilacao, file = file.path(pasta_saida, 
                "ajuste.csv"), row.names = FALSE, quote = FALSE, sep = ";")
}

#' Executa caso oficial com entrada nova e aprimoramentos
#' 
#' executa rodada oficial do modelo SMAP/ONS com dados de entrada novos
#' e aprimoramentos
#'
#' @param pasta_caso caminho da pasta contendo as pastas "Arq_Entrada" e
#' "Arq_Saida"
#' @export
executa_validacao_calibracao <- function(pasta_caso){

    pasta_entrada <- file.path(pasta_caso, "Arq_Entrada")
    pasta_saida <- file.path(pasta_caso, "Arq_Saida")
    
    if (!dir.exists(pasta_entrada)) {
        stop("nao existe a pasta de entrada ", pasta_entrada)
    }
    if (!dir.exists(pasta_saida)) {
        dir.create(pasta_saida)
    }

    arquivos <- le_arquivos(pasta_entrada)
    arquivo_parametros <- file.path(pasta_entrada,arquivos[arquivo == "PARAMETROS", 
                                    nome_arquivo])

    periodos <- le_datas_calibracao(arquivo_parametros)
    parametros <- le_parametros(arquivo_parametros)

    datas <- cria_datas(periodos[parametro == "data_inicio_simulacao", valor], 
                        periodos[parametro == "data_final_simulacao", valor])
    parametros[parametro == "Tuin", valor := valor * 100]
    inicializacao <- cria_inicializacao(parametros, limite_inferior_ebin = 1, 
                                        limite_superior_ebin = 1)
    arq <- file.path(pasta_entrada, "inicializacao.csv")
    inicializacao <- le_inicializacao(arq)
    vazao <- le_historico_verificado(file.path(pasta_entrada, 
                        arquivos[arquivo == "VAZAO_OBSERVADA", nome_arquivo]))
    data_minima <- max(datas[, min(data)], 
                (min(vazao$data) - 
                inicializacao[variavel == "numero_dias_assimilacao", unique(valor)]))
    data_maxima <- min(datas[, max(data)], 
                (max(vazao$data) - datas[, unique(numero_dias_previsao)]))
    datas <- datas[data >= data_minima & data <= data_maxima]
    sub_bacias <- cria_sub_bacias(parametros)

    write.table(datas, file = file.path(pasta_entrada,
                "datas_rodadas.csv"), row.names = FALSE, quote = FALSE, sep = ";")
    write.table(entrada$inicializacao, file = file.path(pasta_entrada,
                "inicializacao.csv"), row.names = FALSE, quote = FALSE, sep = ";")
    write.table(sub_bacias, file = file.path(pasta_entrada,
                "sub_bacias.csv"), row.names = FALSE, quote = FALSE, sep = ";")

    entrada <- le_arq_entrada_novo(pasta_entrada)

    if (nrow(entrada$evapotranspiracao_prevista) == 0) {
        saida <- rodada_encadeada_oficial(entrada$parametros,
            entrada$inicializacao, entrada$precipitacao_observada, entrada$precipitacao_prevista,
            entrada$evapotranspiracao_nc, entrada$vazao,
            entrada$postos_plu, entrada$datas_rodadas, entrada$sub_bacias$nome)
    } else {
        saida <- rodada_encadeada_pmur_etp(entrada$parametros,
            entrada$inicializacao, entrada$precipitacao_observada, entrada$precipitacao_prevista,
            entrada$evapotranspiracao_observada,
            entrada$evapotranspiracao_prevista, entrada$vazao_observada,
            entrada$postos_plu, entrada$datas_rodadas, entrada$sub_bacias$nome)
    }

    #saida$previsao <- saida$previsao[variavel == "Qcalc"]
    saida$previsao <- saida$previsao[data_caso >= periodos[parametro == "data_inicio_objetivo", valor] &
                    data_caso <= periodos[parametro == "data_final_objetivo",valor]]

    write.table(saida$previsao, file = file.path(pasta_saida, 
                "previsao.csv"), row.names = FALSE, quote = FALSE, sep = ";")
    write.table(saida$assimilacao[variavel == "Qcalc"], file = file.path(pasta_saida, 
                "assimilacao.csv"), row.names = FALSE, quote = FALSE, sep = ";")
    write.table(saida$otimizacao, file = file.path(pasta_saida, 
                "otimizacao.csv"), row.names = FALSE, quote = FALSE, sep = ";")
    write.table(saida$funcao_objetivo, file = file.path(pasta_saida, 
                "funcao_objetivo.csv"), row.names = FALSE, quote = FALSE, sep = ";")
    write.table(saida$precipitacao, file = file.path(pasta_saida, 
                "precipitacao.csv"), row.names = FALSE, quote = FALSE, sep = ";")

    sub_bacias <- unique(saida$previsao$nome)

    avaliacoes <- lapply(sub_bacias, function(sb) {
        previsao  <- saida$previsao[nome == sb & variavel == "Qcalc"]
        obs     <- entrada$vazao_observada[posto == sb]
        analisa_previsoes(previsao, obs)
    })

    # 3) Combina cada componente apenas uma vez
    resultado   <- rbindlist(lapply(avaliacoes, `[[`, "resultado"))

    write.table(resultado, file = file.path(pasta_saida, 
                "validacao.csv"), row.names = FALSE, quote = FALSE, sep = ";")
}