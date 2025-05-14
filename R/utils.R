#' transforma_NC_serie
#' 
#' transforma normal climatologica de evapotranspiracao em uma serie temporal
#' 
#' 
#' @param serie_temporal serie temporal a ser utilizada como espelho
#' @param normal_climatologica data.table com NC de evapotranspiracao com as colunas
#'     \itemize{
#'     \item{mes - mes da NC}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da NC de evapotranspiracao observada}
#'     }
#' @importFrom data.table data.table setnames setorder
#' @importFrom lubridate month
#' @examples
#' nome2 <- "baixoig"
#' normal_climatologica <- historico_etp_NC[nome == nome2]
#' serie_temporal <- historico_precipitacao[posto == postos_plu[nome == nome2, posto]]
#' serie_temporal <- ponderacao_espacial(serie_temporal, postos_plu[nome == nome2])
#' serie_temporal_NC <- transforma_NC_serie(serie_temporal, normal_climatologica)
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{valor - valor da serie temporal}
#'     }
#' @export 
transforma_NC_serie <- function(serie_temporal, normal_climatologica) {
    serie_temporal_etp <- data.table::data.table(serie_temporal)
    serie_temporal_etp[, mes := lubridate::month(data)]
    serie_temporal_etp <- merge(serie_temporal_etp, normal_climatologica, by = "mes")
    serie_temporal_etp[, nome.x := NULL]
    serie_temporal_etp[, valor.x := NULL]
    serie_temporal_etp[, mes := NULL]

    data.table::setnames(serie_temporal_etp, old = c("nome.y", "valor.y"), new = c("nome", "valor"))
    data.table::setorder(serie_temporal_etp, nome, data)
    serie_temporal_etp
}


#' Transforma historico de serie temporal em data table de previsao
#'
#' Transforma historico de serie temporal em data table de previsao
#'
#' @param serie_temporal data.table com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da serie_temporal observada}
#'     }
#' @param datas_rodadas data.table com as colunas
#'     \itemize{
#'     \item{data - data da rodada}
#'     \item{numero_dias_previsao - numero de dias de previsao}
#'      }
#' @importFrom data.table data.table setcolorder setorder
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{valor - valor da previsao}
#'     }
#' @examples 
#' sub_bacia <- "avermelha"
#' precipitacao <- historico_precipitacao[posto %in% postos_plu[nome == sub_bacia, posto]]
#' precipitacao <- ponderacao_espacial(precipitacao, postos_plu[nome == sub_bacia])
#' datas_rodadas <- data.table::data.table(
#'   data = as.Date(c("2020-05-01", "2020-05-08")),
#'   numero_dias_previsao = c(15, 20)
#' )
#' 
#' previsao <- transforma_historico_previsao(precipitacao, datas_rodadas)
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

        data_previsao <- serie_temporal[data %in% seq.Date(data_rodada + 1, data_rodada + numero_dias_previsao + 2, by = 1)]
        aux <- serie_temporal[data %in% data_previsao[, data]]
        colnames(aux)[1] <- "data_previsao"
        aux[, data_rodada := data_rodada]
        aux[, cenario := "historico"]

        previsao <- data.table::rbindlist(list(previsao, aux))
    }
    data.table::setcolorder(aux, c("data_rodada", "data_previsao", "cenario", "nome", "valor"))
    data.table::setorder(previsao, data_rodada, nome, data_previsao)
    return(previsao)
}

#' Combina data table observado e previsto
#'
#' Combina data table observado e previsto
#'
#' @param observado data.table com as colunas
#'     \itemize{
#'     \item{data - data da observacao}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da serie_temporal observada}
#'     }
#' @param previsto data.table com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da previsao}
#'     }
#' @importFrom data.table data.table setcolorder setorder
#' @return serie_temporal data.table com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{posto - nome do posto}
#'     \item{id - id do posto}
#'     \item{valor - valor da previsao}
#'     }
#' @export
combina_observacao_previsao <- function(observado, previsto){

    result <- lapply(unique(previsto$cenario), function(cenario) {
        dt <- data.table::copy(observado)
        dt[, cenario := cenario]
        return(dt)
    })
    serie_temporal <- data.table::rbindlist(result)
    colnames(serie_temporal)[1] <- "data_previsao"

    serie_temporal <- rbind(serie_temporal, previsto)

    data.table::setorder(serie_temporal, data_rodada, cenario, data_previsao)
    serie_temporal
}


#' Completa as previsoes com mais n dias
#'
#' Completa o data table de precisoes com mais N dias, para a ponderacao temporal ate t+2
#'
#' @param datas_rodadas data table com as colunas
#'     \itemize{
#'     \item{data - data da rodada}
#'     \item{numero_dias_previsao - numero de dias de previsao}
#'     }
#' @param precipitacao_prevista data.table com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{valor - valor da previsao}
#'     }
#' @param numero_dias numero de dias alem da data final da previsao a ser verificado
#' @importFrom data.table data.table setcolorder setorder setnames CJ
#' @return precipitacao_prevista data.table com as colunas
#'     \itemize{
#'     \item{data_rodada - data da rodada}
#'     \item{data_previsao - data da previsao}
#'     \item{cenario - nome do cenario}
#'     \item{nome - nome da sub-bacia}
#'     \item{valor - valor da previsao}
#'     }
#' @export
completa_previsao <- function(precipitacao_prevista, datas_rodadas, numero_dias = 2){
    result_list <- list()
    for (i in seq_len(nrow(datas_rodadas))) {
        current_data_rodada <- datas_rodadas[i, ]
        if (any(precipitacao_prevista[data_rodada == current_data_rodada$data, 
                                      as.numeric(max(data_previsao) - (current_data_rodada$data + 
                                                                      current_data_rodada$numero_dias_previsao - 1 + 
                                                                      numero_dias)), by = .(nome, cenario)]$V1 <= 
                0)) {
          mean_values <- precipitacao_prevista[, .(mean_valor = mean(valor)), 
                                              by = .(nome, data_rodada, cenario)]
          unique_combinations <- unique(precipitacao_prevista[data_rodada == 
                                                                current_data_rodada$data, .(nome, data_rodada, 
                                                                                            cenario)])
          datas <- seq.Date(current_data_rodada$data + 1, 
                            current_data_rodada$data + current_data_rodada$numero_dias_previsao + 
                              numero_dias, by = 1)
          all_combinations <- data.table::CJ(nome = unique(unique_combinations$nome), 
                                            cenario = unique(unique_combinations$cenario), data_previsao = datas)
          missing_forecasts <- all_combinations[!precipitacao_prevista, 
                                                on = c("nome", "cenario", "data_previsao")]
          missing_forecasts <- mean_values[missing_forecasts, 
                                          on = .(nome, cenario)]
          missing_forecasts[, `:=`(data_rodada, current_data_rodada$data)]
          data.table::setnames(missing_forecasts, "mean_valor", 
                              "valor")
          missing_forecasts <- unique(missing_forecasts)
          data.table::setcolorder(missing_forecasts, c("data_rodada", 
                                                      "data_previsao", "cenario", "nome", "valor"))
          result_list[[i]] <- missing_forecasts
          precipitacao_prevista <- data.table::rbindlist(list(precipitacao_prevista, 
                                                              result_list[[i]]), use.names = TRUE)
        }
    }
    data.table::setorder(precipitacao_prevista, nome, data_rodada, 
                        data_previsao, cenario)
    precipitacao_prevista
}

#' Ordem de afluencia
#'
#' @param montante Nomes ou IDs das UHEs de montante da cascata
#' @param jusante Nomes ou IDs das UHEs imediatamente a jusante de cada UHE
#'
#' @return Retorna a ordem de afluencia para cada UHE
#' @export
ordem_afluencia <- function(montante, jusante) {
  # Vetor com ordens de afluencia
  ord_aflu <- rep(NA, length(montante))
  
  # UHES qua nao sao jusantes de nenhuma outra (nao tem montantes)
  tmp <- which(!is.element(montante,jusante))
  ord <- 1
  ord_aflu[tmp] <- ord
  
  # Ate preencher as ordens de todos montante
  while (any(is.na(ord_aflu))) {
    ord <- ord + 1
    uhe_c_ord <- montante[!is.na(ord_aflu)]
    uhe_s_ord <- montante[is.na(ord_aflu)]
    
    for(u in uhe_s_ord){
      # UHEs imediatamente a montande de u
      m.u <- montante[is.element(jusante, u)]
      
      # Se todos os montantes imediatos ja tiverem ordem determinada, u recebe a ordem seguinte
      if (all(is.element(m.u, uhe_c_ord))) {
        tmp <- which(montante == u)
        ord_aflu[tmp] <- ord
      }
    }
  }
  return(ord_aflu)
}

#' Cria arquivo de datas a serem simuladas
#'
#' @param data_inicio data inicio da simulacao
#' @param data_fim data fim da simulacao
#'
#' @return data table contendo as datas dos casos a serem executados e seus respectivos horizontes:
#'     \itemize{
#'     \item{data - data do caso}
#'     \item{numero_dias_previsao - horizonte do caso}
#'     }
#' @importFrom data.table data.table
#' @export

cria_datas <- function(data_inicio, data_fim, numero_dias_previsao = 42) {

  # 1) Calcular quantos dias até a próxima quinta-feira (4 = quinta em ISO-8601: domingo=7, segunda=1, ..., sábado=6)
  dias_ate_quinta <- (4 - as.integer(format(data_inicio, "%u"))) %% 7
  primeira_quinta <- data_inicio + dias_ate_quinta

  # 2) Gerar sequência de quintas até a data_fim
  if (numero_dias_previsao > 42) {
    intervalo <- "1 month"
  } else {
    intervalo <- "1 week"
  }
  datas_quinta <- seq(from = primeira_quinta, to = data_fim, by = intervalo)

  # 3) Montar o data.table com numero_dias_previsao = 42
  datas <- data.table::data.table(
    data = datas_quinta,
    numero_dias_previsao = rep(numero_dias_previsao, length(datas_quinta))
  )

  datas
}

#' Cria arquivo de inicializacao
#'
#' @param parametros (opcional) data.table com as colunas
#'     \itemize{
#'     \item{nome - nome da sub-bacia}
#'     \item{parametro - nome do parametro}
#'     \item{valor - valor do parametro}
#'     }
#' @param nome   (opcional) vetor de nomes (mesmo comprimento que Ebin, Supin, Tuin)
#' @param Ebin   (opcional) vetor de valores para Ebin
#' @param Supin  (opcional) vetor de valores para Supin
#' @param Tuin   (opcional) vetor de valores para Tuin
#' @param numero_dias_assimilacao (integer) valor fixo (padrao = 32)
#' @param ajusta_precipitacao     (integer) valor fixo (padrao = 1)
#' @return data.table com a inicializacao com as colunas
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     \item{variavel - nome da variavel}
#'     \item{valor - valor da variavel}
#'     }
#' @importFrom data.table setorder rbindlist
#' @export

cria_inicializacao <- function(parametros = NULL,
                              nome  = NULL,
                              Ebin = NULL,
                              Supin = NULL,
                              Tuin = NULL, 
                              limite_inferior_ebin = 0.8,
                              limite_superior_ebin = 1.2,
                              limite_inferior_prec = 0.5,
                              limite_superior_prec = 2,
                              numero_dias_assimilacao = 32L,
                              ajusta_precipitacao = 1L) {

  # 1) Monta dt_sel a partir de data.table ou vetores
  if (is.null(parametros)) {
    if (any(sapply(list(nome, Ebin, Supin, Tuin), is.null))) {
      stop("Se 'parametros' for NULL, fornecer 'nome', 'Ebin', 'Supin' e 'Tuin'.")
    }
    if (! (length(nome)==length(Ebin) &&
           length(nome)==length(Supin) &&
           length(nome)==length(Tuin)) ) {
      stop("'nome', 'Ebin', 'Supin' e 'Tuin' devem ter mesmo comprimento.")
    }
    # Cria uma data.table ampla e depois derrete
    dt_base <- data.table(
      nome  = nome,
      Ebin  = Ebin,
      Supin = Supin,
      Tuin  = Tuin
    )
    dt_sel <- melt(
      dt_base,
      id.vars       = "nome",
      measure.vars  = c("Ebin","Supin","Tuin"),
      variable.name = "variavel",
      value.name    = "valor"
    )
  } else {
    if (!all(c("nome","parametro","valor") %in% names(parametros))) {
      stop("O data.table 'parametros' deve ter colunas 'nome','parametro','valor'.")
    }
    dt_sel <- parametros[
      parametro %chin% c("Ebin","Supin","Tuin"),
      .(nome, variavel = parametro, valor)
    ]
  }
  
  # 2) Parâmetros fixos (6 por nome)
  dt_fixos <- unique(dt_sel[, .(nome)])[ , .(
    variavel = c(
      "numero_dias_assimilacao",
      "ajusta_precipitacao",
      "limite_inferior_ebin",
      "limite_superior_ebin",
      "limite_inferior_prec",
      "limite_superior_prec"
    ),
    valor = c(
      numero_dias_assimilacao,
      ajusta_precipitacao,
      limite_inferior_ebin,
      limite_superior_ebin,
      limite_inferior_prec,
      limite_superior_prec
    )
  ), by = nome ]
  
  # 3) Empilha e ordena
  inicializacao <- rbindlist(list(dt_sel, dt_fixos), use.names = TRUE)
  setorder(inicializacao, nome, variavel)
  
  return(inicializacao)
}

#' Cria arquivo de sub_bacias
#'
#' @param parametros data.table com as colunas
#'     \itemize{
#'     \item{nome - nome da sub-bacia}
#'     \item{parametro - nome do parametro}
#'     \item{valor - valor do parametro}
#'     }
#'
#' @return data.table sub_bacia com as colunas
#'     \itemize{
#'     \item{nome - nome da sub_bacia}
#'     }
#' @export

cria_sub_bacias <- function(parametros) {

  sub_bacias <- data.table::data.table(
    nome = unique(parametros$nome)
  )

  sub_bacias
}

#' Agrega previsao semanal
#' 
#' Realiza agregacao semanal da previsao e observacao levando em 
#' consideracao semanas operativa de sabado a sexta
#' 
#' @param simulacao data table com a previsao contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{nome}{nome da sub-bacia}
#'     \item{variavel}{nome da variavel}
#'     \item{valor}{valor da variavel}
#'     }
#' @param observacao data table com o historico de vazao com as colunas:
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' 
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data_caso}{data da rodada}
#'     \item{nome}{nome da sub-bacia}
#'     \item{previsao}{ valor da previsao semanal}
#'     \item{observacao}{valor observacao semanal}
#'     \item{discretizacao}{discretizacao da previsao}
#'     \item{horizonte}{horizonte da previsao}
#'     }
#' 
#' @export

agrega_semanal <- function(simulacao, observacao) {
    # Step 1: Merge the two data.tables by "data"
    colnames(simulacao)[2] <- "data"
    merged_data <- merge(simulacao, observacao, by = "data")
    data.table::setorder(merged_data, data_caso, data)
    merged_data[, dia_semana := lubridate::wday(data)]
    data.table::setDT(merged_data)

    # Step 1: Exclude rows before the first "dia_semana" == 7 for each "data_caso"
    merged_data <- merged_data[, .SD[dia_semana == 7 | (cumsum(dia_semana == 7) > 0)], by = data_caso]

    # Step 2: Create a sequence for each "data_caso" based on the remainder when dividing by 7
    merged_data[, horizonte := rep(1:(.N %/% 7 + (ifelse(.N %% 7 == 0, 0, 1))), each = 7, length.out = .N), by = data_caso]

    # Step 3: Calculate weekly mean and count
    simulacao_semanal <- merged_data[, .(previsao = mean(valor.x), observacao = mean(valor.y), count = .N), by = .(data_caso, horizonte, nome)]
    simulacao_semanal[, count := NULL]
    simulacao_semanal[, discretizacao := "semanal"]
    data.table::setcolorder(simulacao_semanal, c("data_caso", "nome", "previsao", 
                  "observacao", "discretizacao", "horizonte"))
    simulacao_semanal
}

#' Agrega previsao mensal
#' 
#' Realiza agregacao mensal da previsao e observacao
#' 
#' @param simulacao data table com a previsao contendo as seguintes colunas:
#'     \itemize{
#'     \item{data_caso}{data da rodada}
#'     \item{data_previsao}{data da previsao}
#'     \item{cenario}{nome do cenario}
#'     \item{nome}{nome da sub-bacia}
#'     \item{variavel}{nome da variavel}
#'     \item{valor}{valor da variavel}
#'     }
#' @param observacao data table com o historico de vazao com as colunas:
#'     \itemize{
#'     \item{data}{data da observacao}
#'     \item{posto}{nome do posto}
#'     \item{valor}{valor da variavel}
#'     }
#' 
#' @return data.table com as colunas
#'     \itemize{
#'     \item{data_caso}{data da rodada}
#'     \item{nome}{nome da sub-bacia}
#'     \item{previsao}{ valor da previsao semanal}
#'     \item{observacao}{valor observacao semanal}
#'     \item{discretizacao}{discretizacao da previsao}
#'     \item{horizonte}{horizonte da previsao}
#'     }
#' 
#' @export
#' 

agrega_mensal <- function(simulacao, observacao) {
    # 2) Atribui a cada linha o “mes” a que ela pertence (blocos de 30 dias)
    #    horizonte 0–29 mes 1, 30–59  mes 2, etc.
    simulacao[, horizonte := as.integer(data.table::as.IDate(data_previsao) - 
              data.table::as.IDate(data_caso))]
    simulacao[, horizonte := floor(horizonte / 30) + 1]

    # 3) Prepare observação do mesmo modo
    observacao[, data := data.table::as.IDate(data)]  # se for IDate

    # 4) Merge previsão + observação
    dt <- merge(
    simulacao[, .(data_caso, data_previsao, nome, prev = valor, horizonte)],
    observacao[, .(data, nome = posto, obs = valor)],
    by.x = c("data_previsao","nome"),
    by.y = c("data","nome"),
    all.x = TRUE
    )

    # 5) Agrega média dentro de cada bloco de 30 dias
    simulacao_mensal <- dt[
    , .( 
        previsao = mean(prev, na.rm=TRUE),
        observacao  = mean(obs,  na.rm=TRUE)
        ),
    by = .(nome, data_caso, horizonte)
    ]
    data.table::setcolorder(simulacao_mensal, 
        c("data_caso", "horizonte", "nome", 
        "previsao", "observacao"))
    simulacao_mensal[, discretizacao := "mensal"]
    data.table::setcolorder(simulacao_mensal, c("data_caso", "nome", "previsao", 
                  "observacao", "discretizacao", "horizonte"))
    simulacao_mensal
}
