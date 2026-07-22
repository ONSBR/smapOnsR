test_that("testa arquivo arquivos.csv", {
    dir.create(file.path(system.file("extdata", package = "smapOnsR"), "validacao_arq_entrada_novo"))
    zip::unzip(system.file("extdata", "validacao_arq_entrada_novo.zip", package = "smapOnsR"), exdir = system.file("extdata", "validacao_arq_entrada_novo", package = "smapOnsR"))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN13", "CT13.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))
    
    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN13", "CT13.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN13", "CT13.3", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN13", "CT13.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN13", "CT13.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN13", "CT13.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))
})

test_that("testa arquivo posto_plu.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.1", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_postos_plu(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.2", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.3", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.4", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.5", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.6", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.7", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.8", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.9", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.10", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.11", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN14", "CT14.12", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))
})

test_that("testa datasRodadas.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN15", "CT15.1", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN15", "CT15.2", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN15", "CT15.3", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN15", "CT15.4", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_datas_rodada(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN15", "CT15.5", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN15", "CT15.6", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN15", "CT15.7", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN15", "CT15.8", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN15", "CT15.9", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))
})

test_that("testa inicializacao.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.1", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.2", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.3", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.4", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_inicializacao(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.5", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.6", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.7", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.8", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.9", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.10", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.11", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN16", "CT16.12", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))
})

# Helper: monta um data.table completo de inicializacao com valores padrão válidos
monta_dt_inicial <- function(nome, mes = NULL, valor_base = 1) {
  vars <- data.table::data.table(
    variavel = c("Ebin", "Supin", "Tuin", "numero_dias_assimilacao",
                 "limite_inferior_ebin", "limite_superior_ebin",
                 "limite_inferior_prec", "limite_superior_prec",
                 "funcao_objetivo", "ajusta_precipitacao"),
    valor = c(0.5, 0.5, 50, 2, 0.8, 1.2, 0.5, 2, 0, 1)
  )
  dt <- data.table::data.table(
    nome = nome,
    variavel = vars$variavel,
    valor = vars$valor
  )
  if (!is.null(mes)) dt[, mes := mes]
  dt
}

# 1) Expansão quando mes ausente (NULL) -> mes = 0 internamente -> expande 1:12

test_that("Expande mes ausente para todos os meses mantendo validações globais", {
  # Cria CSV sem coluna mes
  tmp <- tempfile(fileext = ".csv")
  dt_input <- monta_dt_inicial("B1")
  data.table::fwrite(dt_input, tmp)

  res <- le_inicializacao(tmp)
  # Deve conter 10 variáveis x 12 meses
  expect_equal(nrow(res), 4 * 12 + 6)
  # Verifica cobertura de meses 1:12 em cada variável
  meses_res <- res[variavel == "limite_superior_ebin", sort(unique(mes)), by = variavel]
  expect_true(all(meses_res$V1 == 1:12))
  # Verifica que valor de ajusta_precipitacao ==1 para todos os meses
  expect_true(all(res[variavel == "ajusta_precipitacao"]$valor == 1))
})

# 2) Preserva mes explícito sem duplicar e sem expansion

test_that("Preserva mes explícito quando fornecido para todas variáveis", {
  tmp <- tempfile(fileext = ".csv")
  # Meses distintos para teste: 1,2,3
  dt_input <- monta_dt_inicial("B2", mes = 1)
  dt_input2 <- monta_dt_inicial("B2", mes = 2)
  dt_input3 <- monta_dt_inicial("B2", mes = 3)
  dt_all <- rbind(dt_input, dt_input2, dt_input3)
  data.table::fwrite(dt_all, tmp)

  res <- le_inicializacao(tmp)
  # Deve manter 30 linhas (10 variáveis x3 meses)
  expect_equal(nrow(res), 10 * 3)
  # Valores de 'mes' devem ser exatamente os fornecidos
  expect_setequal(res$mes, c(1,2,3))
})

# 3) Erro para cobertura parcial de meses explícitos

test_that("Erro se meses explícitos não cobrem 1:12", {
  tmp <- tempfile(fileext = ".csv")
  dt_input1 <- monta_dt_inicial("B3", mes = 1)
  dt_input2 <- monta_dt_inicial("B3", mes = 2)
  # falta demais meses
  dt_partial <- dt_input1[variavel == "Ebin" | variavel == "Tuin" | variavel == "Supin" | variavel == "numero_dias_assimilacao"]
  # dt_partial apenas 4 variáveis x 2 meses
  data.table::fwrite(dt_partial, tmp)

  expect_error(
    le_inicializacao(tmp)
  )
})

# 5) Arquivo inexistente gera erro imediatamente

test_that("Erro ao chamar com arquivo inexistente", {
  expect_error(le_inicializacao("inexistente.csv"), regexp = "nao existe")
})


test_that("testa parametros.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.1", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.2", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_parametros(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.3", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.4", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.5", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.6", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.7", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.8", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.9", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arq_entrada_novo(pasta_entrada))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.10", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.11", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN17", "CT17.12", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))
})

test_that("testa precipitacao_prevista.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.1", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.2", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_precipitacao_prevista(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.3", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.4", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_precipitacao_prevista(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.5", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_precipitacao_prevista(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.6", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.7", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.8", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.9", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.10", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.11", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_precipitacao_prevista(arq)))

    arq_prec <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.12", "precipitacao_prevista.csv", package = "smapOnsR")
    arq_etp <- system.file("extdata", "validacao_arq_entrada_novo", "CN18", "CT18.12", "evapotranspiracao_prevista.csv", package = "smapOnsR")
    precipitacao_prevista <- data.table::fread(arq_prec)
    evapotranspiracao_prevista <- data.table::fread(arq_etp)
    expect_error(valida_cenarios(evapotranspiracao_prevista, precipitacao_prevista))
})

# Helper: trunca a previsao ao limite (data + numero_dias_previsao - 1) de cada data_rodada,
# espelhando o truncamento aplicado em le_arq_entrada_novo (R/leitura_nova.R) tanto na
# precipitacao quanto na etp previstas antes de chamar valida_previsao_etp.
trunca_horizonte_previsao <- function(previsao, datas_rodadas) {
    limites_previsao <- datas_rodadas[, .(data_rodada = data,
        data_limite = data + numero_dias_previsao - 1)]
    previsao[limites_previsao, on = "data_rodada"][data_previsao <= data_limite,
        .(data_rodada, data_previsao, cenario, nome, valor)]
}

test_that("trunca a previsao ao horizonte de cada data_rodada respeitando multiplas datas de rodada", {
    # Reproduz a passagem de variaveis para valida_previsao_etp em le_arq_entrada_novo com DUAS
    # datas de rodada de horizontes diferentes. A precipitacao prevista e completada com 2 dias
    # alem do horizonte (completa_previsao, numero_dias = 2) e a etp para exatamente no horizonte
    # (numero_dias = 0). O truncamento precisa respeitar o limite de CADA data_rodada
    # individualmente: um corte global pela maior data limite nao removeria os dias extras da
    # rodada de horizonte menor, mantendo o erro de validacao.
    nome_bacia <- "bacia_teste"
    cenarios <- c("cenario_1", "cenario_2")

    data_rodada_a <- as.Date("2021-12-01")
    n_dias_a <- 3
    data_rodada_b <- as.Date("2021-12-10")
    n_dias_b <- 7

    limite_a <- data_rodada_a + n_dias_a - 1   # 2021-12-03
    limite_b <- data_rodada_b + n_dias_b - 1   # 2021-12-16

    datas_rodadas <- data.table::data.table(
        data = c(data_rodada_a, data_rodada_b),
        numero_dias_previsao = as.numeric(c(n_dias_a, n_dias_b)))

    # previsao crua de cada rodada: de data_rodada + 1 ate o limite do horizonte. As datas das
    # duas rodadas sao disjuntas e o valor e constante para manter deterministica a media usada
    # por completa_previsao ao preencher os dias faltantes.
    constroi_crua <- function(data_rodada, n_dias) {
        datas_previsao <- seq.Date(data_rodada + 1, data_rodada + n_dias - 1, by = 1)
        data.table::CJ(data_rodada = data_rodada, data_previsao = datas_previsao,
            nome = nome_bacia, cenario = cenarios)
    }
    previsao_crua <- data.table::rbindlist(list(
        constroi_crua(data_rodada_a, n_dias_a),
        constroi_crua(data_rodada_b, n_dias_b)))
    data.table::setcolorder(previsao_crua, c("data_rodada", "data_previsao", "nome", "cenario"))

    precipitacao_crua <- data.table::copy(previsao_crua)[, valor := 10]
    etp_crua <- data.table::copy(previsao_crua)[, valor := 3]

    precipitacao_prevista <- completa_previsao(precipitacao_crua, datas_rodadas, numero_dias = 2)
    colnames(precipitacao_prevista)[5] <- "valor"
    evapotranspiracao_prevista <- completa_previsao(etp_crua, datas_rodadas, numero_dias = 0)

    # a precipitacao completada ultrapassa o horizonte de cada rodada; a etp para no horizonte
    expect_equal(precipitacao_prevista[data_rodada == data_rodada_a, max(data_previsao)], limite_a + 2)
    expect_equal(precipitacao_prevista[data_rodada == data_rodada_b, max(data_previsao)], limite_b + 2)
    expect_equal(evapotranspiracao_prevista[data_rodada == data_rodada_a, max(data_previsao)], limite_a)
    expect_equal(evapotranspiracao_prevista[data_rodada == data_rodada_b, max(data_previsao)], limite_b)

    # sem nenhum truncamento, valida_previsao_etp acusa falta de cenario na etp para os dias extras
    expect_error(valida_previsao_etp(evapotranspiracao_prevista, precipitacao_prevista),
        regexp = "evapotranspiracao")

    # um corte GLOBAL pela maior data limite nao corrige a rodada de horizonte menor: os dias
    # extras da rodada A (apos limite_a) sao <= limite_b, permaneceriam e o erro persistiria.
    precipitacao_corte_global <- precipitacao_prevista[data_previsao <= max(limite_a, limite_b)]
    expect_true(precipitacao_corte_global[data_rodada == data_rodada_a & data_previsao > limite_a, .N] > 0)
    expect_error(valida_previsao_etp(evapotranspiracao_prevista, precipitacao_corte_global),
        regexp = "evapotranspiracao")

    # truncamento por data_rodada aplicado em le_arq_entrada_novo (precip e etp)
    precipitacao_prevista_horizonte <- trunca_horizonte_previsao(precipitacao_prevista, datas_rodadas)
    evapotranspiracao_prevista_horizonte <- trunca_horizonte_previsao(evapotranspiracao_prevista, datas_rodadas)

    # cada data_rodada e cortada no SEU proprio limite, e nao por um limite global
    maximos <- precipitacao_prevista_horizonte[, .(max_data = max(data_previsao)), by = data_rodada]
    data.table::setorder(maximos, data_rodada)
    expect_equal(maximos$max_data, c(limite_a, limite_b))

    # os dias extras da rodada A foram removidos; demais dias e cenarios de cada rodada preservados
    expect_equal(precipitacao_prevista_horizonte[data_rodada == data_rodada_a & data_previsao > limite_a, .N], 0)
    expect_equal(precipitacao_prevista_horizonte[data_rodada == data_rodada_a & cenario == "cenario_1", .N], n_dias_a - 1)
    expect_equal(precipitacao_prevista_horizonte[data_rodada == data_rodada_b & cenario == "cenario_1", .N], n_dias_b - 1)
    expect_setequal(precipitacao_prevista_horizonte$cenario, cenarios)

    # com o truncamento por data_rodada, a validacao passa sem erro
    expect_no_error(valida_previsao_etp(evapotranspiracao_prevista_horizonte, precipitacao_prevista_horizonte))
})

test_that("trunca tambem a etp quando os arquivos de previsao passam do horizonte", {
    # Regressao do caso arq_entrada_pmur: quando os arquivos de previsao (precip e etp) vao ALEM
    # de data + numero_dias_previsao - 1, completa_previsao nao os encurta. Truncar apenas a
    # precipitacao deixaria a etp com dias que a precipitacao nao tem -> valida_previsao_etp
    # falharia pelo lado da precipitacao. O truncamento precisa ser aplicado a AMBAS as previsoes.
    nome_bacia <- "bacia_teste"
    cenarios <- c("cenario_1", "cenario_2")
    data_rodada <- as.Date("2021-12-01")
    numero_dias_previsao <- 5
    limite <- data_rodada + numero_dias_previsao - 1   # 2021-12-05

    datas_rodadas <- data.table::data.table(data = data_rodada,
        numero_dias_previsao = as.numeric(numero_dias_previsao))

    # arquivos crus que se estendem 2 dias ALEM do horizonte (ate limite + 2), em precip e etp
    datas_previsao <- seq.Date(data_rodada + 1, limite + 2, by = 1)
    previsao_crua <- data.table::CJ(data_rodada = data_rodada, data_previsao = datas_previsao,
        nome = nome_bacia, cenario = cenarios)
    data.table::setcolorder(previsao_crua, c("data_rodada", "data_previsao", "nome", "cenario"))
    precipitacao_crua <- data.table::copy(previsao_crua)[, valor := 10]
    etp_crua <- data.table::copy(previsao_crua)[, valor := 3]

    precipitacao_prevista <- completa_previsao(precipitacao_crua, datas_rodadas, numero_dias = 2)
    colnames(precipitacao_prevista)[5] <- "valor"
    evapotranspiracao_prevista <- completa_previsao(etp_crua, datas_rodadas, numero_dias = 0)

    # completa_previsao NAO encurta arquivos longos: ambos seguem ate limite + 2
    expect_equal(precipitacao_prevista[, max(data_previsao)], limite + 2)
    expect_equal(evapotranspiracao_prevista[, max(data_previsao)], limite + 2)

    # truncar APENAS a precipitacao deixa a etp com dias extras -> erro no lado da precipitacao
    precipitacao_prevista_horizonte <- trunca_horizonte_previsao(precipitacao_prevista, datas_rodadas)
    expect_equal(precipitacao_prevista_horizonte[, max(data_previsao)], limite)
    expect_error(valida_previsao_etp(evapotranspiracao_prevista, precipitacao_prevista_horizonte),
        regexp = "no arquivo de previsao de precipitacao")

    # truncar AMBAS (comportamento de le_arq_entrada_novo) alinha as previsoes e a validacao passa
    evapotranspiracao_prevista_horizonte <- trunca_horizonte_previsao(evapotranspiracao_prevista, datas_rodadas)
    expect_equal(evapotranspiracao_prevista_horizonte[, max(data_previsao)], limite)
    expect_no_error(valida_previsao_etp(evapotranspiracao_prevista_horizonte, precipitacao_prevista_horizonte))
})

test_that("testa vazoes.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.1", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.2", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.3", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_historico_verificado(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.4", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.5", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.7", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.8", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.9", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.10", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.11", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN19", "CT19.12", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))
})


test_that("testa evapotranspiracao_nc.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN20", "CT20.1", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(le_evapotranspiracao_nc(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN20", "CT20.2", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(le_evapotranspiracao_nc(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN20", "CT20.3", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(le_evapotranspiracao_nc(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN20", "CT20.4", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_evapotranspiracao_nc(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN20", "CT20.5", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_evapotranspiracao_nc(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN20", "CT20.6", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(le_evapotranspiracao_nc(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN20", "CT20.7", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_evapotranspiracao_nc(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN20", "CT20.8", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(le_evapotranspiracao_nc(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN20", "CT20.9", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN20", "CT20.10", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    unlink(system.file("extdata", "validacao_arq_entrada_novo", package = "smapOnsR"), recursive = TRUE)
})
