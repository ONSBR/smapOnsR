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
