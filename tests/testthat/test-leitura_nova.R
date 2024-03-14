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
