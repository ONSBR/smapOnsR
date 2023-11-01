test_that("testa arquivo arquivos.csv", {
    dir.create(file.path(system.file("extdata", package = "smapOnsR"), "validacao_arq_entrada_novo"))
    zip::unzip(system.file("extdata", "validacao_arq_entrada_novo.zip", package = "smapOnsR"), exdir = system.file("extdata", "validacao_arq_entrada_novo", package = "smapOnsR"))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN01", "CT1.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))
    
    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN01", "CT1.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN01", "CT1.3", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN01", "CT1.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN01", "CT1.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN01", "CT1.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arquivos(pasta_entrada))
})

test_that("testa arquivo posto_plu.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.1", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.2", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.3", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.4", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.5", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.6", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    # arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.7", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    # expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.8", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.9", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.10", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN02", "CT2.11", "Arq_Entrada", "postos_plu.csv", package = "smapOnsR")
    expect_error(le_postos_plu(arq))
})

test_that("testa datasRodadas.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN03", "CT3.1", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN03", "CT3.2", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN03", "CT3.3", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN03", "CT3.4", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_datas_rodada(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN03", "CT3.5", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN03", "CT3.6", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN03", "CT3.7", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN03", "CT3.8", "Arq_Entrada", "datasRodadas.csv", package = "smapOnsR")
    expect_error(le_datas_rodada(arq))

})

test_that("testa inicializacao.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.1", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.2", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.3", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.4", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_inicializacao(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.5", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.6", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.7", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.8", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.9", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.10", "Arq_Entrada", "inicializacao.csv", package = "smapOnsR")
    expect_error(le_inicializacao(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN04", "CT4.11", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))
})

test_that("testa parametros.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.1", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.2", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_parametros(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.3", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.4", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.5", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.6", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.7", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.8", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.9", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_arq_entrada_novo(pasta_entrada))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.10", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN05", "CT5.11", "Arq_Entrada", "parametros.csv", package = "smapOnsR")
    expect_error(le_parametros(arq))
})

test_that("testa precipitacao_prevista.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.1", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.2", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_precipitacao_prevista(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.3", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.4", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_precipitacao_prevista(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.5", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.6", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.7", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.8", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.9", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(le_precipitacao_prevista(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.10", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN06", "CT6.11", "Arq_Entrada", "precipitacao_prevista.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_precipitacao_prevista(arq)))
})

test_that("testa vazoes.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN07", "CT7.1", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN07", "CT7.2", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN07", "CT7.3", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_historico_verificado(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN07", "CT7.4", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN07", "CT7.5", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN07", "CT7.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN07", "CT7.7", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN07", "CT7.8", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN07", "CT7.9", "Arq_Entrada", "vazoes.csv", package = "smapOnsR")
    expect_error(le_historico_verificado(arq))
})


test_that("testa evapotranspiracao_nc.csv", {
    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN08", "CT8.1", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(le_evapotranspiracao_nc(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN08", "CT8.2", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(le_evapotranspiracao_nc(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN08", "CT8.3", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(le_evapotranspiracao_nc(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN08", "CT8.4", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_evapotranspiracao_nc(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN08", "CT8.5", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_evapotranspiracao_nc(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN08", "CT8.6", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(le_evapotranspiracao_nc(arq))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN08", "CT8.7", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(suppressWarnings(le_evapotranspiracao_nc(arq)))

    arq <- system.file("extdata", "validacao_arq_entrada_novo", "CN08", "CT8.8", "evapotranspiracao_nc.csv", package = "smapOnsR")
    expect_error(le_evapotranspiracao_nc(arq))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN08", "CT8.9", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "validacao_arq_entrada_novo", "CN08", "CT8.10", package = "smapOnsR")
    expect_error(suppressWarnings(le_arq_entrada_novo(pasta_entrada)))

    unlink(system.file("extdata", "validacao_arq_entrada_novo", package = "smapOnsR"), recursive = TRUE)
})
