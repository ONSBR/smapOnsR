#test_that("testa le_entrada_caso", {
#  dir <- getwd()
#  pasta_entrada <- file.path(dir, "inst//extdata//arq_entrada")
#  caso <- le_entrada_caso(pasta_entrada)
#
#  expect_equal(caso$numero_subbacias == 3, TRUE)
#  expect_equal(caso$nome_subbacia[1] == "camargos", TRUE)
#  expect_error(le_entrada_caso('pasta_entrada'))
#})
#
#test_that("testa le_entrada_evapotranspiracao", {
#  dir <- getwd()
#  pasta_entrada <- file.path(dir, "inst//extdata//arq_entrada")
#  caso <- le_entrada_caso(pasta_entrada)
#
#  evapotranspiracao <- le_entrada_evapotranspiracao(nome_subbacia = caso$nome_subbacia[1], pasta_entrada = pasta_entrada)
#  expect_equal(colnames(evapotranspiracao), c("mes", "posto", "valor"))
#  expect_equal(evapotranspiracao[, valor][1] == 4.94, TRUE)
#
#  expect_error(le_entrada_evapotranspiracao(nome_subbacia = "baixoig", pasta_entrada = pasta_entrada))
#})
#
#test_that("testa le_entrada_parametros", {
#  dir <- getwd()
#  pasta_entrada <- file.path(dir, "inst//extdata//arq_entrada")
#  caso <- le_entrada_caso(pasta_entrada)
#
#  parametros_subbacia <- le_entrada_parametros(nome_subbacia = caso$nome_subbacia[1], pasta_entrada = pasta_entrada)
#  expect_equal(colnames(parametros_subbacia), c("Nome", "Area", "nKt", paste0("Kt", 2:-60),
#    "Str", "K2t", "Crec", "Ai", "Capc", "K_kt", "K2t2", "H1", "H", "K3t", "K1t",
#    "Ecof", "Pcof", "Ecof2", "ktMin", "ktMax"))
#  expect_equal(parametros_subbacia$Str == 100, TRUE)
#
#  expect_error(le_entrada_parametros(nome_subbacia = "baixoig", pasta_entrada = pasta_entrada))
#})
#
#test_that("testa le_entrada_posto_plu", {
#  dir <- getwd()
#  pasta_entrada <- file.path(dir, "inst//extdata//arq_entrada")
#  caso <- le_entrada_caso(pasta_entrada)
#
#  postos_plu <- le_entrada_posto_plu(nome_subbacia = caso$nome_subbacia[3], pasta_entrada = pasta_entrada)
#  expect_equal(colnames(evapotranspiracao), c("mes", "posto", "valor"))
#  expect_equal(evapotranspiracao[, valor][1] == 4.94, TRUE)
#
#  expect_error(le_entrada_evapotranspiracao(nome_subbacia = "baixoig", pasta_entrada = pasta_entrada))
#})

test_that("testa arquivo caso.txt", {
    pasta_entrada <- system.file("extdata", "Arq_entrada", package = "smapOnsR")
    caso <- le_entrada_caso(pasta_entrada)

    expect_equal(caso$numero_subbacias, 22)

    dir.create(file.path(system.file("extdata", package = "smapOnsR"), "Validacao"))
    zip::unzip(system.file("extdata", "validacao.zip", package = "smapOnsR"), exdir = system.file("extdata", "Validacao", package = "smapOnsR"))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.3", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.7", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.8", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.9", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.11", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.12", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.13", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_caso(pasta_entrada))
})


test_that("testa arquivo Modelos_precipitacao.txt", {
    pasta_entrada <- system.file("extdata", "Arq_entrada", package = "smapOnsR")
    modelos_precipitacao <- le_entrada_modelos_precipitacao(pasta_entrada)

    expect_equal(modelos_precipitacao$numero_cenarios, 52)

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_modelos_precipitacao(pasta_entrada))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_modelos_precipitacao(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.3", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_modelos_precipitacao(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_modelos_precipitacao(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_modelos_precipitacao(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_modelos_precipitacao(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.7", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_modelos_precipitacao(pasta_entrada)$numero_cenarios, 1)

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.8", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_modelos_precipitacao(pasta_entrada)$numero_cenarios, 2)

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.9", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_modelos_precipitacao(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.11", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_modelos_precipitacao(pasta_entrada)$numero_cenarios, 1)

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.12", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_modelos_precipitacao(pasta_entrada))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.13", "Arq_Entrada", package = "smapOnsR")
    expect_error(le_entrada_modelos_precipitacao(pasta_entrada))

    unlink(system.file("extdata", "Validacao", package = "smapOnsR"), recursive = TRUE)
})