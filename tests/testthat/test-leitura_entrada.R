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
    dir.create(file.path(system.file("extdata", package = "smapOnsR"), "Validacao"))
    zip::unzip(system.file("extdata", "validacao.zip", package = "smapOnsR"), exdir = system.file("extdata", "Validacao", package = "smapOnsR"))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.3", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.7", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.8", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.9", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.11", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.12", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN01", "CT1.13", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_caso(pasta_entrada)))
})


test_that("testa arquivo Modelos_precipitacao.txt", {
    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_modelos_precipitacao(pasta_entrada)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_modelos_precipitacao(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.3", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_modelos_precipitacao(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_modelos_precipitacao(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_modelos_precipitacao(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_modelos_precipitacao(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.7", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_modelos_precipitacao(pasta_entrada)$numero_cenarios, 1)

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.8", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_modelos_precipitacao(pasta_entrada)$numero_cenarios, 2)

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.9", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_modelos_precipitacao(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.11", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_modelos_precipitacao(pasta_entrada)$numero_cenarios, 1)

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.12", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_modelos_precipitacao(pasta_entrada)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN02", "CT2.13", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_modelos_precipitacao(pasta_entrada)))
})

test_that("testa arquivo 'sub_bacia'_inicializacao.txt", {
    nome_subbacia <- "Porto"
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
   
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.3", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.7", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.8", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.9", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.10", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.11", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.12", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.13", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.14", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.15", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.16", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_inicializacao(pasta_entrada, nome_subbacia)$inicializacao[variavel == "Ebin", valor], 12.5)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.17", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.18", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.19", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.20", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.21", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_inicializacao(pasta_entrada, nome_subbacia)$inicializacao[variavel == "Supin", valor], 1.4)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.22", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.23", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.24", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.25", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.26", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_inicializacao(pasta_entrada, nome_subbacia)$inicializacao[variavel == "Tuin", valor], 30.2)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.27", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.28", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.29", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_inicializacao(pasta_entrada, nome_subbacia)$inicializacao[variavel == "Tuin", valor], 30.0)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN03", "CT3.30", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_inicializacao(pasta_entrada, nome_subbacia)))    
})

test_that("testa arquivo 'sub_bacia'_'modelo_precipitacao'.txt", {
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.1", "Arq_Entrada", package = "smapOnsR")
    modelos_precipitacao <- suppressWarnings(le_entrada_modelos_precipitacao(pasta_entrada))

    nome_subbacia <- "Porto"

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))
   
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.3", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.7", "Arq_Entrada", package = "smapOnsR")
    expect_equal(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao))$longitude[1], -45.4)

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.8", "Arq_Entrada", package = "smapOnsR")
    expect_equal(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao))$longitude[1], -45.4)

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.9", "Arq_Entrada", package = "smapOnsR")
    expect_equal(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao))$longitude[1], -45)

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.10", "Arq_Entrada", package = "smapOnsR")
    expect_equal(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao))$longitude[1], -45.4)

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.11", "Arq_Entrada", package = "smapOnsR")
    expect_equal(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao))$longitude[1], -45.4)

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.12", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.13", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.14", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.15", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.16", "Arq_Entrada", package = "smapOnsR")
    expect_equal(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao))$latitude[1], -21)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.17", "Arq_Entrada", package = "smapOnsR")
    expect_equal(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao))$latitude[1], -21.8)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.18", "Arq_Entrada", package = "smapOnsR")
    expect_equal(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao))$latitude[1], -21.801)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.19", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.20", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.21", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.22", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.23", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.24", "Arq_Entrada", package = "smapOnsR")
    expect_equal(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao))$longitude[1], -45.4)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.25", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)$longitude[1], -45.4)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN04", "CT4.26", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_pontos_grade(pasta_entrada, nome_subbacia, modelos_precipitacao)))
})

test_that("testa arquivo 'sub_bacia'_'evapotranspiracao'.txt", {
    nome_subbacia <- "Porto"

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))
   
    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.3", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)$mes[1], 1)

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.6", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)$mes[1], 1)

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.7", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.8", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)$valor[1], 4)

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.9", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)$valor[1], 4.9)

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.10", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)$valor[1], 4.904)

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.11", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.12", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)$valor[1], 494)

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.13", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.14", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.15", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.16", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.17", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)$valor[1], 4.94)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.18", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)$valor[1], 4.94)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN05", "CT5.19", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_evapotranspiracao(pasta_entrada, nome_subbacia)))
})

test_that("testa arquivo 'sub_bacia'_'postos_plu'.txt", {
    nome_subbacia <- "Porto"

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.3", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))
   
    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.7", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_posto_plu(pasta_entrada, nome_subbacia)$valor[1], 0.2)
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.8", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_posto_plu(pasta_entrada, nome_subbacia)$valor[1], 0.2)

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.9", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_posto_plu(pasta_entrada, nome_subbacia)$valor[3], 1)

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.10", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_posto_plu(pasta_entrada, nome_subbacia)$valor[1], 0.2)

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.11", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_posto_plu(pasta_entrada, nome_subbacia)$valor[1], 0.201)

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.12", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.13", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.14", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.15", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_posto_plu(pasta_entrada, nome_subbacia)$valor[1], 0.191)

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.16", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.17", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.18", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_posto_plu(pasta_entrada, nome_subbacia)$valor[1], 0.191)

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.19", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.20", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.21", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.22", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_posto_plu(pasta_entrada, nome_subbacia)$posto[1], "2245196")

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.23", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.24", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_posto_plu(pasta_entrada, nome_subbacia)$posto[1], "2245196")

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.25", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_posto_plu(pasta_entrada, nome_subbacia)$posto[1], "2245196")

    pasta_entrada <- system.file("extdata", "Validacao", "CN06", "CT6.26", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_posto_plu(pasta_entrada, nome_subbacia)))
})

test_that("testa arquivo 'sub_bacia'_'parametros'.txt", {
    nome_subbacia <- "Porto"

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.1", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.2", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.4", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.5", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))
    
    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.6", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.7", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.8", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.9", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_parametros(pasta_entrada, nome_subbacia)$valor[2], 5)

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.10", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.11", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.12", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.12", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.13", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.14", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.15", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.16", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.17", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_parametros(pasta_entrada, nome_subbacia)$valor[2], 5)

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.18", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.19", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_parametros(pasta_entrada, nome_subbacia)$valor[2], 5)

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.20", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.21", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.22", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.23", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.24", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.25", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.26", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.27", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.28", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.29", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.30", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.31", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.32", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.33", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.34", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.35", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.36", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.37", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.38", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.39", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.40", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.41", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.42", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.43", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.44", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.45", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.46", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.47", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.48", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.49", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.50", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.51", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.52", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.53", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.54", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.55", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.56", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.57", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.58", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.59", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.60", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.61", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.62", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.63", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.64", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.65", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.66", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.67", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.68", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.69", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.70", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.71", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.72", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.73", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.74", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.75", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.76", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.77", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.78", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.79", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.80", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.81", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.82", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.83", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.84", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.85", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.86", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.87", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.88", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.89", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.90", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.91", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.92", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.93", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.94", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_parametros(pasta_entrada, nome_subbacia)$valor[81], 2)

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.95", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.96", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_parametros(pasta_entrada, nome_subbacia)$valor[1], 123456.6)

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.97", "Arq_Entrada", package = "smapOnsR")
    expect_equal(le_entrada_parametros(pasta_entrada, nome_subbacia)$valor[1], 100322)

    pasta_entrada <- system.file("extdata", "Validacao", "CN07", "CT7.98", "Arq_Entrada", package = "smapOnsR")
    expect_error(suppressWarnings(le_entrada_parametros(pasta_entrada, nome_subbacia)))

    unlink(system.file("extdata", "Validacao", package = "smapOnsR"), recursive = TRUE)
})