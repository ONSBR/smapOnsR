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