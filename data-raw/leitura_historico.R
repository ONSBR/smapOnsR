devtools::load_all()

unzip("inst//extdata//dados.zip")

historico_precipitacao <- le_historico_verificado("inst/extdata/psat.csv")
historico_vazao <- le_historico_verificado("inst/extdata/vazoes.csv")
historico_etp <- le_historico_verificado("inst/extdata/etp.csv")

usethis::use_data(historico_precipitacao, overwrite = TRUE)
usethis::use_data(historico_vazao, overwrite = TRUE)
usethis::use_data(historico_etp, overwrite = TRUE)

unlink("inst//extdata//etp", recursive = TRUE)
unlink("inst//extdata//parametros", recursive = TRUE)
unlink("inst//extdata//etp.csv")
unlink("inst//extdata//PSAT.csv")
unlink("inst//extdata//vazoes.csv")
