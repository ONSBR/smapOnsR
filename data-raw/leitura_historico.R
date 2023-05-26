devtools::load_all()

historico_precipitacao <- le_historico_verificado("inst/extdata/psat.csv")
historico_vazao <- le_historico_verificado("inst/extdata/vazoes.csv")
historico_etp <- le_historico_verificado("inst/extdata/etp.csv")

usethis::use_data(historico_precipitacao, overwrite = TRUE)
usethis::use_data(historico_vazao, overwrite = TRUE)
usethis::use_data(historico_etp, overwrite = TRUE)

# historico <- merge(historico_etp, historico_vazao, by = c("posto", "data"))
# data.table::setorder(historico, id.y, data)
