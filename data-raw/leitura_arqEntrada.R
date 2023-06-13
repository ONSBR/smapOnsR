## PARAMETROS
devtools::load_all()

unzip("inst//extdata//dados.zip")
arquivos <- list.files("inst/extdata/parametros")

parametros <- data.table::data.table()
for (arq in arquivos){
    saida <- le_parametros(paste0("inst/extdata/parametros/", arq))
    parametros <- rbind(parametros, saida)
}
parametros <- data.table::melt(parametros, id.vars = "Nome", variable.name = "parametro",
           value.name = "valor")
usethis::use_data(parametros, overwrite = TRUE)

arquivos <- list.files("inst/extdata/etp")
historico_etp_NC <- data.table::data.table()
for (arq in arquivos){
    saida <- le_evapotranspiracao(paste0("inst/extdata/etp/", arq))
    historico_etp_NC <- rbind(historico_etp_NC, saida)
}
historico_etp_NC[, id := match(historico_etp_NC$posto, unique(historico_etp_NC$posto))]
data.table::setcolorder(historico_etp_NC, c("mes", "posto", "id", "valor"))

usethis::use_data(historico_etp_NC, overwrite = TRUE)

unlink("inst//extdata//etp", recursive = TRUE)
unlink("inst//extdata//parametros", recursive = TRUE)
unlink("inst//extdata//etp.csv")
unlink("inst//extdata//PSAT.csv")
unlink("inst//extdata//vazoes.csv")
