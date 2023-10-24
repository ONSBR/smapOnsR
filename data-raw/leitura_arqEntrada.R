## PARAMETROS
devtools::load_all()

unzip("inst//extdata//dados.zip")
arquivos <- list.files("inst//extdata//parametros")

parametros <- data.table::data.table()
postos_plu <- data.table::data.table()
for (arq in arquivos){
    nome_subbacia <- strsplit(arq, "_")[[1]][1]
    pasta_entrada <- system.file("extdata", "parametros", package = "smapOnsR")
    saida <- le_entrada_parametros(pasta_entrada, nome_subbacia)
    parametros <- rbind(parametros, saida)
}
usethis::use_data(parametros, overwrite = TRUE)
usethis::use_data(postos_plu, overwrite = TRUE)

arquivos <- list.files("inst/extdata/etp")
historico_etp_NC <- data.table::data.table()
for (arq in arquivos){
    saida <- le_evapotranspiracao(paste0("inst/extdata/etp/", arq))
    historico_etp_NC <- rbind(historico_etp_NC, saida)
}
data.table::setcolorder(historico_etp_NC, c("mes", "nome", "valor"))

usethis::use_data(historico_etp_NC, overwrite = TRUE)

unlink("inst//extdata//etp", recursive = TRUE)
unlink("inst//extdata//parametros", recursive = TRUE)
unlink("inst//extdata//postos_plu", recursive = TRUE)
unlink("inst//extdata//etp.csv")
unlink("inst//extdata//PSAT.csv")
unlink("inst//extdata//vazoes.csv")
