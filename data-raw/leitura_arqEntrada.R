## PARAMETROS
devtools::load_all()

arquivos <- list.files("inst/extdata/parametros")

parametros <- data.table::data.table()
for (arq in arquivos){
    saida <- le_parametros(paste0("inst/extdata/parametros/", arq))
    parametros <- rbind(parametros,saida)
}
parametros <- data.table::melt(parametros, id.vars = "nome", variable.name = "parametro",
           value.name = "valor")

usethis::use_data(parametros, overwrite = TRUE)
