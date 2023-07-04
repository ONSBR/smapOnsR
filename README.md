
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smapOnsR

<!-- badges: start -->
<!-- badges: end -->

Pacote com a implementacao em R do modelo SMAP/ONS

## Instalação

Este pacote ainda nao se encontra disponibilizado no CRAN, de modo que
deve ser instalado diretamente a partir do repositorio utilizando:

``` r
# install.packages("devtools")
devtools::install_github("felipe-treistman/smapOnsR")
```

## Exemplo

Exemplo de como realizar o ajuste da transformação censurada latente
gaussiana com a base de dados exemplo.

``` r
library(smapOnsR)
## executa um caso
pasta_entrada <- system.file("extdata", "Arq_entrada", package = "smapOnsR")

saida <- executa_caso_oficial(pasta_entrada)
#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.

#> Warning in data.table::fread(arq): Detected 1 column names but the data has 2
#> columns (i.e. invalid file). Added 1 extra default column name for the first
#> column which is guessed to be row names or an index. Use setnames() afterwards
#> if this guess is not correct, or fix the file write command that created the
#> file to create a valid file.
```
