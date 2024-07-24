
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smapOnsR

<!-- badges: start -->

[![R-CMD-check](https://github.com/ONSBR/smapOnsR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ONSBR/smapOnsR/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/ONSBR/smapOnsR/workflows/test-coverage/badge.svg)](https://github.com/ONSBR/smapOnsR/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/ONSBR/smapOnsR/graph/badge.svg?token=C7FF7ZNCER)](https://codecov.io/gh/ONSBR/smapOnsR)
<!-- badges: end -->

Pacote com a implementacao em R do modelo SMAP/ONS

## Instalação

Para a instalacao do pacote no Windows, e necessaria a instalacao do
programa Rtools, de mesma versao do R:
<https://cran.r-project.org/bin/windows/Rtools/> Este pacote ainda nao
se encontra disponibilizado no CRAN, de modo que deve ser instalado
diretamente a partir do repositorio. Para a versao oficial, deve-se
utilizar o seguinte comando

``` r
# install.packages("devtools")
devtools::install_github("ONSBR/smapOnsR@v1.0.0")
```

Para a versao em desenvolvimento, deve-se utilizar o seguinte comando:

``` r
# install.packages("devtools")
devtools::install_github("ONSBR/smapOnsR")
```

## Exemplo

Exemplo de como executar um caso com os dados de entrada oficiais.

``` r
library(smapOnsR)
## Executar linha abaixo
zip::unzip(system.file("extdata", "dados_entrada.zip", package = "smapOnsR"), exdir = system.file("extdata", package = "smapOnsR"))
pasta_entrada <- system.file("extdata", "caso_completo2", package = "smapOnsR")

executa_caso_oficial(pasta_entrada)
```

Exemplo de como executar um caso com os dados de entrada novos.

``` r
library(smapOnsR)
## Executar linha abaixo
pasta_entrada <- system.file("extdata", "arq_entrada_novo", package = "smapOnsR")

saida <- executa_caso_novo(pasta_entrada)
```
