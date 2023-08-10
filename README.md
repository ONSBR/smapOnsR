
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smapOnsR

<!-- badges: start -->

[![R-CMD-check](https://github.com/felipe-treistman/smapOnsR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/felipe-treistman/smapOnsR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Pacote com a implementacao em R do modelo SMAP/ONS

## Instalação

Este pacote ainda nao se encontra disponibilizado no CRAN, de modo que
deve ser instalado diretamente a partir do repositorio utilizando:

``` r
# install.packages("devtools")
devtools::install_github("felipe-treistman/smapOnsR")
```

Por questões de paralelismo a funcao principal está separada em um
pacote a parte, sendo necessária a sua instalação:

``` r
# install.packages("devtools")
devtools::install_github("felipe-treistman/funcaoSmapCpp")
```

## Exemplo

Exemplo de como realizar o ajuste da transformação censurada latente
gaussiana com a base de dados exemplo.

``` r
library(smapOnsR)
#> Please note that 'maptools' will be retired during October 2023,
#> plan transition at your earliest convenience (see
#> https://r-spatial.org/r/2023/05/15/evolution4.html and earlier blogs
#> for guidance);some functionality will be moved to 'sp'.
#>  Checking rgeos availability: FALSE
## Executar linha abaixo
#pasta_entrada <- system.file("extdata", "Arq_entrada", package = "smapOnsR")

#saida <- executa_caso_oficial(pasta_entrada)
```
