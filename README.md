
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

Exemplo de como executar um caso com os dados de entrada oficiais.

``` r
library(smapOnsR)
#> The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, will retire in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> The sp package is now running under evolution status 2
#>      (status 2 uses the sf package in place of rgdal)
#> Please note that 'maptools' will be retired during October 2023,
#> plan transition at your earliest convenience (see
#> https://r-spatial.org/r/2023/05/15/evolution4.html and earlier blogs
#> for guidance);some functionality will be moved to 'sp'.
#>  Checking rgeos availability: FALSE
## Executar linha abaixo
#pasta_entrada <- system.file("extdata", "Arq_entrada", package = "smapOnsR")

#saida <- executa_caso_oficial(pasta_entrada)
```
