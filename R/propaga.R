propaga_tv <- function(vazao_montante,  vazao_jusante,  tempo_viagem){
  Nper <- length(vazao_jusante)
  vazao_propagada <- array(rep(0, Nper), c(Nper))
  lag_dias <- ceiling(tempo_viagem / 24)
  fator_segundo_dia <- ((24 * lag_dias) - tempo_viagem) / 24
  fator_primeiro_dia <- (tempo_viagem - (24 * (lag_dias - 1))) / 24
  for (iper in (1 + lag_dias):Nper) {
    if (lag_dias > 0) {
      vazao_propagada[iper] <- (((vazao_montante[iper - lag_dias + 1] * fator_segundo_dia) + (vazao_montante[iper - lag_dias] * fator_primeiro_dia) ) ) + vazao_jusante[iper]
    }else{
      vazao_propagada[iper] <- (((vazao_montante[iper] * fator_segundo_dia) + (vazao_montante[iper] * fator_primeiro_dia))) + vazao_jusante[iper]
    }
  }
  vazao_propagada
}

propaga_muskingum <- function(vazao_montante, vazao_jusante, n, coeficientes){
  Nper <- length(vazao_jusante)
  vazao_passo_n <- array(rep(0, (n + 1), Nper), c(Nper, (n + 1)))
  vazao_passo_n[1, ] <- vazao_montante[1]
  vazao_passo_n[, 1] <- vazao_montante # primeira coluna: vazao original
  vazao_propagada <- array(rep(0, Nper), c(Nper))
  for (iper in 2:Nper) {
    for (indiceN in 2:(n + 1)) {
      vazao_passo_n[iper, indiceN] <- vazao_passo_n[iper, (indiceN-1)] * coeficientes[1] +  vazao_passo_n[(iper - 1), (indiceN-1)] * coeficientes[2] +  vazao_passo_n[iper-1, indiceN] * coeficientes[3]
    }
    vazao_propagada[iper] <- vazao_passo_n[iper, (n + 1)] + vazao_jusante[iper]
  }
  vazao_propagada
}