test_that("transformacao de NC em serie temporal", {
  # CT27.1

  nome2 <- "baixoig"
  normal_climatologica <- historico_etp_NC[nome == nome2]
  serie_temporal <- historico_precipitacao[posto == postos_plu[nome == nome2, posto]]
  serie_temporal <- ponderacao_espacial(serie_temporal, postos_plu[nome == nome2])
  serie_temporal_NC <- transforma_NC_serie(serie_temporal, normal_climatologica)

  expect_equal(serie_temporal_NC[data == "2019/05/23", valor], normal_climatologica[mes == 5, valor])
})

test_that("transformacao de NC em serie temporal para vetor com apenas 1 mes", {
  # CT27.2

  data_rodada <- as.Date('2020/01/01')
  nome2 <- "baixoig"
  normal_climatologica <- historico_etp_NC[nome == nome2]
  precipitacao <- historico_precipitacao[data < data_rodada & data >= (data_rodada - 30) & posto == postos_plu[nome == nome2, posto]]
  precipitacao <- ponderacao_espacial(precipitacao, postos_plu[nome == nome2])
  evapotranspiracao <- transforma_NC_serie(precipitacao, normal_climatologica)

  expect_equal(evapotranspiracao[1, valor], normal_climatologica[mes == 12, valor])
})

test_that("transformacao historico em previsao", {
  # CT27.3

  sub_bacia <- "avermelha"
  precipitacao <- historico_precipitacao[posto %in% postos_plu[nome == sub_bacia, posto]]
  precipitacao <- ponderacao_espacial(precipitacao, postos_plu[nome == sub_bacia])
  datas_rodadas <- data.table(
    data = as.Date("2020-05-01"),
    numero_dias_previsao = c(15)
  )

  previsao <- transforma_historico_previsao(precipitacao, datas_rodadas)

  expect_equal(previsao[data_previsao == "2020-05-05", valor], precipitacao[data  == "2020-05-05", valor] )

  #-------------------

  datas_rodadas <- data.table(
    a = as.Date(c("2020-05-01", "2020-05-08")),
    numero_dias_previsao = c(15, 20)
  )        
  expect_error(transforma_historico_previsao(precipitacao, datas_rodadas))

  datas_rodadas <- data.table(
    data = c("2020-05-01", "2020-05-08"),
    numero_dias_previsao = c(15, 20)
  )
  expect_error(transforma_historico_previsao(precipitacao, datas_rodadas))

  # CT27.4
  
  evapotranspiracao <- historico_etp[posto == sub_bacia]
  datas_rodadas <- data.table(
    data = as.Date(c("2020-05-01", "2020-05-08")),
    numero_dias_previsao = c(15, 20)
  )

  colnames(evapotranspiracao)[2] <- "nome"
  previsao <- transforma_historico_previsao(evapotranspiracao, datas_rodadas)

  expect_equal(previsao[data_previsao == "2020-05-05", valor], evapotranspiracao[data  == "2020-05-05", valor] )
  expect_equal(previsao[data_previsao == "2020-05-10" & data_rodada == "2020-05-01" , valor], 
              previsao[data_previsao == "2020-05-10" & data_rodada == "2020-05-08" , valor])

  datas_rodadas <- data.table(
    a = as.Date(c("2020-05-01", "2020-05-08")),
    numero_dias_previsao = c(15, 20)
  )        
  expect_error(transforma_historico_previsao(precipitacao, datas_rodadas))

  datas_rodadas <- data.table(
    data = c("2020-05-01", "2020-05-08"),
    numero_dias_previsao = c(15, 20)
  )
  expect_error(transforma_historico_previsao(precipitacao, datas_rodadas))
})

#------testes funcao cria_inicializacao------------------
# 1) Testa a entrada via data.table
test_that("Criacao de data.table inicializacao via data.table gera saida correta", {
  dt_in <- data.table(
    nome      = c("a", "a", "a", "b", "b", "b", "b"),
    parametro = c("Ebin", "Supin", "Tuin", "Ebin", "Supin", "Tuin", "X"),
    valor     = c(10, 5, 3, 20, 7, 4, 999)
  )
  # Deve ignorar o parametro "X"
  dt_out <- cria_inicializacao(parametros = dt_in)

  # Para cada nome deve ter exatamente 9 linhas
  expect_equal(dt_out[nome == "a", .N], 9)
  expect_equal(dt_out[nome == "b", .N], 9)

  # Verifica que as variaveis estao corretas
  vars_a <- dt_out[nome == "a", variavel]
  expect_setequal(vars_a, c("Ebin", "Supin", "Tuin",
                            "numero_dias_assimilacao", "ajusta_precipitacao",
                            "limite_inferior_ebin", "limite_superior_ebin",
                            "limite_inferior_prec", "limite_superior_prec"))
  
  # Valores fixos padrao
  expect_equal(
    dt_out[nome == "a" & variavel == "numero_dias_assimilacao", valor],
    32L
  )
  expect_equal(
    dt_out[nome == "b" & variavel == "ajusta_precipitacao", valor],
    1L
  )
})

# 2) Testa a entrada via vetores
test_that("Entrada via vetores gera saida correta", {
  nomes  <- c("x", "y")
  Ebin   <- c(100, 200)
  Supin  <- c(10, 20)
  Tuin   <- c(1, 2)
  dt_out <- cria_inicializacao(
    nome  = nomes,
    Ebin  = Ebin,
    Supin = Supin,
    Tuin  = Tuin
  )
  
  # Linhas esperadas: 2 nomes × 9 variaveis = 10
  expect_equal(nrow(dt_out), 18)
  # Verifica combinacao de nome e valores
  expect_equal(
    dt_out[variavel == "Ebin", valor],
    c(100, 200)
  )
  expect_equal(
    dt_out[variavel == "Tuin", valor],
    c(1, 2)
  )
})

# 3) Testa valores fixos customizados
test_that("Parametros fixos customizados sao aplicados", {
  dt_out <- cria_inicializacao(
    nome  = "zz",
    Ebin  = 1,
    Supin = 2,
    Tuin  = 3,
    numero_dias_assimilacao = 99L,
    ajusta_precipitacao     = 0L
  )
  expect_equal(
    dt_out[variavel=="numero_dias_assimilacao", valor],
    99L
  )
  expect_equal(
    dt_out[variavel=="ajusta_precipitacao", valor],
    0L
  )
})

# 4) Testa erros de validacao
test_that("Erro quando falta vetores obrigatorios", {
  expect_error(
    cria_inicializacao(nome = "a", Ebin = 1, Supin = 2),
    "Se 'parametros' for NULL, fornecer 'nome', 'Ebin', 'Supin' e 'Tuin'."
  )
})

test_that("Erro quando vetores têm comprimentos diferentes", {
  expect_error(
    cria_inicializacao(
      nome  = c("a", "b"),
      Ebin  = c(1, 2, 3),
      Supin = c(4, 5),
      Tuin  = c(6, 7)
    ),
    "'nome', 'Ebin', 'Supin' e 'Tuin' devem ter mesmo comprimento."
  )
})

test_that("Erro quando data.table nao tem colunas esperadas", {
  dt_bad <- data.table(foo = 1, bar = 2)
  expect_error(
    cria_inicializacao(parametros = dt_bad),
    "O data.table 'parametros' deve ter colunas 'nome','parametro','valor'."
  )
})