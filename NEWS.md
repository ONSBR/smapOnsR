# smapOnsR 1.3.2

## Minor changes

* Possibilidade de importar e exportar datas no processo de calibracao

* Possibilidade de importar e exportar valores de inicializacao da calibracao

* Possibilidade de exportacao da simulacao na calibracao

* Reparametrizacao da funcao beta 

* Reparametrizacao da variavel pmur, como percentual relativo ao capc

* Adiciona caso exemplo com teste para casos com aprimoramentos

* Eficientizacao das funcoes de propagacao e totalizacao

# smapOnsR 1.3.1

## Minor changes

* Otimizacao do codigo da funcao completa_previsao

* Funcao de ponderacao temporal em c++

* Adiciona nova função executa_caso_oficial_v2 para rodadas oficiais com novos aprimoramentos 

* Padronizacao da variavel numero_dias_assimilacao nas entradas oficial e nova

* Adiciona nova funcao de rodada com aprimoramentos, porem, sem assimilar a etp

# smapOnsR 1.3.0

## Major changes

* Inclusao da possibilidade de utilizacao dos aprimoramentos no Rsolo

# smapOnsR 1.2.0

## Minor changes

* Parametros adicionais para a otimizacao da calibracao

## Bug fixes

* Correcao do calculo de kt min

# smapOnsR 1.1.0

## Major changes

* Disponibilizacao de Docker

## Minor changes

* Ajuste na assimilacao para casos com menos de 31 dias

* Ajuste na funcao de totalizacao

* Ajuste no arquivo de configuracao exemplo

# smapOnsR 1.0.0

* Versão validada a ser utilizada oficialmente

# smapOnsR 0.3.0

## Major changes

* Processo de calibracao paralelizado

## Minor changes

* Ajuste na validacao de arquivo de inicializacao oficial, e de previsao nova

* Inclusao de teste de validacao de previsao nova

* Ajuste em arquivos exemplo

* Ajuste na leitura de arquivo de precipitacao observada, "modelos_precipitacao" e "_parametros" oficial

* Alteracao do teste das funcoes de rodadas_encadeadas

* Criacao de arquivo com funcoes de validacao de dados de entrada (futuramente todas as validacoes de arquivos de entrada estará contida nesse arquivo)


# smapOnsR 0.2.2

## Bug fixes
* Correcao da ponderacao temporal para somatorio de kts diferente de 1

* Correcao da aplicacao dos pesos da assimilacao na etp para casos com etp serie temporal

* Ajuste na leitura de "modelos_precipitacao.txt" quando possui comentario com caractere especial

## Minor changes
* Ajuste leitura caso.txt e arquivo de vazoes e parametros oficiais 

* Ajuste testes operacionais e entrada de dados de validacao

# smapOnsR 0.2.1

## Major changes

* Ajuste da precipitacao observada apos o processo de assimilacao limitado ao valor maximo de 1

## Minor changes

* escrita de arquivos de otimizacao e funcao_objetivo na funcao de execucao

* ajuste nas funcoes de leitura de arquivo de saida

# smapOnsR 0.2.0

## Major changes

* Possibilidade de ajuste da precipitacao observada apos o processo de assimilacao. O processo de assimilacao permanece igual. Apos o termino da assimilacao a precipitacao observada diaria e corrigida pelo fator do penultimo dia de assimilacao.

## Minor changes

* Inclusao de testes de validacao do arquivo bat.conf

* Ajuste na validacao de arquivo de previsoes em novo formato

* Numero de cenarios pode ser variavel por data a ser executada

## Bug fixes

* Inclusao da vazao de planicie no calculo da vazao superficial para rodadas encadeadas

* Ajuste na assimilacao para casos com vazao nula

* Ajuste na funcao "completa_previsao" para execucoes com mais de 1 caso 

# smapOnsR 0.1.0

* Versão inicial utilizada na validacao do pacote smapOnsR
