# smapOnsR 0.2.2

## Bug fixes
* Correcao da ponderacao temporal para somatorio de kts diferente de 1

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

* Possibilidade de ajuste da precipitacao observada apos o processo de assimilacao. O processo de assimilacao permanece igual. Apos o termino da assimilacao a precipitação observada diaria e corrigida pelo fator do penultimo dia de assimilacao.

## Minor changes

* Inclusao de testes de validacao do arquivo bat.conf

* Ajuste na validacao de arquivo de previsoes em novo formato

* Numero de cenarios pode ser variavel por data a ser executada

## Bug fixes

* Inclusao da vazao de planicie no calculo da vazao superficial para rodadas encadeadas

* Ajuste na assimilacao para casos com vazao nula

* Ajuste na funcao "completa_previsao" para execucoes com mais de 1 caso 

# smapOnsR 0.1.0

* Versão inicial utilizada na validação do pacote smapOnsR
