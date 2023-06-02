#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ponderacao_temporal_cpp(NumericVector serie_temporal, NumericVector kt) {
  int kt_min = sum(kt[Rcpp::Range(3, 62)] > 0);
  int kt_max = sum(kt[Rcpp::Range(0, 1)] > 0);
  int id_min = kt_min;
  int id_max = serie_temporal.size() - kt_max - 1;
  NumericVector serie_temporal_ponderada(id_max - id_min);

  for (int i = id_min; i <= id_max; ++i) {
    int inicio = i - kt_min;
    int fim = i + kt_max;
    serie_temporal_ponderada[i - id_min] = sum(serie_temporal[Rcpp::Range(inicio, fim)] * kt[Rcpp::Range(kt_min + 2, 2 - kt_max)]);
  }

  return serie_temporal_ponderada;
}