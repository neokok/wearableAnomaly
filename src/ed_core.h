#ifndef WEARABLEANOMALY_ED_CORE_H
#define WEARABLEANOMALY_ED_CORE_H

#include <Rcpp.h>

Rcpp::NumericMatrix pairwise_energy(const Rcpp::NumericVector& x);
double energy_stat_segment(const Rcpp::NumericVector& x, int a, int b, int c, int d);

#endif
