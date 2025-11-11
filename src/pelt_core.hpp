#ifndef WEARABLEANOMALY_PELT_CORE_HPP
#define WEARABLEANOMALY_PELT_CORE_HPP

#include <Rcpp.h>

Rcpp::IntegerVector pelt_core_meanvar(Rcpp::NumericVector x, int min_seg_len, double pen_value);

#endif
