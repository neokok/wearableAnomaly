#include "ed_core.h"

#include <algorithm>
#include <cmath>

// [[Rcpp::export]]
Rcpp::NumericMatrix pairwise_energy(const Rcpp::NumericVector& x) {
  const int n = x.size();
  Rcpp::NumericMatrix mat(n, n);
  for (int i = 0; i < n; ++i) {
    mat(i, i) = 0.0;
    for (int j = i + 1; j < n; ++j) {
      const double diff = std::abs(x[i] - x[j]);
      mat(i, j) = diff;
      mat(j, i) = diff;
    }
  }
  return mat;
}

namespace {

double sum_within_segment(const Rcpp::NumericVector& x, int start, int end) {
  double total = 0.0;
  for (int i = start; i < end; ++i) {
    const double xi = x[i];
    for (int j = i + 1; j <= end; ++j) {
      total += std::abs(xi - x[j]);
    }
  }
  return total * 2.0;  // convert to ordered pairs (i != j)
}

double sum_cross_segments(const Rcpp::NumericVector& x, int a_start, int a_end, int b_start, int b_end) {
  double total = 0.0;
  for (int i = a_start; i <= a_end; ++i) {
    const double xi = x[i];
    for (int j = b_start; j <= b_end; ++j) {
      total += std::abs(xi - x[j]);
    }
  }
  return total;
}

}  // namespace

// [[Rcpp::export]]
double energy_stat_segment(const Rcpp::NumericVector& x, int a, int b, int c, int d) {
  // convert to zero-based indices
  const int left_start = a - 1;
  const int left_end = b - 1;
  const int right_start = c - 1;
  const int right_end = d - 1;

  if (left_start < 0 || right_end >= x.size() || left_end >= right_start) {
    return NA_REAL;
  }

  const int n1 = left_end - left_start + 1;
  const int n2 = right_end - right_start + 1;
  if (n1 <= 0 || n2 <= 0) {
    return NA_REAL;
  }

  const double left_sum = sum_within_segment(x, left_start, left_end);
  const double right_sum = sum_within_segment(x, right_start, right_end);
  const double cross_sum = sum_cross_segments(x, left_start, left_end, right_start, right_end);

  const double cross_mean = cross_sum / (static_cast<double>(n1) * static_cast<double>(n2));
  const double left_denom = static_cast<double>(n1) * static_cast<double>(n1 - 1);
  const double right_denom = static_cast<double>(n2) * static_cast<double>(n2 - 1);
  const double left_mean = left_denom > 0.0 ? left_sum / left_denom : 0.0;
  const double right_mean = right_denom > 0.0 ? right_sum / right_denom : 0.0;
  const double scale = (static_cast<double>(n1) * static_cast<double>(n2)) /
    (static_cast<double>(n1) + static_cast<double>(n2));
  return scale * (2.0 * cross_mean - left_mean - right_mean);
}
