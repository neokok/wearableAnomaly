#include "pelt_core.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::IntegerVector pelt_core_meanvar(Rcpp::NumericVector x, int min_seg_len, double pen_value) {
  const int n = x.size();
  if (min_seg_len < 2) {
    Rcpp::stop("`min_seg_len` must be at least 2.");
  }
  if (n < 2 * min_seg_len) {
    Rcpp::stop("Series is shorter than required for the supplied `min_seg_len`.");
  }

  for (int i = 0; i < n; ++i) {
    if (!std::isfinite(x[i])) {
      Rcpp::stop("Input must not contain missing or infinite values.");
    }
  }

  std::vector<double> prefix(n + 1, 0.0);
  std::vector<double> prefix_sq(n + 1, 0.0);
  for (int i = 0; i < n; ++i) {
    const double xi = x[i];
    prefix[i + 1] = prefix[i] + xi;
    prefix_sq[i + 1] = prefix_sq[i] + xi * xi;
  }

  std::vector<double> F(n + 1, std::numeric_limits<double>::infinity());
  std::vector<int> last_change(n + 1, 0);
  F[0] = -pen_value;

  std::vector<int> candidates;
  candidates.reserve(n + 1);
  candidates.push_back(0);

  const double eps = 1e-12;
  const double prune_tol = 1e-8;

  for (int t = min_seg_len; t <= n; ++t) {
    std::vector<int> eligible;
    eligible.reserve(candidates.size());
    for (int s : candidates) {
      if (t - s >= min_seg_len) {
        eligible.push_back(s);
      }
    }

    if (eligible.empty()) {
      candidates.push_back(t);
      continue;
    }

    std::vector<double> totals(eligible.size());
    double best_total = std::numeric_limits<double>::infinity();
    int best_prev = eligible[0];

    for (std::size_t i = 0; i < eligible.size(); ++i) {
      const int s = eligible[i];
      const double len = static_cast<double>(t - s);
      const double seg_sum = prefix[t] - prefix[s];
      const double seg_sq = prefix_sq[t] - prefix_sq[s];
      const double sse = seg_sq - (seg_sum * seg_sum) / len;
      double var_hat = sse / len;
      if (var_hat < eps) {
        var_hat = eps;
      }
      const double cost = len * std::log(var_hat);
      const double total = F[s] + cost + pen_value;
      totals[i] = total;
      if (total < best_total) {
        best_total = total;
        best_prev = s;
      }
    }

    F[t] = best_total;
    last_change[t] = best_prev;

    std::vector<int> new_candidates;
    new_candidates.reserve(candidates.size() + 1);
    for (int s : candidates) {
      if (t - s < min_seg_len) {
        new_candidates.push_back(s);
        continue;
      }
      for (std::size_t i = 0; i < eligible.size(); ++i) {
        if (eligible[i] == s) {
          if (totals[i] <= best_total + prune_tol) {
            new_candidates.push_back(s);
          }
          break;
        }
      }
    }
    new_candidates.push_back(t);
    std::sort(new_candidates.begin(), new_candidates.end());
    new_candidates.erase(std::unique(new_candidates.begin(), new_candidates.end()), new_candidates.end());
    candidates.swap(new_candidates);
  }

  std::vector<int> cps;
  cps.reserve(static_cast<std::size_t>(n / std::max(1, min_seg_len)));
  int current = n;
  while (current > 0) {
    const int prev = last_change[current];
    if (prev <= 0) {
      break;
    }
    cps.push_back(prev);
    current = prev;
  }
  std::reverse(cps.begin(), cps.end());

  return Rcpp::wrap(cps);
}
