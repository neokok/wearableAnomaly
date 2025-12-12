# Comparing R and Rcpp Implementations

## Motivation

`wearableAnomaly` ships both R and Rcpp implementations for its
computationally intensive routines. By default, the package selects the
native code path when the shared library is available, but you can
switch back to pure R for parity or teaching purposes. This vignette
illustrates:

1.  **PELT** vs. the reference
    [changepoint](https://github.com/rkillick/changepoint/)
    implementation.
2.  **E-divisive** vs. `{ecp}`.
3.  How to toggle between R and Rcpp backends and benchmark them.

> **Note**: Comparator packages are optional. Install them beforehand to
> run the chunks marked `eval = FALSE`.

## Setup

``` r

library(wearableAnomaly)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
set.seed(20251118)
series <- toy_cgm(n_id = 1, n = 300, by = "5 min")
ts <- as_wearable_ts(series, id = id, time = time, value = value)
```

## Toggling the backend

Two options control backend selection:

``` r

options(
  wearableAnomaly.use_rcpp = TRUE,     # enable/disable native code
  wearableAnomaly.rcpp_threshold = 1L  # minimum n to trigger native path
)
```

Setting `wearableAnomaly.use_rcpp = FALSE` forces the pure R fallback,
which is useful for debugging or machines without compilers.

## PELT vs `{changepoint}`

``` r

library(changepoint)
pelt_pkg <- detect_changepoints_pelt(ts, min_seg_len = 15, penalty = "MBIC")
cpt_pkg  <- changepoint::cpt.meanvar(ts$value, method = "PELT", penalty = "MBIC", minseglen = 15)

all.equal(pelt_pkg$cp_index, changepoint::cpts(cpt_pkg))
```

### Benchmark (optional)

``` r

library(bench)
bench::mark(
  wearable = detect_changepoints_pelt(ts, min_seg_len = 15),
  changepoint = changepoint::cpt.meanvar(ts$value, method = "PELT", penalty = "MBIC", minseglen = 15),
  iterations = 5
)
```

The Rcpp core (`src/pelt_core.cpp`) uses prefix sums and pruning to
match the reference algorithm’s accuracy while reducing allocations and
loop overhead.

## E-divisive vs `{ecp}`

``` r

library(ecp)
ediv_pkg <- detect_changepoints_edivisive(ts, min_seg_len = 20, R = 199)
ecp_pkg  <- ecp::e.divisive(matrix(ts$value, ncol = 1), sig.lvl = 0.05, R = 199, beta = 0.1)
ecp_cpts <- ecps_pkg$estimates[-c(1, length(ecp_pkg$estimates))]

match_idx <- match(ediv_pkg$cp_index, ecp_cpts)
all(!is.na(match_idx))
```

### Benchmark (optional)

``` r

bench::mark(
  wearable = detect_changepoints_edivisive(ts, min_seg_len = 20, R = 99),
  ecp = ecp::e.divisive(matrix(ts$value, ncol = 1), sig.lvl = 0.05, R = 99, beta = 0.05),
  iterations = 3
)
```

The Rcpp helper (`energy_stat_segment()`) precomputes pairwise distances
and reuses them during permutation tests, resulting in substantial
runtime savings while matching `{ecp}` across seeds.

## Inspecting backend choice

You can confirm which path is active with the exported helper:

``` r

wearableAnomaly::: .ed_cpp_available()
#> [1] TRUE
```

If `FALSE`, ensure the package was compiled (`R CMD INSTALL .`), or set
`wearableAnomaly.use_rcpp = FALSE` to avoid warnings on machines without
build tools.

## Reproducing the tests

The parity and timing tests used for development live under
`tests/testthat/test_pelt.R` and `tests/testthat/test_edivisive.R`. Run
them directly with:

``` r

devtools::test(filter = "pelt|edivisive")
```

They automatically skip when
[changepoint](https://github.com/rkillick/changepoint/), `{ecp}`, or the
Rcpp backend are unavailable.
