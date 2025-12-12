# Detect changepoints with the E-divisive algorithm

Implements a univariate version of the E-divisive procedure based on
energy distance to detect distributional changepoints. Candidate
changepoints are assessed via a permutation test and accepted when the
p-value is below `alpha`. Detected changepoints are then investigated
recursively on the left and right segments subject to the same minimum
segment length constraint.

## Usage

``` r
detect_changepoints_edivisive(x, min_seg_len = 12L, R = 199L, alpha = 0.05)
```

## Arguments

- x:

  A `wa_ts` tibble produced by
  [`as_wearable_ts()`](https://neokok.github.io/wearableAnomaly/reference/as_wearable_ts.md).

- min_seg_len:

  Minimum number of observations permitted in any segment.

- R:

  Number of permutations used to compute the reference distribution.

- alpha:

  Significance level required to accept a split (default `0.05`).

## Value

A tibble with columns `id`, `cp_time`, `p_value`, and `method`. The
tibble has zero rows when no changepoints are detected.

## Examples

``` r
set.seed(123)
n <- 180
times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
  by = "5 min", length.out = n
)
values <- c(
  rnorm(60, 100, 4),
  rnorm(60, 120, 6),
  rnorm(60, 110, 4)
)
ts <- as_wearable_ts(
  dplyr::tibble(id = "id01", time = times, value = values),
  id = id, time = time, value = value
)
detect_changepoints_edivisive(ts, min_seg_len = 12, R = 99)
#> # A tibble: 2 × 4
#>   id    cp_time             p_value method   
#>   <chr> <dttm>                <dbl> <chr>    
#> 1 id01  2025-01-01 04:55:00    0.01 edivisive
#> 2 id01  2025-01-01 09:50:00    0.01 edivisive
```
