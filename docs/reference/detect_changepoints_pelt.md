# Detect changepoints in wearable signals using the PELT algorithm

Detect changepoints in wearable signals using the PELT algorithm

## Usage

``` r
detect_changepoints_pelt(
  x,
  cost = c("meanvar", "mean", "poisson"),
  penalty = c("MBIC", "BIC", "AIC"),
  min_seg_len = 12L
)
```

## Arguments

- x:

  A `wa_ts` tibble produced by
  [`as_wearable_ts()`](https://neokok.github.io/wearableAnomaly/reference/as_wearable_ts.md).

- cost:

  Cost function to optimise. Choose from `"meanvar"` (default),
  `"mean"`, or `"poisson"`.

- penalty:

  Penalty strategy. Options are `"MBIC"`, `"BIC"`, and `"AIC"`.

- min_seg_len:

  Minimum number of observations permitted in any segment.

## Value

A tibble with the detected changepoints containing columns `id`,
`cp_time`, `cp_index`, `new_level`, `new_var`, `method`, and `penalty`.
Returns a zero-row tibble with these columns when no changepoints are
detected.

## Examples

``` r
set.seed(123)
n <- 240
times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
                    by = "5 min", length.out = n)
values <- c(rnorm(120, mean = 100, sd = 5), rnorm(120, mean = 130, sd = 5))
data <- dplyr::tibble(id = "id01", time = times, value = values)
ts <- as_wearable_ts(data, id = id, time = time, value = value)
detect_changepoints_pelt(ts, cost = "meanvar", penalty = "MBIC")
#> # A tibble: 1 × 7
#>   id    cp_time             cp_index new_level new_var method penalty
#>   <chr> <dttm>                 <int>     <dbl>   <dbl> <chr>  <chr>  
#> 1 id01  2025-01-01 09:55:00      120      130.    24.2 pelt   MBIC   
```
