# Detect flatline segments in a wearable time series

Detect flatline segments in a wearable time series

## Usage

``` r
detect_flatlines(x, tol = 1e-06, min_len = "15 min")
```

## Arguments

- x:

  A `wa_ts` tibble.

- tol:

  Numeric tolerance used when comparing consecutive signal values.

- min_len:

  Minimum duration (character string understood by
  [`base::as.difftime()`](https://rdrr.io/r/base/difftime.html)) that a
  flat segment must span to be reported.

## Value

A tibble with columns `id`, `start`, `end`, `duration`, `type`, `value`,
and `strength`. A zero-row tibble is returned when no flatlines are
detected.

## Examples

``` r
data <- toy_cgm(n_id = 1, n = 12)
ts <- as_wearable_ts(data, id = id, time = time, value = value)
ts$value[4:8] <- 125
detect_flatlines(ts, tol = 1e-5, min_len = "15 min")
#> # A tibble: 1 × 7
#>   start               end                 duration  type    value strength id   
#>   <dttm>              <dttm>              <drtn>    <chr>   <dbl>    <int> <chr>
#> 1 2025-01-01 00:15:00 2025-01-01 00:35:00 1200 secs flatli…   125        5 id01 
```
