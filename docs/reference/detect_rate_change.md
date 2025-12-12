# Detect rate-of-change anomalies in a wearable time series

Detect rate-of-change anomalies in a wearable time series

## Usage

``` r
detect_rate_change(
  x,
  window = "15 min",
  threshold = 3,
  scale = c("mad", "sd", "none")
)
```

## Arguments

- x:

  A `wa_ts` tibble.

- window:

  Character string describing the maximum separation between points used
  to compute rate-of-change.

- threshold:

  Numeric threshold applied to the scaled rate-of-change.

- scale:

  Scaling strategy: MAD, standard deviation, or "none".

## Value

A tibble like
[`detect_flatlines()`](https://neokok.github.io/wearableAnomaly/reference/detect_flatlines.md)
with `type` values `"roc_up"` or `"roc_down"` and a `strength` column
containing the maximum scaled deviation within each segment.

## Examples

``` r
data <- toy_cgm(n_id = 1, n = 12)
ts <- as_wearable_ts(data, id = id, time = time, value = value)
ts$value[6] <- ts$value[5] + 60
detect_rate_change(ts, window = "15 min", threshold = 3)
#> # A tibble: 2 × 7
#>   start               end                 duration type     value strength id   
#>   <dttm>              <dttm>              <drtn>   <chr>    <dbl>    <dbl> <chr>
#> 1 2025-01-01 00:20:00 2025-01-01 00:25:00 300 secs roc_up     152     5.45 id01 
#> 2 2025-01-01 00:25:00 2025-01-01 00:30:00 300 secs roc_down   142     7.27 id01 
```
