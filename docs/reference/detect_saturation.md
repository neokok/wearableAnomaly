# Detect saturation segments in a wearable time series

Detect saturation segments in a wearable time series

## Usage

``` r
detect_saturation(
  x,
  lower = attr(x, "lower"),
  upper = attr(x, "upper"),
  min_len = "10 min"
)
```

## Arguments

- x:

  A `wa_ts` tibble.

- lower, upper:

  Numeric bounds defining saturation thresholds. Defaults to the
  metadata carried by the `wa_ts` object.

- min_len:

  Minimum duration (character string) that a saturation run must span to
  be reported.

## Value

A tibble structured like
[`detect_flatlines()`](https://neokok.github.io/wearableAnomaly/reference/detect_flatlines.md)
with type values `"saturation_high"` or `"saturation_low"`.

## Examples

``` r
data <- toy_cgm(n_id = 1, n = 12)
ts <- as_wearable_ts(data, id = id, time = time, value = value)
ts$value[5:9] <- 405
detect_saturation(ts, min_len = "10 min")
#> # A tibble: 1 × 7
#>   start               end                 duration  type    value strength id   
#>   <dttm>              <dttm>              <drtn>    <chr>   <dbl>    <int> <chr>
#> 1 2025-01-01 00:20:00 2025-01-01 00:40:00 1200 secs satura…   405        5 id01 
```
