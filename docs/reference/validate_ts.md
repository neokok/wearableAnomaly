# Validate a wearable-anomaly time series object

Validate a wearable-anomaly time series object

## Usage

``` r
validate_ts(x, lower = attr(x, "lower"), upper = attr(x, "upper"))

has_issues(issues)
```

## Arguments

- x:

  A `wa_ts` object created by
  [`as_wearable_ts()`](https://neokok.github.io/wearableAnomaly/reference/as_wearable_ts.md).

- lower, upper:

  Numeric bounds used to flag out-of-range values. Defaults to the
  attributes stored on `x`.

- issues:

  Output tibble returned by `validate_ts()`.

## Value

A tibble recording detected issues with columns `issue`, `id`, `time`,
`value`, and `details`. A zero-row tibble is returned when no issues are
found.

## Examples

``` r
data <- toy_cgm(n_id = 1, n = 6)
ts <- as_wearable_ts(data, id = id, time = time, value = value)
validate_ts(ts)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: issue <chr>, id <chr>, time <dttm>, value <dbl>, details <chr>
```
