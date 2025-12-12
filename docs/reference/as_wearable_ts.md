# Coerce data into a wearable-anomaly time series tibble

Coerce data into a wearable-anomaly time series tibble

## Usage

``` r
as_wearable_ts(
  data,
  id,
  time,
  value,
  tz = "UTC",
  lower = 40,
  upper = 400,
  cadence = NULL
)
```

## Arguments

- data:

  A data frame containing subject identifiers, timestamps, and signal
  values.

- id, time, value:

  Tidy-select expressions identifying the identifier, timestamp, and
  value columns respectively.

- tz:

  Time zone used when coercing the `time` column to POSIXct. Defaults to
  "UTC".

- lower, upper:

  Numeric bounds used when assessing valid signal values.

- cadence:

  Optional numeric cadence (in seconds) describing the expected sampling
  interval. Stored as metadata when supplied.

## Value

A tibble with class `wa_ts` and attributes `lower`, `upper`, `cadence`
(stored as `NA` when not supplied), `tz`, and `dups_dropped`.

## Examples

``` r
data <- toy_cgm(n_id = 2, n = 6)
wa <- as_wearable_ts(data, id = id, time = time, value = value)
validate_ts(wa)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: issue <chr>, id <chr>, time <dttm>, value <dbl>, details <chr>
```
