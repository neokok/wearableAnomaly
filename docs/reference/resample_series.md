# Resample a wearable time series onto an even cadence

Resample a wearable time series onto an even cadence

## Usage

``` r
resample_series(x, by = "5 min", agg = c("mean", "last", "median"))
```

## Arguments

- x:

  A `wa_ts` tibble produced by
  [`as_wearable_ts()`](https://neokok.github.io/wearableAnomaly/reference/as_wearable_ts.md).

- by:

  Character string understood by
  [`base::seq.POSIXt()`](https://rdrr.io/r/base/seq.POSIXt.html) that
  defines the target sampling cadence (default "5 min").

- agg:

  Aggregation strategy applied when multiple observations fall into the
  same resampling window. Choose from "mean", "last", or "median".

## Value

A `wa_ts` tibble sampled on the requested cadence. Metadata attributes
`lower`, `upper`, `cadence`, `tz`, and `dups_dropped` are preserved,
with `cadence` updated to the new cadence in seconds.

## Examples

``` r
data <- toy_cgm(n_id = 1, n = 8, by = "7 min")
ts <- as_wearable_ts(data, id = id, time = time, value = value)
resampled <- resample_series(ts, by = "5 min", agg = "mean")
```
