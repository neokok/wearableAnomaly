# Simulate CGM benchmark series with ground-truth changepoints

Simulate CGM benchmark series with ground-truth changepoints

## Usage

``` r
simulate_cgm_benchmark(
  n_series = 10L,
  n_points = 288L,
  min_seg_len = 12L,
  n_cps = 3L,
  seed = NULL,
  start_time = as.POSIXct("2025-01-01 00:00:00", tz = "UTC"),
  dt_minutes = 5
)
```

## Arguments

- n_series:

  Number of independent subjects to simulate.

- n_points:

  Number of observations per subject (default 288 = 24h at 5 minute
  cadence).

- min_seg_len:

  Minimum segment length between changepoints (in samples).

- n_cps:

  Nominal number of changepoints to embed per subject.

- seed:

  Optional integer for reproducibility.

- start_time:

  POSIXct timestamp of the first observation.

- dt_minutes:

  Cadence in minutes between readings.

## Value

A list with two tibbles: `data` containing columns `id`, `time`,
`value`; and `truth` containing `id`, `start`, and `end` (changepoint
times).

## Examples

``` r
sim <- simulate_cgm_benchmark(n_series = 2, n_points = 96, n_cps = 2, seed = 42)
names(sim)
#> [1] "data"  "truth"
```
