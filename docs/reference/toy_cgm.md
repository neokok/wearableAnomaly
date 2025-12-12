# Generate a small synthetic CGM-like data set

Generate a small synthetic CGM-like data set

## Usage

``` r
toy_cgm(n_id = 2, n = 12, by = "5 min", seed = 1)
```

## Arguments

- n_id:

  Number of unique subjects to simulate.

- n:

  Number of observations per subject.

- by:

  Interval passed to
  [`base::seq.POSIXt()`](https://rdrr.io/r/base/seq.POSIXt.html) for
  timestamp spacing.

- seed:

  Optional integer seed for reproducible noise.

## Value

A tibble with columns `id`, `time`, and `value` suitable for examples
and unit tests.

## Examples

``` r
toy_cgm()
#> # A tibble: 24 × 3
#>    id    time                value
#>    <chr> <dttm>              <dbl>
#>  1 id01  2025-01-01 00:00:00   117
#>  2 id01  2025-01-01 00:05:00   128
#>  3 id01  2025-01-01 00:10:00   126
#>  4 id01  2025-01-01 00:15:00   135
#>  5 id01  2025-01-01 00:20:00   122
#>  6 id01  2025-01-01 00:25:00   106
#>  7 id01  2025-01-01 00:30:00   102
#>  8 id01  2025-01-01 00:35:00    96
#>  9 id01  2025-01-01 00:40:00    93
#> 10 id01  2025-01-01 00:45:00    91
#> # ℹ 14 more rows
```
