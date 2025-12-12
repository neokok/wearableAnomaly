# High-level anomaly detection pipeline

Runs the validation, optional resampling, artifact detectors,
changepoint detectors, and scoring stages in a single convenience
wrapper.

## Usage

``` r
detect_anomalies(
  x,
  preset = c("research", "clinical"),
  resample_by = "5 min",
  ...
)
```

## Arguments

- x:

  A `wa_ts` object or data frame coerced via
  [`as_wearable_ts()`](https://neokok.github.io/wearableAnomaly/reference/as_wearable_ts.md).

- preset:

  Pipeline preset controlling weighting and combination strategy.
  `"research"` emphasises sensitivity via score summation whereas
  `"clinical"` emphasises specificity via max pooling.

- resample_by:

  Optional cadence passed to
  [`resample_series()`](https://neokok.github.io/wearableAnomaly/reference/resample_series.md).
  Set to `NULL` to skip resampling.

- ...:

  Additional arguments forwarded to
  [`merge_segments()`](https://neokok.github.io/wearableAnomaly/reference/merge_segments.md)
  or
  [`score_anomalies()`](https://neokok.github.io/wearableAnomaly/reference/score_anomalies.md)
  when the argument names match those functions.

## Value

A list containing merged `segments`, raw `changepoints`, raw
`artifacts`, validation `issues`, and (placeholder) `plots`.
