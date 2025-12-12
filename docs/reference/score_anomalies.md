# Score merged anomaly segments

Normalises scores within each detector source and combines them across
sources using the requested strategy.

## Usage

``` r
score_anomalies(segments, weights = NULL, combine = c("max", "sum", "vote"))
```

## Arguments

- segments:

  Output from
  [`merge_segments()`](https://neokok.github.io/wearableAnomaly/reference/merge_segments.md).

- weights:

  Optional named numeric vector giving source-specific weights.
  Unspecified sources default to 1.

- combine:

  Combination strategy: `"max"` (default), `"sum"`, or `"vote"`.

## Value

The input tibble with the `score` column updated to reflect the combined
score. A column `raw_score` is added to retain the pre-combined values.
