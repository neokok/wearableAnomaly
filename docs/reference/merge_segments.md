# Merge heterogeneous anomaly detections

Standardises the outputs of the artifact detectors, changepoint
detectors, and other anomaly routines into a common schema before
merging overlapping segments within each subject. The merged output
includes a `components` list column that records the contributing
detections for downstream scoring.

## Usage

``` r
merge_segments(..., gap = "5 min")
```

## Arguments

- ...:

  Individual detection results as data frames/tibbles or a single named
  list. Objects can come directly from
  [`detect_flatlines()`](https://neokok.github.io/wearableAnomaly/reference/detect_flatlines.md),
  [`detect_saturation()`](https://neokok.github.io/wearableAnomaly/reference/detect_saturation.md),
  [`detect_rate_change()`](https://neokok.github.io/wearableAnomaly/reference/detect_rate_change.md),
  [`detect_changepoints_pelt()`](https://neokok.github.io/wearableAnomaly/reference/detect_changepoints_pelt.md),
  or
  [`detect_changepoints_edivisive()`](https://neokok.github.io/wearableAnomaly/reference/detect_changepoints_edivisive.md).

- gap:

  Maximum gap tolerated between neighbouring detections (per id) before
  they are collapsed into a single segment. Can be a character string
  understood by
  [`base::as.difftime()`](https://rdrr.io/r/base/difftime.html) or a
  numeric number of seconds.

## Value

A tibble with columns `id`, `start`, `end`, `source`, `subtype`,
`score`, `notes`, and `components`. The `components` column stores the
underlying detections contributing to each merged segment.
