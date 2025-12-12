# Package index

## Package overview

- [`wearableAnomaly`](https://neokok.github.io/wearableAnomaly/reference/wearableAnomaly-package.md)
  [`wearableAnomaly-package`](https://neokok.github.io/wearableAnomaly/reference/wearableAnomaly-package.md)
  : wearableAnomaly: Tools for Wearable Device Anomaly Detection

## Preprocessing & validation

- [`as_wearable_ts()`](https://neokok.github.io/wearableAnomaly/reference/as_wearable_ts.md)
  : Coerce data into a wearable-anomaly time series tibble
- [`validate_ts()`](https://neokok.github.io/wearableAnomaly/reference/validate_ts.md)
  [`has_issues()`](https://neokok.github.io/wearableAnomaly/reference/validate_ts.md)
  : Validate a wearable-anomaly time series object
- [`resample_series()`](https://neokok.github.io/wearableAnomaly/reference/resample_series.md)
  : Resample a wearable time series onto an even cadence

## Artifact detectors

- [`detect_flatlines()`](https://neokok.github.io/wearableAnomaly/reference/detect_flatlines.md)
  : Detect flatline segments in a wearable time series
- [`detect_saturation()`](https://neokok.github.io/wearableAnomaly/reference/detect_saturation.md)
  : Detect saturation segments in a wearable time series
- [`detect_rate_change()`](https://neokok.github.io/wearableAnomaly/reference/detect_rate_change.md)
  : Detect rate-of-change anomalies in a wearable time series

## Changepoint detectors

- [`detect_changepoints_pelt()`](https://neokok.github.io/wearableAnomaly/reference/detect_changepoints_pelt.md)
  : Detect changepoints in wearable signals using the PELT algorithm
- [`detect_changepoints_edivisive()`](https://neokok.github.io/wearableAnomaly/reference/detect_changepoints_edivisive.md)
  : Detect changepoints with the E-divisive algorithm

## Pipeline & scoring

- [`detect_anomalies()`](https://neokok.github.io/wearableAnomaly/reference/detect_anomalies.md)
  : High-level anomaly detection pipeline
- [`merge_segments()`](https://neokok.github.io/wearableAnomaly/reference/merge_segments.md)
  : Merge heterogeneous anomaly detections
- [`score_anomalies()`](https://neokok.github.io/wearableAnomaly/reference/score_anomalies.md)
  : Score merged anomaly segments

## Evaluation & simulation

- [`evaluate_methods()`](https://neokok.github.io/wearableAnomaly/reference/evaluate_methods.md)
  : Evaluate predicted anomaly segments
- [`toy_cgm()`](https://neokok.github.io/wearableAnomaly/reference/toy_cgm.md)
  : Generate a small synthetic CGM-like data set
- [`simulate_cgm_benchmark()`](https://neokok.github.io/wearableAnomaly/reference/simulate_cgm_benchmark.md)
  : Simulate CGM benchmark series with ground-truth changepoints
