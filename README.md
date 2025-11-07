# wearableAnomaly

Tools for building anomaly detection workflows on top of wearable sensor data.

## Installation

The package is not on CRAN yet. Install the development version from GitHub using:

```r
# install.packages("pak")
pak::pak("neokok/wearableAnomaly")
```

Or with `remotes`:

```r
install.packages("remotes")
remotes::install_github("neokok/wearableAnomaly")
```

## Getting Started

```r
library(wearableAnomaly)

hello()
```

## Contributing

We welcome pull requests and feedback. Please file issues or feature requests on the GitHub tracker.


Function interface and expectations

All functions operate on long-format data:
|  Column | Type      | Description                             |
| ------: | --------- | --------------------------------------- |
|    `id` | character | Subject identifier                      |
|  `time` | POSIXct   | Timestamp (UTC recommended)             |
| `value` | numeric   | Sensor reading (e.g., glucose in mg/dL) |


Return objects are tibbles unless noted. Keep column names consistent across detectors.

1) as_wearable_ts()

Purpose: Coerce a long data frame to class wa_ts and attach bounds/cadence.
Input: data, column specs (id, time, value), tz="UTC", lower=40, upper=400, cadence=NULL.
Output: Tibble id,time,value with class wa_ts and attributes lower, upper, cadence, tz.

2) validate_ts()

Purpose: Report integrity issues in a wa_ts.
Input: x (wa_ts), optional lower, upper.
Output: Tibble with columns issue, id, time, value, details (0 rows if clean).

3) resample_series()

Purpose: Regularize to a fixed interval.
Input: x (wa_ts), by="5 min", agg in {"mean","last","median"}.
Output: wa_ts on an even grid.

4) detect_flatlines()

Purpose: Find constant runs.
Input: x (wa_ts), tol=1e-6, min_len="15 min".
Output: Tibble id, start, end, duration, type="flatline".

5) detect_saturation()

Purpose: Find sustained readings near bounds.
Input: x (wa_ts), lower, upper, min_len.
Output: Tibble id, start, end, duration, type="saturation_low" or "saturation_high".

6) detect_rate_change()

Purpose: Find unusually fast increases or decreases.
Input: x (wa_ts), window="15 min", threshold=3, scale in {"mad","sd","none"}.
Output: Tibble id, start, end, type="roc_up" or "roc_down", strength.

7) detect_changepoints_pelt()

Purpose: Mean or mean/variance changepoints via PELT.
Input: x (wa_ts), cost in {"meanvar","mean","poisson"}, penalty in {"MBIC","BIC","AIC"}, min_seg_len=12L.
Output: Tibble id, cp_time, cp_index, new_level, new_var, method="pelt", penalty.

8) detect_changepoints_edivisive()

Purpose: Distributional changepoints via E-divisive.
Input: x (wa_ts), min_seg_len=12L, R=199.
Output: Tibble id, cp_time, p_value, method="edivisive".

9) merge_segments()

Purpose: Merge overlapping or nearby segments from multiple detectors.
Input: One or more segment tibbles, gap window to merge.
Output: Tibble id, start, end, source, subtype, score, notes.

10) score_anomalies()

Purpose: Combine and normalize scores across detectors.
Input: segments, weights=NULL, combine in {"max","sum","vote"}.
Output: Same tibble with added score.

11) summarize_anomalies()

Purpose: Summaries per subject or per day.
Input: segments, by in {"id","day"}.
Output: Tibble of counts and durations by group.

12) detect_anomalies()

Purpose: One-call pipeline that orchestrates validation, resampling, detectors, merging, and scoring.
Input: x (raw or wa_ts), preset in {"research","clinical"}, resample_by="5 min", bounds.
Output: List with $segments, $changepoints, $artifacts, and optional $plots.

13) simulate_cgm()

Purpose: Synthetic CGM-like data with ground truth.
Input: n_id, days, by="5 min", seed, missing, dropout_runs, sat_frac.
Output: List $data (long df) and $truth (labeled segments).

14) evaluate_methods()

Purpose: Compare predictions to truth on simulated data.
Input: pred, truth, tolerance="10 min".
Output: Tibble with precision, recall, F1, and IoU per type.

15) autoplot.anomalies()

Purpose: Visual overlay plots for quick audit.
Input: Output of detect_anomalies(), overlay in {"changepoints","artifacts"}, n_id=9.
Output: A ggplot object.

Dependency map
Call order in the pipeline

as_wearable_ts()

validate_ts()

resample_series() (optional)

Detectors (any order):

detect_flatlines()

detect_saturation()

detect_rate_change()

detect_changepoints_pelt()

detect_changepoints_edivisive()

merge_segments()

score_anomalies()

summarize_anomalies() (optional)

autoplot.anomalies() (optional)

detect_anomalies() wraps steps 2–6 as a single call

Support flow

simulate_cgm() produces $data and $truth for testing and demos.

evaluate_methods() consumes predicted segments and $truth.

| Function                  | Calls internally                                                                     | Consumes                       | Produces                       |
| ------------------------- | ------------------------------------------------------------------------------------ | ------------------------------ | ------------------------------ |
| `detect_anomalies()`      | `validate_ts`, `resample_series`, all detectors, `merge_segments`, `score_anomalies` | raw or `wa_ts`                 | list: segments, cps, artifacts |
| `merge_segments()`        | —                                                                                    | detector outputs               | unified segments               |
| `score_anomalies()`       | —                                                                                    | merged segments                | merged segments + `score`      |
| `summarize_anomalies()`   | —                                                                                    | merged segments                | summary table                  |
| Detectors (`flatlines` …) | —                                                                                    | `wa_ts` or resampled `wa_ts`   | segment tibbles                |
| `resample_series()`       | —                                                                                    | `wa_ts`                        | `wa_ts`                        |
| `validate_ts()`           | —                                                                                    | `wa_ts`                        | issues tibble                  |
| `as_wearable_ts()`        | —                                                                                    | long df                        | `wa_ts`                        |
| `simulate_cgm()`          | —                                                                                    | params                         | `$data`, `$truth`              |
| `evaluate_methods()`      | —                                                                                    | predicted segments, `$truth`   | metrics tibble                 |
| `autoplot.anomalies()`    | —                                                                                    | result of `detect_anomalies()` | ggplot                         |


Notes for Copilot

Prefer pure R first, then Rcpp for hotspots (no OpenMP unless explicitly toggled).

Return tibbles with consistent schemas; include roxygen2 docs and runnable examples using synthetic data.

Add unit tests for each function.

For R→Rcpp translations, preserve signatures and numeric behavior, handle NA/NaN, preallocate, and avoid allocations in loops.