# wearableAnomaly Function Specs

Each entry records the intended inputs, outputs, invariants, and a minimal usage example so future contributors (human or AI) can reason about behaviour before editing code.

---

## `as_wearable_ts(data, id, time, value, tz = "UTC", lower = 40, upper = 400, cadence = NULL)`

- **Inputs**: Data frame with identifier, timestamp, numeric value columns (selected via tidy-eval arguments). Optional metadata: timezone, expected lower/upper bounds, cadence (seconds).
- **Outputs**: Tibble with columns `id`, `time` (POSIXct), `value`, classed `wa_ts` with attributes `lower`, `upper`, `cadence`, `tz`, `dups_dropped`.
- **Invariants**:
  - `time` sorted within each `id`; function reorders and warns if necessary.
  - Duplicate `(id, time)` rows removed; count stored in attribute.
  - If all values are `NA` -> error.
- **Minimal example**:
  ```r
  ts <- as_wearable_ts(toy_cgm(n_id = 1, n = 6), id = id, time = time, value = value)
  ```

## `validate_ts(x, lower = attr(x, "lower"), upper = attr(x, "upper"))`

- **Inputs**: `wa_ts` tibble.
- **Outputs**: Tibble of issues (`issue`, `id`, `time`, `value`, `details`). Zero rows when no problems.
- **Invariants**:
  - Checks required columns, POSIXct `time`, strictly increasing timestamps per `id`, duplicates, non-finite values, bound violations.
- **Minimal example**:
  ```r
  issues <- validate_ts(ts)
  has_issues(issues)
  ```

## `resample_series(x, by = "5 min", agg = c("mean","last","median"))`

- **Inputs**: `wa_ts`; cadence string convertible by `seq.POSIXt`.
- **Outputs**: `wa_ts` on regular grid; preserves metadata, updates `cadence`.
- **Invariants**:
  - Handles empty input (`nrow == 0`).
  - Within each `id`, bins observations, applies aggregation, retains `NA` bins.
- **Minimal example**:
  ```r
  resampled <- resample_series(ts, by = "5 min", agg = "mean")
  ```

## Artifact detectors (`detect_flatlines`, `detect_saturation`, `detect_rate_change`)

- **Inputs**: `wa_ts`; detector-specific parameters (tolerances, thresholds, min durations).
- **Outputs**: Tibble of episodes (`id`, `start`, `end`, `duration`, `type`, `value`, `strength`).
- **Invariants**:
  - Return zero-row tibble when no events or input empty.
  - `type` encodes detector, e.g., `"flatline"`, `"saturation_high"`, `"roc_up"`.
- **Minimal example**:
  ```r
  flats <- detect_flatlines(ts, tol = 1e-5, min_len = "15 min")
  ```

## `detect_changepoints_pelt(x, cost = c("meanvar","mean","poisson"), penalty = c("MBIC","BIC","AIC"), min_seg_len = 12)`

- **Inputs**: `wa_ts` with numeric `value`. Supports cost/penalty choices; min segment length ≥ 2.
- **Outputs**: Tibble (`id`, `cp_time`, `cp_index`, `new_level`, `new_var`, `method`, `penalty`).
- **Invariants**:
  - Drops `NA` values per id (warns with count).
  - Uses Rcpp core for `meanvar` when threshold met; otherwise R fallback.
- **Minimal example**:
  ```r
  cps <- detect_changepoints_pelt(ts, cost = "meanvar", penalty = "MBIC")
  ```

## `detect_changepoints_edivisive(x, min_seg_len = 12, R = 199, alpha = 0.05)`

- **Inputs**: `wa_ts`; permutation count `R`, significance level `alpha`.
- **Outputs**: Tibble (`id`, `cp_time`, `p_value`, `method = "edivisive"`).
- **Invariants**:
  - Recurses to split segments only when p-value ≤ `alpha`.
  - Automatically selects Rcpp backend when compiled symbol available.
- **Minimal example**:
  ```r
  ediv <- detect_changepoints_edivisive(ts, min_seg_len = 20, R = 99)
  ```

## `merge_segments(..., gap = "5 min")`

- **Inputs**: Detector outputs (tibbles) passed individually or as a named list; gap tolerance.
- **Outputs**: Tibble with standardized columns (`id`, `start`, `end`, `source`, `subtype`, `score`, `notes`, `components`).
- **Invariants**:
  - Standardizes schema even when start/end columns differ (`time`, `cp_time`).
  - Merges overlapping episodes within each id when gap ≤ tolerance.
- **Minimal example**:
  ```r
  merged <- merge_segments(flatlines = flats, pelt = cps, edivisive = ediv)
  ```

## `score_anomalies(segments, weights = NULL, combine = c("max","sum","vote"))`

- **Inputs**: Output of `merge_segments()`; optional weights per source, combination rule.
- **Outputs**: Same tibble with `score` updated and `raw_score` preserved.
- **Invariants**:
  - Normalizes scores within each source before combining.
  - `components` column left intact for downstream inspection.
- **Minimal example**:
  ```r
  scored <- score_anomalies(merged, weights = c(flatlines = 2), combine = "sum")
  ```

## `detect_anomalies(x, preset = c("research","clinical"), resample_by = "5 min", ...)`

- **Inputs**: `wa_ts`; preset controlling default weights/combination; optional overrides forwarded to merge/score helpers.
- **Outputs**: List with `segments`, `changepoints` (pelt & ediv), `artifacts`, `issues`, `plots`.
- **Invariants**:
  - Always validates input via `validate_ts()`; optionally resamples.
  - Uses default weights unless user supplies `weights`/`combine` via `...`.
- **Minimal example**:
  ```r
  res <- detect_anomalies(ts, preset = "research", gap = "10 min")
  ```

## `evaluate_methods(pred, truth, tolerance = "10 min", method = NULL, runtime_sec = NULL)`

- **Inputs**: Prediction outputs (a tibble or named list of tibbles with `id`, `start`, `end`, optional `runtime_sec` metadata) and a truth tibble with matching columns. `tolerance` controls allowable slack when matching episodes/changepoints.
- **Outputs**: Tibble with columns `method`, `runtime_sec`, `mean_n_cps`, `precision`, `recall`, `f1`, `mae_cp`, `iou`.
- **Invariants**:
  - Supports single or multi-method inputs; list names or provided metadata label each method.
  - Handles empty predictions and/or truth by returning zero changepoints and perfect precision/recall/IoU where appropriate.
  - Computes mean absolute error (`mae_cp`) over matched changepoint start times (minutes) and reports `NA` when no matches exist.
- **Minimal example**:
  ```r
  preds <- list(
    wearable_pelt = list(segments = wa_segments, runtime_sec = 0.42),
    changepoint = list(segments = cp_segments, runtime_sec = 0.88)
  )
  metrics <- evaluate_methods(pred = preds, truth = truth_segments, tolerance = "10 min")
  ```

---

Add new functions here with the same template before coding changes, so contributors and AI copilots have a quick reference.
