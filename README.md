# wearableAnomaly

R tooling for ingesting continuous glucose monitor (CGM)–style signals, spotting artifacts, and flagging changepoints in a reproducible pipeline.

## Overview

- **Preprocess & validate**: `toy_cgm()` provides tiny demo series; `as_wearable_ts()` standardizes `<id, time, value>` data with bounds metadata; `validate_ts()` and `resample_series()` clean and regularize inputs.
- **Artifact detectors**: `detect_flatlines()`, `detect_saturation()`, and `detect_rate_change()` emit tidy episode tables (start/end/duration/type/strength) for common sensor glitches.
- **Changepoints**: `detect_changepoints_pelt()` (mean/variance or Poisson costs) and `detect_changepoints_edivisive()` (energy-distance with permutations) surface structural shifts. Rcpp backends are selected automatically when compiled.
- **Pipeline**: `detect_anomalies()` orchestrates validation, optional resampling, detectors, changepoints, `merge_segments()`, and `score_anomalies()` to produce a unified table ready for benchmarking or plotting. `evaluate_methods()` compares predictions to truth labels when available.

## Installation

The package is not on CRAN. Install the development version from GitHub with whichever helper you prefer:

```r
# install.packages("pak")
pak::pak("neokok/wearableAnomaly")
```

or

```r
install.packages("remotes")
remotes::install_github("neokok/wearableAnomaly")
```

## Quick start

```r
library(dplyr)
library(wearableAnomaly)

# 1) Simulate one CGM-like subject and coerce to the wa_ts class
raw <- toy_cgm(n_id = 1, n = 96, by = "5 min", seed = 2025)
ts <- as_wearable_ts(raw, id = id, time = time, value = value)

# 2) Optional resampling (already 5 min cadence here, but shown for completeness)
ts5 <- resample_series(ts, by = "5 min", agg = "mean")

# 3) Run artifact and changepoint detectors
flat <- detect_flatlines(ts5, min_len = "20 min")
sat  <- detect_saturation(ts5, min_len = "15 min")
pelt <- detect_changepoints_pelt(ts5, penalty = "MBIC", min_seg_len = 12)

# 4) One-call pipeline with scoring
results <- detect_anomalies(ts, preset = "research", gap = "10 min")

# Peek at merged/scored segments
results$segments %>%
  select(id, start, end, source, subtype, score) %>%
  head()
```

The detectors all return tibbles, so you can `dplyr::arrange()`/`plot()`/export them directly or feed them into evaluation utilities.

## Documentation

- **Pkgdown site**: rebuildable HTML reference under [`docs/`](docs/index.html).
- **Vignettes**: `vignettes/workflow.Rmd` for the end-to-end analysis and `vignettes/rcpp-comparison.Rmd` for R vs. Rcpp backend details.
- **Benchmarks**: `bench/bench_pelt.R` contains the interactive PELT timing script; expanded benchmark articles (including speed/accuracy tables) live or will live under `docs/articles/`.
- **GenAI tutorial**: Read the [`GenAI workflow tutorial`](https://neokok.github.io/wearableAnomaly/articles/genai-tutorial.html) for the exact prompts, tooling, screenshots, and lessons learned from building the package with AI assistance; the underlying source lives in `vignettes/genai-tutorial.Rmd`.

## Function reference (current exports)

| Category           | Functions                                                                                                             |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------- |
| Preprocess/check   | `toy_cgm()`, `as_wearable_ts()`, `validate_ts()`, `has_issues()`, `resample_series()`                                  |
| Artifact detectors | `detect_flatlines()`, `detect_saturation()`, `detect_rate_change()`                                                    |
| Changepoints       | `detect_changepoints_pelt()`, `detect_changepoints_edivisive()` (with automatic R / Rcpp backends)                     |
| Pipeline helpers   | `merge_segments()`, `score_anomalies()`, `detect_anomalies()`                                                          |
| Evaluation         | `evaluate_methods()`                                                                                                   |

All detector outputs follow a consistent tidy schema, making it straightforward to merge them, compute benchmarks, and reproduce the “speed & accuracy” tables from the final presentation.

## Future work

For later exploration (not part of this release):

- Richer simulators that go beyond `toy_cgm()`/`simulate_cgm_benchmark()` (e.g., multi-day truth grids).
- Higher-level summaries of anomaly runs for reporting dashboards.
- Additional visualization sugar (e.g., reusable overlay helpers) to complement the custom ggplot code shown in the vignette.

## Contributing

Issues and pull requests are welcome. Please open a ticket if you run into installation problems or have ideas for additional detectors, simulators, or tutorials.
