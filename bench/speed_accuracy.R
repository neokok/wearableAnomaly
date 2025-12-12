#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(wearableAnomaly)
})

set.seed(615)

sim <- simulate_cgm_benchmark(
  n_series = 50,
  n_points = 288,
  min_seg_len = 12,
  n_cps = 4,
  seed = 615
)

truth_segments <- sim$truth
data_tbl <- sim$data

cp_available <- requireNamespace("changepoint", quietly = TRUE)
ecp_available <- requireNamespace("ecp", quietly = TRUE)
if (!cp_available) {
  message("Package 'changepoint' not available; skipping comparator PELT.")
}
if (!ecp_available) {
  message("Package 'ecp' not available; skipping comparator E-divisive.")
}

segments_from_times <- function(id, times) {
  if (length(times) == 0L) {
    return(dplyr::tibble(
      id = character(),
      start = as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC"),
      end = as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC")
    ))
  }
  dplyr::tibble(
    id = rep(id, length(times)),
    start = times,
    end = times
  )
}

method_store <- list(
  wearable_pelt = list(segments = list(), runtime = numeric()),
  changepoint_pelt = list(segments = list(), runtime = numeric()),
  wearable_ediv = list(segments = list(), runtime = numeric()),
  ecp_ediv = list(segments = list(), runtime = numeric())
)

ids <- unique(data_tbl$id)

for (id_val in ids) {
  series <- dplyr::filter(data_tbl, id == id_val)
  ts <- as_wearable_ts(series, id = id, time = time, value = value)

  # wearableAnomaly PELT
  pelt_time <- system.time({
    wa_pelt <- detect_changepoints_pelt(ts, min_seg_len = 12)
  })[["elapsed"]]
  cp_times <- if (nrow(wa_pelt) == 0) ts$time[FALSE] else wa_pelt$cp_time
  method_store$wearable_pelt$segments[[length(method_store$wearable_pelt$segments) + 1L]] <-
    segments_from_times(id_val, cp_times)
  method_store$wearable_pelt$runtime <- c(method_store$wearable_pelt$runtime, pelt_time)

  if (cp_available) {
    cp_time <- system.time({
      cp_fit <- changepoint::cpt.meanvar(series$value, method = "PELT", penalty = "MBIC", minseglen = 12)
      cp_idx <- changepoint::cpts(cp_fit)
    })[["elapsed"]]
    cp_times_pkg <- if (length(cp_idx) == 0) ts$time[FALSE] else ts$time[pmin(pmax(cp_idx, 1), nrow(series))]
    method_store$changepoint_pelt$segments[[length(method_store$changepoint_pelt$segments) + 1L]] <-
      segments_from_times(id_val, cp_times_pkg)
    method_store$changepoint_pelt$runtime <- c(method_store$changepoint_pelt$runtime, cp_time)
  }

  # wearableAnomaly E-divisive
  ed_time <- system.time({
    wa_ed <- detect_changepoints_edivisive(ts, min_seg_len = 12, R = 99)
  })[["elapsed"]]
  ed_times <- if (nrow(wa_ed) == 0) ts$time[FALSE] else wa_ed$cp_time
  method_store$wearable_ediv$segments[[length(method_store$wearable_ediv$segments) + 1L]] <-
    segments_from_times(id_val, ed_times)
  method_store$wearable_ediv$runtime <- c(method_store$wearable_ediv$runtime, ed_time)

  if (ecp_available) {
    ecp_time <- system.time({
      ecp_res <- ecp::e.divisive(matrix(series$value, ncol = 1), R = 99, sig.lvl = 0.05, min.size = 12)
    })[["elapsed"]]
    est_vec <- ecp_res$estimates
    if (length(est_vec) <= 2) {
      ecp_idx <- integer()
    } else {
      ecp_idx <- est_vec[-c(1, length(est_vec))]
    }
    ecp_times <- if (length(ecp_idx) == 0) ts$time[FALSE] else ts$time[pmin(pmax(ecp_idx, 1), nrow(series))]
    method_store$ecp_ediv$segments[[length(method_store$ecp_ediv$segments) + 1L]] <-
      segments_from_times(id_val, ecp_times)
    method_store$ecp_ediv$runtime <- c(method_store$ecp_ediv$runtime, ecp_time)
  }
}

assemble_entry <- function(entry) {
  segments <- if (length(entry$segments) == 0) {
    segments_from_times(character(), numeric())
  } else {
    dplyr::bind_rows(entry$segments)
  }
  list(
    segments = segments,
    runtime_sec = if (length(entry$runtime) == 0) NA_real_ else mean(entry$runtime)
  )
}

pred_entries <- list()
for (nm in names(method_store)) {
  entry <- method_store[[nm]]
  if (length(entry$segments) == 0) {
    next
  }
  pred_entries[[nm]] <- assemble_entry(entry)
}

metrics <- evaluate_methods(
  pred = pred_entries,
  truth = truth_segments,
  tolerance = "10 min"
)

print(metrics)

output_path <- file.path("bench", "speed_accuracy_summary.csv")
utils::write.csv(metrics, file = output_path, row.names = FALSE)
message("Benchmark summary written to ", output_path)
