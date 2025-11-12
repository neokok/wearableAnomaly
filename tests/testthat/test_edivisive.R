base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")

make_ts_data <- function(values, id = "id01", by = "5 min") {
  times <- seq.POSIXt(base_time, by = by, length.out = length(values))
  ts_tbl <- as_wearable_ts(
    dplyr::tibble(id = id, time = times, value = values),
    id = id, time = time, value = value
  )
  list(ts_tbl = ts_tbl, times = times)
}

test_that("E-divisive detects a single variance change", {
  set.seed(20250101)
  n <- 240
  values <- c(rnorm(n / 2, 0, 1), rnorm(n / 2, 0, 2))
  series <- make_ts_data(values)

  res <- detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 12, R = 49)
  expect_equal(nrow(res), 1L)

  true_time <- series$times[n / 2]
  diff_minutes <- abs(as.numeric(difftime(res$cp_time, true_time, units = "mins")))
  expect_lte(diff_minutes, 10)
})

test_that("E-divisive detects a pure mean shift", {
  set.seed(20250102)
  n <- 200
  values <- c(rnorm(n / 2, -5, 1), rnorm(n / 2, 5, 1))
  series <- make_ts_data(values)

  res <- detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 12, R = 49)
  expect_equal(nrow(res), 1L)
  diff_minutes <- abs(as.numeric(difftime(res$cp_time, series$times[n / 2], units = "mins")))
  expect_lte(diff_minutes, 10)
})

test_that("E-divisive detects two changepoints with varying variances", {
  set.seed(20250103)
  values <- c(
    rnorm(80, 0, 1),
    rnorm(80, 0, 4),
    rnorm(80, 0, 1)
  )
  series <- make_ts_data(values)

  res <- detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 20, R = 39)
  expect_equal(nrow(res), 2L)

  expected_times <- series$times[c(80, 160)]
  diffs <- abs(as.numeric(difftime(res$cp_time, expected_times, units = "mins")))
  expect_true(all(diffs <= 10))
})

test_that("E-divisive returns empty tibble when no change exists", {
  set.seed(20250104)
  series <- make_ts_data(rnorm(180, 0, 1))
  res <- detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 12, R = 39)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0L)
})

test_that("E-divisive reports small p-values for strong changes", {
  set.seed(20250105)
  values <- c(rnorm(100, 0, 1), rnorm(100, 8, 1))
  series <- make_ts_data(values)

  res <- detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 15, R = 99)
  expect_equal(nrow(res), 1L)
  expect_lte(res$p_value, 1 / (99 + 1))
})

test_that("E-divisive handles edge cases and missing values", {
  set.seed(20250106)
  short_series <- make_ts_data(rnorm(20, 0, 1))
  expect_error(
    detect_changepoints_edivisive(short_series[["ts_tbl"]], min_seg_len = 12, R = 19),
    "at least"
  )

  values <- c(rnorm(60, 0, 1), NA_real_, rnorm(60, 4, 1))
  series <- make_ts_data(values)
  expect_warning(
    res <- detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 15, R = 39),
    "Dropped"
  )
  expect_equal(nrow(res), 1L)
})

test_that("E-divisive output schema matches expectations", {
  set.seed(20250107)
  values <- c(rnorm(70, -2, 1.5), rnorm(70, 2, 1.2))
  series <- make_ts_data(values)
  res <- detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 12, R = 39)

  expect_true(all(colnames(res) == c("id", "cp_time", "p_value", "method")))
  expect_s3_class(res, "tbl_df")
  if (nrow(res) > 0) {
    expect_true(all(res$method == "edivisive"))
    expect_true(all(res$p_value >= 0 & res$p_value <= 1))
  }
})

test_that("E-divisive aligns with ecp::e.divisive when available", {
  skip_if_not_installed("ecp")
  ecp_fn <- getExportedValue("ecp", "e.divisive")
  set.seed(20250108)
  values <- c(
    rnorm(90, 0, 1),
    rnorm(70, 3, 1.5),
    rnorm(70, -1, 1)
  )
  series <- make_ts_data(values)

  res <- detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 20, R = 29, alpha = 0.1)
  expect_true(nrow(res) >= 1)

  ecp_fit <- ecp_fn(X = matrix(values, ncol = 1), sig.lvl = 0.1, R = 29, beta = 0.05)
  ecp_cps <- ecp_fit$estimates
  if (length(ecp_cps) <= 2) {
    skip("ecp did not report any changepoints for this seed.")
  }
  ecp_cps <- ecp_cps[-c(1, length(ecp_cps))]

  res_idx <- match(res$cp_time, series$times)
  k <- min(length(res_idx), length(ecp_cps))
  expect_true(k > 0)
  expect_true(all(abs(res_idx[seq_len(k)] - ecp_cps[seq_len(k)]) <= 1))
})

test_that("E-divisive R and Rcpp backends agree", {
  skip_if_not(getFromNamespace(".ed_cpp_available", "wearableAnomaly")())

  seeds <- c(11, 42, 128)
  for (s in seeds) {
    set.seed(s)
    values <- c(rnorm(90, 0, 1), rnorm(90, 3, 1.5), rnorm(90, -1, 1))
    series <- make_ts_data(values)

    force_opts <- function(backend) {
      if (identical(backend, "r")) {
        list(wearableAnomaly.use_rcpp = FALSE, wearableAnomaly.rcpp_threshold = .Machine$integer.max)
      } else {
        list(wearableAnomaly.use_rcpp = TRUE, wearableAnomaly.rcpp_threshold = 1L)
      }
    }

    old <- options(force_opts("r"))
    on.exit(options(old), add = TRUE)
    set.seed(s)
    res_r <- detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 15, R = 49)

    old2 <- options(force_opts("rcpp"))
    on.exit(options(old2), add = TRUE)
    set.seed(s)
    res_cpp <- detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 15, R = 49)

    expect_equal(nrow(res_r), nrow(res_cpp))
    if (nrow(res_r) > 0) {
      idx_r <- match(res_r$cp_time, series$times)
      idx_cpp <- match(res_cpp$cp_time, series$times)
      expect_equal(length(idx_r), length(idx_cpp))
      expect_true(all(abs(idx_r - idx_cpp) <= 1))
      expect_equal(res_r$p_value, res_cpp$p_value, tolerance = 1e-8)
    }
  }
})

test_that("E-divisive Rcpp backend benchmark is reported", {
  skip_if_not(getFromNamespace(".ed_cpp_available", "wearableAnomaly")())
  skip_on_cran()
  if (nzchar(Sys.getenv("CI", ""))) {
    skip("Skipping timing comparison on CI.")
  }

  set.seed(20250110)
  values <- c(rnorm(200, 0, 1), rnorm(200, 4, 1.2))
  series <- make_ts_data(values)

  opts_r <- options(
    wearableAnomaly.use_rcpp = FALSE,
    wearableAnomaly.rcpp_threshold = .Machine$integer.max
  )
  on.exit(options(opts_r), add = TRUE)
  set.seed(99)
  time_r <- system.time(detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 20, R = 39))["elapsed"]

  opts_cpp <- options(
    wearableAnomaly.use_rcpp = TRUE,
    wearableAnomaly.rcpp_threshold = 1L
  )
  on.exit(options(opts_cpp), add = TRUE)
  set.seed(99)
  time_cpp <- system.time(detect_changepoints_edivisive(series[["ts_tbl"]], min_seg_len = 20, R = 39))["elapsed"]

  cat(sprintf("E-divisive timing (seconds): R=%.3f, Rcpp=%.3f\n", time_r, time_cpp))
  expect_true(is.finite(time_r + time_cpp))
})
