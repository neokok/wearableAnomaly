test_that("PELT detects single change in meanvar", {
  set.seed(20251111)
  n <- 240
  times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"), by = "5 min", length.out = n)
  values <- c(rnorm(120, mean = 100, sd = 5), rnorm(120, mean = 130, sd = 5))
  data <- dplyr::tibble(id = "id01", time = times, value = values)
  ts <- as_wearable_ts(data, id = id, time = time, value = value)

  res <- detect_changepoints_pelt(ts, cost = "meanvar", penalty = "MBIC", min_seg_len = 12)
  expect_equal(nrow(res), 1L)
  expect_equal(res$method, "pelt")
  expect_equal(res$penalty, "MBIC")

  true_time <- times[120]
  diff_minutes <- abs(as.numeric(difftime(res$cp_time, true_time, units = "mins")))
  expect_lte(diff_minutes, 10)
  expect_true(abs(res$new_level - 130) < 5)
})

test_that("PELT detects two changes", {
  set.seed(42)
  n <- 360
  times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"), by = "5 min", length.out = n)
  values <- c(
    rnorm(120, mean = 100, sd = 4),
    rnorm(120, mean = 130, sd = 4),
    rnorm(120, mean = 110, sd = 4)
  )
  data <- dplyr::tibble(id = "id01", time = times, value = values)
  ts <- as_wearable_ts(data, id = id, time = time, value = value)

  res <- detect_changepoints_pelt(ts, cost = "meanvar", penalty = "MBIC", min_seg_len = 12)
  expect_equal(nrow(res), 2L)
  true_times <- times[c(120, 240)]
  diffs <- abs(as.numeric(difftime(res$cp_time, true_times, units = "mins")))
  expect_true(all(diffs <= 10))
})

test_that("PELT returns empty tibble when no change", {
  set.seed(99)
  n <- 120
  times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"), by = "5 min", length.out = n)
  values <- rnorm(n, mean = 100, sd = 5)
  data <- dplyr::tibble(id = "id01", time = times, value = values)
  ts <- as_wearable_ts(data, id = id, time = time, value = value)

  res <- detect_changepoints_pelt(ts, cost = "meanvar", penalty = "MBIC", min_seg_len = 12)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
})

test_that("PELT detects Poisson rate change", {
  set.seed(123)
  n <- 240
  times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"), by = "5 min", length.out = n)
  counts <- c(rpois(120, lambda = 5), rpois(120, lambda = 12))
  data <- dplyr::tibble(id = "id01", time = times, value = counts)
  ts <- as_wearable_ts(data, id = id, time = time, value = value)

  res <- detect_changepoints_pelt(ts, cost = "poisson", penalty = "BIC", min_seg_len = 12)
  expect_equal(nrow(res), 1)
  true_time <- times[120]
  diff_minutes <- abs(as.numeric(difftime(res$cp_time, true_time, units = "mins")))
  expect_true(all(diff_minutes <= 15))
})

test_that("PELT handles short series and missing values appropriately", {
  times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"), by = "5 min", length.out = 20)
  values <- rnorm(20)
  data <- dplyr::tibble(id = "id01", time = times, value = values)
  ts <- as_wearable_ts(data, id = id, time = time, value = value)
  expect_error(detect_changepoints_pelt(ts, min_seg_len = 12), "at least")

  times2 <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"), by = "5 min", length.out = 240)
  values2 <- c(rnorm(120, 100, 5), rnorm(120, 130, 5))
  values2[c(10, 150)] <- NA_real_
  data2 <- dplyr::tibble(id = "id01", time = times2, value = values2)
  ts2 <- as_wearable_ts(data2, id = id, time = time, value = value)
  expect_warning(res <- detect_changepoints_pelt(ts2, cost = "meanvar", penalty = "MBIC"), "Dropped")
  expect_equal(nrow(res), 1)
})

test_that("PELT output schema is correct", {
  set.seed(101)
  data <- toy_cgm(n_id = 1, n = 120)
  data$value[61:120] <- data$value[61:120] + 20
  ts <- as_wearable_ts(data, id = id, time = time, value = value)

  res <- detect_changepoints_pelt(ts, cost = "meanvar", penalty = "AIC", min_seg_len = 12)
  expect_true(all(colnames(res) == c("id", "cp_time", "cp_index", "new_level", "new_var", "method", "penalty")))
  expect_s3_class(res, "tbl_df")
})

test_that("PELT matches changepoint package within tolerance", {
  skip_if_not_installed("changepoint")
  set.seed(456)
  n <- 200
  times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"), by = "5 min", length.out = n)
  values <- c(rnorm(80, 100, 4), rnorm(60, 130, 4), rnorm(60, 110, 4))
  data <- dplyr::tibble(id = "id01", time = times, value = values)
  ts <- as_wearable_ts(data, id = id, time = time, value = value)

  res <- detect_changepoints_pelt(ts, cost = "meanvar", penalty = "MBIC", min_seg_len = 12)
  expect_true(nrow(res) >= 1)

  cpt_fit <- changepoint::cpt.meanvar(values, method = "PELT", penalty = "MBIC", minseglen = 12)
  cpt_indices <- changepoint::cpts(cpt_fit)
  if (length(cpt_indices) > 0) {
    diffs <- abs(res$cp_index - cpt_indices)
    expect_true(all(diffs <= 1))
  }
})

test_that("pelt_postprocess summarises changepoints", {
  helper <- getFromNamespace("pelt_postprocess", "wearableAnomaly")
  values <- c(rep(0, 5), rep(10, 5))
  times <- seq.POSIXt(as.POSIXct("2025-01-01 00:00:00", tz = "UTC"), by = "5 min", length.out = length(values))
  cp_indices <- c(5)
  res <- helper(values, times, cp_indices, method = "pelt", penalty = "MBIC")
  expect_equal(res$cp_index, 5)
  expect_equal(res$cp_time, times[5])
  expect_true(abs(res$new_level - mean(values[6:10])) < 1e-8)
  expect_true(res$new_var >= 0)
})

test_that("Rcpp PELT matches R implementation", {
  skip_if_not(getFromNamespace(".pelt_cpp_available", "wearableAnomaly")())

  core_r <- getFromNamespace(".pelt_core_r", "wearableAnomaly")
  make_cost <- getFromNamespace(".make_cost_functions", "wearableAnomaly")
  penalty_value <- getFromNamespace(".penalty_value", "wearableAnomaly")
  postprocess <- getFromNamespace("pelt_postprocess", "wearableAnomaly")

  seeds <- c(2, 11, 47)
  for (sd in seeds) {
    base <- toy_cgm(n_id = 1, n = 180, seed = sd)
    base$value[61:120] <- base$value[61:120] + 25
    base$value[121:180] <- base$value[121:180] + 10
    ts <- as_wearable_ts(base, id = id, time = time, value = value)

    values <- ts$value
    times <- ts$time
    cost_obj <- make_cost(values, "meanvar")
    pen <- penalty_value("MBIC", "meanvar", length(values))
    min_seg_len <- 20L

    cp_r <- core_r(values, cost_obj$segment_cost, min_seg_len, pen)
    cp_cpp <- pelt_core_meanvar(values, min_seg_len, pen)

    expect_equal(length(cp_cpp), length(cp_r))
    if (length(cp_r) == 0) {
      next
    }

    expect_true(all(abs(cp_r - cp_cpp) <= 1L))

    cp_time_r <- times[cp_r]
    cp_time_cpp <- times[cp_cpp]
    time_diff <- abs(as.numeric(difftime(cp_time_r, cp_time_cpp, units = "mins")))
    expect_true(all(time_diff <= 5))

    info_r <- postprocess(values, times, cp_r, method = "pelt", penalty = "MBIC")
    info_cpp <- postprocess(values, times, cp_cpp, method = "pelt", penalty = "MBIC")
    expect_equal(info_cpp$new_level, info_r$new_level, tolerance = 1e-6)
    expect_equal(info_cpp$new_var, info_r$new_var, tolerance = 1e-6)
  }
})
