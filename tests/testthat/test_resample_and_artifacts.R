duration_secs <- function(by) {
  anchor <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  grid <- seq(from = anchor, by = by, length.out = 2)
  as.numeric(difftime(grid[2], grid[1], units = "secs"))
}

test_that("resample_series regularises irregular cadence", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  data <- dplyr::tibble(
    id = rep(c("id01", "id02"), each = 4),
    time = base_time + c(0, 6 * 60, 11 * 60, 17 * 60, 2 * 60, 7 * 60, 12 * 60, 19 * 60),
    value = c(100, 101, 102, 103, 200, 202, 204, 206)
  )

  wa <- as_wearable_ts(data, id = id, time = time, value = value, tz = "UTC")
  res <- resample_series(wa, by = "5 min", agg = "mean")

  expect_s3_class(res, c("wa_ts", "tbl_df", "tbl", "data.frame"))
  expect_true(all(diff(res$time) >= 0))
  expect_equal(attr(res, "cadence"), duration_secs("5 min"))
  expect_identical(attr(res, "tz"), "UTC")
  expect_identical(attr(res, "lower"), attr(wa, "lower"))
  expect_identical(attr(res, "upper"), attr(wa, "upper"))

  id01 <- dplyr::filter(res, id == "id01")
  expect_true(all(diff(as.numeric(id01$time)) == 5 * 60))
})

test_that("resample_series aggregation strategies differ", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  data <- dplyr::tibble(
    id = "id01",
    time = base_time + c(0, 2 * 60, 4 * 60, 10 * 60, 11 * 60, 12 * 60),
    value = c(10, 20, 30, 40, 50, 60)
  )
  wa <- as_wearable_ts(data, id = id, time = time, value = value)

  res_mean <- resample_series(wa, by = "10 min", agg = "mean")
  res_last <- resample_series(wa, by = "10 min", agg = "last")
  res_median <- resample_series(wa, by = "10 min", agg = "median")

  expect_equal(res_mean$value[1:2], c(20, 50))
  expect_equal(res_last$value[1:2], c(30, 60))
  expect_equal(res_median$value[1:2], c(20, 50))
  expect_true(any(is.na(res_mean$value)))
})

test_that("resample_series validates inputs and retains NAs", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  data <- dplyr::tibble(
    id = "id01",
    time = base_time + c(0, 5 * 60, 10 * 60),
    value = c(NA, 5, NA)
  )
  wa <- as_wearable_ts(data, id = id, time = time, value = value)

  expect_error(resample_series(data, by = "5 min"), "expects a `wa_ts`")
  expect_error(resample_series(wa, by = "not an interval"), "positive time specification")

  res <- resample_series(wa, by = "5 min", agg = "mean")
  expect_true(any(is.na(res$value)))
})

test_that("detect_flatlines identifies long constant runs", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  data <- dplyr::tibble(
    id = "id01",
    time = base_time + seq(0, by = 5 * 60, length.out = 10),
    value = c(100, 101, rep(125, 6), 140, 142)
  )
  wa <- as_wearable_ts(data, id = id, time = time, value = value)

  segs <- detect_flatlines(wa, tol = 1e-6, min_len = "15 min")
  expect_equal(nrow(segs), 1)
  expect_equal(segs$type, "flatline")
  expect_equal(segs$start, data$time[3])
  expect_equal(segs$end, data$time[8])
  expect_equal(as.numeric(segs$duration, units = "mins"), 25)

  no_seg <- detect_flatlines(wa, tol = 1e-6, min_len = "40 min")
  expect_equal(nrow(no_seg), 0)

  jitter <- wa
  jitter$value[3:8] <- jitter$value[3:8] + c(0, 1e-7, -1e-7, 5e-7, -3e-7, 2e-7)
  tol_seg <- detect_flatlines(jitter, tol = 1e-5, min_len = "15 min")
  expect_equal(nrow(tol_seg), 1)
})

test_that("detect_flatlines returns empty tibble when no data", {
  empty <- structure(
    dplyr::tibble(id = character(),
                  time = as.POSIXct(numeric(0), origin = "1970-01-01", tz = "UTC"),
                  value = numeric()),
    class = c("wa_ts", "tbl_df", "tbl", "data.frame"),
    lower = 40,
    upper = 400,
    cadence = NA_real_,
    tz = "UTC",
    dups_dropped = 0L
  )
  segs <- detect_flatlines(empty)
  expect_s3_class(segs, "tbl_df")
  expect_equal(nrow(segs), 0)
})

test_that("detect_saturation flags runs at upper and lower bounds", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  data <- dplyr::tibble(
    id = "id01",
    time = base_time + seq(0, by = 5 * 60, length.out = 12),
    value = c(100, 102, rep(400, 4), 120, 118, rep(40, 4))
  )
  wa <- as_wearable_ts(data, id = id, time = time, value = value)

  segs <- detect_saturation(wa, min_len = "10 min")
  expect_setequal(segs$type, c("saturation_high", "saturation_low"))
  high <- dplyr::filter(segs, type == "saturation_high")
  expect_equal(high$start, data$time[3])
  expect_equal(high$end, data$time[6])
  expect_equal(as.numeric(high$duration, units = "mins"), 15)

  low <- dplyr::filter(segs, type == "saturation_low")
  expect_equal(low$start, data$time[9])
  expect_equal(low$end, data$time[12])
  expect_equal(as.numeric(low$duration, units = "mins"), 15)

  short <- detect_saturation(wa, min_len = "30 min")
  expect_equal(nrow(short), 0)
})

test_that("detect_rate_change flags spikes and dips", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  values <- c(100, 101, 102, 135, 136, 137, 138, 139)
  data <- dplyr::tibble(
    id = "id01",
    time = base_time + seq(0, by = 5 * 60, length.out = length(values)),
    value = values
  )
  wa <- as_wearable_ts(data, id = id, time = time, value = value)

  roc <- detect_rate_change(wa, window = "15 min", threshold = 3, scale = "mad")
  expect_equal(nrow(roc), 1)
  expect_equal(roc$type, "roc_up")
  expect_true(roc$strength > 3)
  expect_equal(roc$start, data$time[3])
  expect_equal(roc$end, data$time[4])

  down_values <- c(142, 141, 140, 110, 109, 108, 107, 106)
  down_data <- dplyr::tibble(
    id = "id01",
    time = base_time + seq(0, by = 5 * 60, length.out = length(down_values)),
    value = down_values
  )
  down <- as_wearable_ts(down_data, id = id, time = time, value = value)
  roc_down <- detect_rate_change(down, window = "15 min", threshold = 3, scale = "mad")
  expect_equal(nrow(roc_down), 1)
  expect_equal(roc_down$type, "roc_down")
  expect_true(roc_down$strength > 3)

  sd_scaled <- detect_rate_change(wa, window = "15 min", threshold = 2, scale = "sd")
  expect_equal(nrow(sd_scaled), 1)
  expect_equal(sd_scaled$type, "roc_up")

  no_issue <- detect_rate_change(wa, window = "15 min", threshold = 10, scale = "mad")
  expect_equal(nrow(no_issue), 0)
})

test_that("detect_rate_change respects window tolerance", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  times <- base_time + c(0, 15 * 60, 31 * 60, 46 * 60) # slightly irregular gaps
  values <- c(100, 101, 150, 151)
  data <- dplyr::tibble(id = "id01", time = times, value = values)
  wa <- as_wearable_ts(data, id = id, time = time, value = value)

  roc <- detect_rate_change(wa, window = "15 min", threshold = 2, scale = "none")
  expect_equal(nrow(roc), 1)
  expect_equal(roc$type, "roc_up")
})
