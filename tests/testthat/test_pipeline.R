test_that("detect_anomalies returns structured output", {
  data <- toy_cgm(n_id = 1, n = 180)
  data$value[61:120] <- data$value[61:120] + 25
  data$value[121:180] <- data$value[121:180] - 15
  ts <- as_wearable_ts(data, id = id, time = time, value = value)

  res <- detect_anomalies(ts, preset = "research", resample_by = NULL, gap = "5 min")
  expect_type(res, "list")
  expect_true(all(c("segments", "changepoints", "artifacts", "issues") %in% names(res)))
  expect_s3_class(res$segments, "tbl_df")
  expect_true(all(c("pelt", "edivisive") %in% names(res$changepoints)))
  expect_true(is.list(res$artifacts))
})

test_that("detect_anomalies supports preset switching", {
  data <- toy_cgm(n_id = 1, n = 120)
  ts <- as_wearable_ts(data, id = id, time = time, value = value)
  res <- detect_anomalies(ts, preset = "clinical", resample_by = "10 min", combine = "max")
  expect_s3_class(res$segments, "tbl_df")
  expect_true(all(res$segments$score >= 0))
})
