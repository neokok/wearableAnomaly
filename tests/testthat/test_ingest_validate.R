test_that("as_wearable_ts returns expected structure and metadata", {
  data <- toy_cgm(n_id = 2, n = 6, seed = 11)
  wa <- as_wearable_ts(data, id = id, time = time, value = value)

  expect_s3_class(wa, c("wa_ts", "tbl_df", "tbl", "data.frame"))
  expect_identical(attr(wa, "lower"), 40)
  expect_identical(attr(wa, "upper"), 400)
  expect_true(is.na(attr(wa, "cadence")))
  expect_identical(attr(wa, "tz"), "UTC")
  expect_identical(attr(wa, "dups_dropped"), 0L)
  expect_equal(nrow(wa), 12)
})

test_that("non-POSIXct time vectors are coerced with requested tz", {
  data <- toy_cgm(n_id = 1, n = 4)
  data$time <- format(data$time, "%Y-%m-%d %H:%M:%S")

  wa <- as_wearable_ts(data, id = id, time = time, value = value, tz = "UTC")
  expect_s3_class(wa$time, "POSIXct")
  expect_identical(attr(wa, "tz"), "UTC")
  tz_attr <- attr(wa$time, "tzone")
  expect_true(is.character(tz_attr))
  expect_true("UTC" %in% tz_attr)
})

test_that("unsorted observations are ordered within id with a warning", {
  data <- toy_cgm(n_id = 1, n = 6)
  scrambled <- data[c(3, 1, 2, 4:6), ]

  expect_warning(
    wa <- as_wearable_ts(scrambled, id = id, time = time, value = value),
    "Reordered input"
  )
  expect_true(all(diff(wa$time) >= 0))
})

test_that("duplicate (id, time) pairs are dropped with accounting", {
  data <- toy_cgm(n_id = 1, n = 4)
  dup_data <- rbind(data, data[2, , drop = FALSE])
  dup_data <- dplyr::arrange(dup_data, id, time)

  expect_warning(
    wa <- as_wearable_ts(dup_data, id = id, time = time, value = value),
    "duplicated observation"
  )
  expect_identical(attr(wa, "dups_dropped"), 1L)
  expect_equal(nrow(wa), 4)
})

test_that("missing columns and all-NA values raise informative errors", {
  data <- toy_cgm(n_id = 1, n = 4)
  data_missing <- dplyr::select(data, -value)

  expect_error(
    as_wearable_ts(data_missing, id = id, time = time, value = value),
    "`value` must refer to an existing column"
  )

  data_na <- data
  data_na$value <- NA_real_
  expect_error(
    as_wearable_ts(data_na, id = id, time = time, value = value),
    "`value` column contains only missing values."
  )
})

test_that("validate_ts flags duplicates, ordering issues, and value problems", {
  base_data <- toy_cgm(n_id = 1, n = 6)
  base_data$value[2] <- Inf
  base_data$value[4] <- 10
  base_data$value[5] <- NA_real_
  issue_data <- rbind(base_data, base_data[3, , drop = FALSE])
  issue_data$time[nrow(issue_data)] <- issue_data$time[nrow(issue_data) - 1]

  wa_issue <- dplyr::as_tibble(issue_data)
  class(wa_issue) <- c("wa_ts", class(wa_issue))
  attr(wa_issue, "lower") <- 40
  attr(wa_issue, "upper") <- 400
  attr(wa_issue, "tz") <- "UTC"

  issues <- validate_ts(wa_issue)
  expect_true(has_issues(issues))
  expect_setequal(
    issues$issue,
    c("duplicate", "non_finite_value", "out_of_range", "non_increasing_time")
  )
})

test_that("validate_ts returns empty tibble when data are clean", {
  data <- toy_cgm(n_id = 1, n = 5)
  wa <- as_wearable_ts(data, id = id, time = time, value = value)

  issues <- validate_ts(wa)
  expect_s3_class(issues, "tbl_df")
  expect_equal(nrow(issues), 0)
  expect_false(has_issues(issues))
})

test_that("validate_ts reports missing columns", {
  wa <- as_wearable_ts(toy_cgm(n_id = 1, n = 4), id = id, time = time, value = value)
  wa$value <- NULL

  issues <- validate_ts(wa)
  expect_true(has_issues(issues))
  expect_equal(unique(issues$issue), "missing_column")
})
