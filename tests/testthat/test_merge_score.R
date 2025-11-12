test_that("merge_segments standardises and merges detections", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  flatlines <- dplyr::tibble(
    id = "id01",
    start = base_time + c(0, 60) * 60,
    end = base_time + c(10, 70) * 60,
    type = c("flatline", "flatline"),
    strength = c(3, 5),
    details = c("flat run", "second run")
  )
  pelt <- dplyr::tibble(
    id = c("id01", "id02"),
    cp_time = base_time + c(5, 120) * 60,
    new_level = c(120, 140),
    method = "pelt"
  )
  ediv <- dplyr::tibble(
    id = "id02",
    cp_time = base_time + 125 * 60,
    p_value = 0.05,
    method = "edivisive"
  )

  merged <- merge_segments(
    flatlines = flatlines,
    pelt = pelt,
    edivisive = ediv,
    gap = "10 min"
  )

  expect_true(all(c("id", "start", "end", "source", "subtype", "score", "notes", "components") %in% names(merged)))
  expect_true(all(lengths(merged$components) >= 1))
  expect_true(sum(merged$id == "id01") >= 1L)
  expect_true(any(grepl("flatlines", merged$source)))
  expect_true(any(grepl("pelt", merged$source)))
})

test_that("score_anomalies normalises and combines per source", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  segments <- dplyr::tibble(
    id = c("id01", "id01", "id01"),
    start = base_time + c(0, 30, 90) * 60,
    end = base_time + c(10, 40, 100) * 60,
    source = c("flatlines", "pelt", "flatlines"),
    subtype = c("flatline", "changepoint", "flatline"),
    score = c(2, 5, 1),
    notes = NA_character_,
    components = list(NULL, NULL, NULL)
  )

  scored_sum <- score_anomalies(segments, weights = c(flatlines = 2, pelt = 1), combine = "sum")
  expect_equal(nrow(scored_sum), 3L)
  expect_true(all(!is.na(scored_sum$score)))
  expect_true(all(scored_sum$score >= 0))

  scored_vote <- score_anomalies(segments, combine = "vote")
  expect_true(all(scored_vote$score <= 1))
})
