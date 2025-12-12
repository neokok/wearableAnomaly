test_that("evaluate_methods returns tidy metrics", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  truth <- dplyr::tibble(
    id = c("id01", "id01"),
    start = base_time + c(0, 60) * 60,
    end = base_time + c(15, 80) * 60
  )
  pred <- truth

  metrics <- evaluate_methods(pred, truth, tolerance = "5 min", method = "wearable", runtime_sec = 0.25)
  expect_s3_class(metrics, "tbl_df")
  expect_true(all(c("method", "runtime_sec", "mean_n_cps", "precision", "recall", "f1", "mae_cp", "iou") %in% names(metrics)))
  expect_equal(nrow(metrics), 1L)
  expect_equal(metrics$method, "wearable")
  expect_equal(metrics$runtime_sec, 0.25)
  expect_equal(metrics$mean_n_cps, 2)
  expect_equal(metrics$precision, 1)
  expect_equal(metrics$recall, 1)
  expect_equal(metrics$f1, 1)
  expect_equal(metrics$mae_cp, 0)
  expect_equal(metrics$iou, 1)
})

test_that("evaluate_methods handles empty inputs", {
  truth <- dplyr::tibble(id = character(), start = as.POSIXct(character()), end = as.POSIXct(character()))
  pred <- truth
  metrics <- evaluate_methods(pred, truth)
  expect_equal(metrics$precision, 1)
  expect_equal(metrics$recall, 1)
  expect_equal(metrics$f1, 1)
  expect_equal(metrics$iou, 1)
  expect_equal(metrics$mean_n_cps, 0)
  expect_true(is.na(metrics$mae_cp))
})

test_that("evaluate_methods accepts list inputs with per-method metadata", {
  base_time <- as.POSIXct("2025-02-01 00:00:00", tz = "UTC")
  truth <- dplyr::tibble(
    id = "id01",
    start = base_time + 30 * 60,
    end = base_time + 35 * 60
  )
  perfect <- truth
  imperfect <- dplyr::tibble(
    id = "id01",
    start = base_time + 120 * 60,
    end = base_time + 125 * 60
  )
  preds <- list(
    wa = list(segments = perfect, runtime_sec = 0.1),
    ref = imperfect
  )

  metrics <- evaluate_methods(preds, truth, tolerance = "2 min")
  expect_equal(nrow(metrics), 2L)
  expect_equal(metrics$method, c("wa", "ref"))
  expect_equal(metrics$runtime_sec[metrics$method == "wa"], 0.1)
  expect_equal(metrics$precision[metrics$method == "wa"], 1)
  expect_equal(metrics$recall[metrics$method == "wa"], 1)
  expect_equal(metrics$precision[metrics$method == "ref"], 0)
  expect_equal(metrics$recall[metrics$method == "ref"], 0)
})
