test_that("evaluate_methods computes precision, recall, and IoU", {
  base_time <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC")
  truth <- dplyr::tibble(
    id = c("id01", "id01"),
    start = base_time + c(0, 60) * 60,
    end = base_time + c(15, 80) * 60
  )
  pred <- dplyr::tibble(
    id = c("id01", "id01"),
    start = base_time + c(2, 120) * 60,
    end = base_time + c(14, 130) * 60
  )

  metrics <- evaluate_methods(pred, truth, tolerance = "10 min")
  expect_named(metrics, c("precision", "recall", "f1", "iou"))
  expect_true(metrics$precision <= 1 && metrics$precision >= 0)
  expect_true(metrics$recall <= 1 && metrics$recall >= 0)
  expect_true(metrics$f1 <= 1 && metrics$f1 >= 0)
  expect_true(metrics$iou <= 1 && metrics$iou >= 0)
})

test_that("evaluate_methods handles empty inputs", {
  truth <- dplyr::tibble(id = character(), start = as.POSIXct(character()), end = as.POSIXct(character()))
  pred <- truth
  metrics <- evaluate_methods(pred, truth)
  expect_equal(metrics$precision, 1)
  expect_equal(metrics$recall, 1)
  expect_equal(metrics$f1, 1)
  expect_equal(metrics$iou, 1)
})
