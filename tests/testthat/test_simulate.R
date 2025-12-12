test_that("simulate_cgm_benchmark returns data and truth tibbles", {
  sim <- simulate_cgm_benchmark(n_series = 2, n_points = 96, n_cps = 2, seed = 123)
  expect_named(sim, c("data", "truth"))

  expect_s3_class(sim$data, "tbl_df")
  expect_true(nrow(sim$data) > 0)
  expect_true(all(c("id", "time", "value") %in% names(sim$data)))

  expect_s3_class(sim$truth, "tbl_df")
  expect_true(all(c("id", "start", "end") %in% names(sim$truth)))
  expect_true(nrow(sim$truth) >= 0)
})
