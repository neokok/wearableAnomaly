test_that("hello prints a greeting", {
  expect_invisible(hello())
  expect_output(hello(), "Hello, world!")
})
