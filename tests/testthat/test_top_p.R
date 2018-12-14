context("Testing fx_top_p")

test_that("Unquoted column names", {
  expect_equal(fx_top_p(mtcars, 0.20, "mpg") %>% nrow(), 7)
  expect_error(fx_top_p(mtcars, 0.20, mpg))
  expect_error(fx_top_p(mtcars, 0.20, banana))
})

test_that("Proportion", {
  expect_error(fx_top_p(mcars, 100, mpg))
})
