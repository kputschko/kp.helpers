context("Describe Table")

test_that("check for invalid arguments", {
  expect_error(fx_describe(mtcars$mpg))
  expect_error(fx_describe("mtcars"))
  expect_error(fx_describe(list(mtcars)))
  expect_error(fx_describe(mtcars, output_format = "apples"))
  expect_error(fx_describe(mtcars, percentile_include = "TREE"))
})

test_that("check for success", {
  expect_that(fx_describe(mtcars), is.data.frame)
  expect_equal(fx_describe(mtcars) %>% nrow(), 11)
  expect_equal(fx_describe(mtcars) %>% ncol(), 9)
})
