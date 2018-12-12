context("Describe Table")

test_that("check for invalid arguments", {
  expect_error(fx_describe_table(mtcars$mpg))
  expect_error(fx_describe_table("mtcars"))
  expect_error(fx_describe_table(list(mtcars)))
})

# test_that("check for success", {
#   expect_that(fx_describe_table(mtcars), is.data.frame)
#   expect_equal(fx_describe_table(mtcars) %>% nrow(), 11)
#   expect_equal(fx_describe_table(mtcars) %>% ncol(), 9)
# })
