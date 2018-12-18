context("Testing ggplot2 themes")

test_that("make sure the theme is a ggplot theme", {
  expect_true("theme" %in% class(theme_kp("light")))
  expect_true("theme" %in% class(theme_kp("dark")))
  expect_error(theme_kp("banana"))
})
