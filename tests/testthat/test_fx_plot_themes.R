context("Testing ggplot2 themes")

test_that("make sure the theme is a ggplot theme", {
  expect_true(is.list(fx_plot_themes()))
  expect_equal(length(fx_plot_themes()), 2)
})
