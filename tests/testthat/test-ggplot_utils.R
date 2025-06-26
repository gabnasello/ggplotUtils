# Test file content
library(testthat)
library(ggplotUtils)
library(ggplot2)

test_that("create_barplot works correctly", {
  # Create test data
  df <- data.frame(
    x = rep(c("A", "B", "C"), each = 5),
    y = c(rnorm(5, 5), rnorm(5, 7), rnorm(5, 6))
  )
  
  # Test basic functionality
  p <- create_barplot(df)
  expect_s3_class(p, "ggplot")
  expect_true(!is.null(attr(p, "summary_data")))
})

test_that("add_errorbars works correctly", {
  df <- data.frame(
    x = rep(c("A", "B"), each = 5),
    y = c(rnorm(5, 5), rnorm(5, 7))
  )
  
  p <- create_barplot(df)
  p_with_errors <- add_errorbars(p)
  expect_s3_class(p_with_errors, "ggplot")
})

test_that("apply_minimal_theme works correctly", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  result <- apply_minimal_theme(p, title = "Test Plot")
  expect_s3_class(result, "ggplot")
})

test_that("compute_summary_stats handles different error types", {
  df <- data.frame(
    x = rep(c("A", "B"), each = 10),
    y = c(rnorm(10, 5), rnorm(10, 7))
  )
  
  # Test different error types
  result_sd <- compute_summary_stats(df, "y", "x", error_type = "sd")
  result_se <- compute_summary_stats(df, "y", "x", error_type = "se")
  result_ci <- compute_summary_stats(df, "y", "x", error_type = "ci")
  
  expect_true(all(c("mean", "lower", "upper") %in% names(result_sd)))
  expect_true(all(c("mean", "lower", "upper") %in% names(result_se)))
  expect_true(all(c("mean", "lower", "upper") %in% names(result_ci)))
})