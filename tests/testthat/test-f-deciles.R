testthat::skip()
test_that("f_calc_deciles creates decile columns correctly", {
  # Create test data
  test_data <- data.frame(
    id = 1:100,
    income = runif(100, 1000, 10000),
    weight = runif(100, 0.5, 2)
  )
  
  result <- f_calc_deciles(
    dta = test_data,
    dec_var = "income",
    wt_var = "weight",
    n_dec = 10
  )
  
  # Check that decile column was created
  expect_true("income_decile_10" %in% names(result))
  
  # Check that it's a factor
  expect_s3_class(result$income_decile_10, "factor")
  
  # Check that it has 10 levels
  expect_equal(nlevels(result$income_decile_10), 10)
  
  # Check that original columns are preserved
  expect_true(all(c("id", "income", "weight") %in% names(result)))
})

test_that("f_calc_deciles works with multiple variables", {
  test_data <- data.frame(
    income1 = runif(100, 1000, 10000),
    income2 = runif(100, 2000, 8000),
    weight = rep(1, 100)
  )
  
  result <- f_calc_deciles(
    dta = test_data,
    dec_var = c("income1", "income2"),
    wt_var = "weight",
    n_dec = 5
  )
  
  # Check both decile columns created
  expect_true("income1_decile_5" %in% names(result))
  expect_true("income2_decile_5" %in% names(result))
  
  # Check they're factors with 5 levels
  expect_equal(nlevels(result$income1_decile_5), 5)
  expect_equal(nlevels(result$income2_decile_5), 5)
})

test_that("f_calc_deciles uses equal weights when wt_var is NULL", {
  test_data <- data.frame(
    income = runif(100, 1000, 10000)
  )
  
  result <- f_calc_deciles(
    dta = test_data,
    dec_var = "income",
    wt_var = NULL,
    n_dec = 10
  )
  
  # Should still create decile column
  expect_true("income_decile_10" %in% names(result))
  
  # Should not have wt_temp__ column
  expect_false("wt_temp__" %in% names(result))
})

test_that("f_calc_deciles uses equal weights when wt_var not in data", {
  test_data <- data.frame(
    income = runif(100, 1000, 10000)
  )
  
  result <- f_calc_deciles(
    dta = test_data,
    dec_var = "income",
    wt_var = "nonexistent_weight",
    n_dec = 10
  )
  
  # Should still create decile column
  expect_true("income_decile_10" %in% names(result))
})

test_that("f_calc_deciles skips existing decile columns", {
  test_data <- data.frame(
    income = runif(100, 1000, 10000),
    income_decile_10 = factor(sample(1:10, 100, replace = TRUE))
  )
  
  original_decile <- test_data$income_decile_10
  
  expect_message(
    result <- f_calc_deciles(
      dta = test_data,
      dec_var = "income",
      wt_var = NULL,
      n_dec = 10
    ),
    "All requested decile variables already exist"
  )
  
  # Should not recalculate existing decile
  expect_identical(result$income_decile_10, original_decile)
})

test_that("f_calc_deciles handles missing variables gracefully", {
  test_data <- data.frame(
    income = runif(100, 1000, 10000)
  )
  
  expect_warning(
    result <- f_calc_deciles(
      dta = test_data,
      dec_var = c("income", "nonexistent"),
      wt_var = NULL,
      n_dec = 10
    ),
    "Skipping missing variables"
  )
  
  # Should create decile for existing variable
  expect_true("income_decile_10" %in% names(result))
  
  # Should not create decile for nonexistent variable
  expect_false("nonexistent_decile_10" %in% names(result))
})

test_that("f_calc_deciles aborts when all variables missing", {
  test_data <- data.frame(
    income = runif(100, 1000, 10000)
  )
  
  expect_error(
    f_calc_deciles(
      dta = test_data,
      dec_var = c("var1", "var2"),
      wt_var = NULL,
      n_dec = 10
    ),
    "None of.*variables found in data"
  )
})

test_that("f_calc_deciles handles NULL dec_var", {
  test_data <- data.frame(
    income = runif(100, 1000, 10000)
  )
  
  expect_warning(
    result <- f_calc_deciles(
      dta = test_data,
      dec_var = NULL,
      wt_var = NULL,
      n_dec = 10
    ),
    "must be a non-empty character vector"
  )
  
  # Should return original data unchanged
  expect_identical(result, test_data)
})

test_that("f_calc_deciles handles empty dec_var", {
  test_data <- data.frame(
    income = runif(100, 1000, 10000)
  )
  
  expect_warning(
    result <- f_calc_deciles(
      dta = test_data,
      dec_var = character(0),
      wt_var = NULL,
      n_dec = 10
    ),
    "must be a non-empty character vector"
  )
  
  # Should return original data unchanged
  expect_identical(result, test_data)
})

test_that("f_calc_deciles works with different n_dec values", {
  test_data <- data.frame(
    income = runif(100, 1000, 10000)
  )
  
  # Test quartiles (4 groups)
  result_4 <- f_calc_deciles(
    dta = test_data,
    dec_var = "income",
    wt_var = NULL,
    n_dec = 4
  )
  
  expect_true("income_decile_4" %in% names(result_4))
  expect_equal(nlevels(result_4$income_decile_4), 4)
  
  # Test quintiles (5 groups)
  result_5 <- f_calc_deciles(
    dta = test_data,
    dec_var = "income",
    wt_var = NULL,
    n_dec = 5
  )
  
  expect_true("income_decile_5" %in% names(result_5))
  expect_equal(nlevels(result_5$income_decile_5), 5)
})

test_that("f_calc_deciles preserves data frame class", {
  # Test with tibble
  test_tibble <- tibble::tibble(
    income = runif(100, 1000, 10000)
  )
  
  result <- f_calc_deciles(
    dta = test_tibble,
    dec_var = "income",
    wt_var = NULL,
    n_dec = 10
  )
  
  expect_s3_class(result, "tbl_df")
})

test_that("f_calc_deciles handles partial existing deciles", {
  test_data <- data.frame(
    income1 = runif(100, 1000, 10000),
    income2 = runif(100, 2000, 8000),
    income1_decile_10 = factor(sample(1:10, 100, replace = TRUE))
  )
  
  original_decile <- test_data$income1_decile_10
  
  result <- f_calc_deciles(
    dta = test_data,
    dec_var = c("income1", "income2"),
    wt_var = NULL,
    n_dec = 10
  )
  
  # Should preserve existing decile for income1
  expect_identical(result$income1_decile_10, original_decile)
  
  # Should create new decile for income2
  expect_true("income2_decile_10" %in% names(result))
  expect_s3_class(result$income2_decile_10, "factor")
})
