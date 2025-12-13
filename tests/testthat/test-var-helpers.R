testthat::test_that("Duplicated factr levels are made unique", {
  var_string <- sample(c(letters[1:5]), 10, rep = T)
  var_fct <- fct_keep_dup_string(var_string) %>% as.factor()
  testthat::expect_equal(length(levels(var_fct)), length(var_string))
  testthat::expect_equal(length(var_fct), length(var_string))
})


test_that("get_var_nm orders variables according to vars argument when reorder=TRUE", {
  # Test with income variables in custom order
  custom_order <- c("yf", "ym", "yd")
  result <- get_var_nm(vars = custom_order, reorder = TRUE)
  
  expect_equal(result$var, custom_order)
  expect_s3_class(result$var_title, "factor")
  expect_equal(as.character(result$var_title), 
               c("Final income", "Market income", "Disposable income"))
})

test_that("get_var_nm preserves default order when reorder=FALSE", {
  custom_order <- c("yf", "ym", "yd")
  result <- get_var_nm(vars = custom_order, reorder = FALSE)
  
  # Should not be in custom order, but in dictionary order
  expect_false(identical(result$var, custom_order))
})

test_that("get_var_nm returns all vars when vars=NULL", {
  result <- get_var_nm(vars = NULL)
  
  expect_gt(nrow(result), 0)
  expect_true(all(c("var", "var_title") %in% colnames(result)))
})

test_that("get_measure_nm orders measures according to x argument", {
  custom_order <- c("fgt1", "hc", "gini")
  result <- get_measure_nm(x = custom_order)
  
  expect_equal(result$measure, custom_order)
  expect_equal(nrow(result), 3)
})

test_that("get_measure_nm returns all measures when x=NULL", {
  result <- get_measure_nm(x = NULL)
  
  expect_gt(nrow(result), 0)
  expect_true(all(c("measure", "measure_title") %in% colnames(result)))
})

test_that("get_measure_nm filters correctly with partial match", {
  measures <- c("fgt0", "fgt1", "gini")
  result <- get_measure_nm(x = measures)
  
  expect_equal(nrow(result), 3)
  expect_equal(result$measure, measures)
})

test_that("get_inc_nm respects reorder parameter", {
  # With reorder=TRUE and specific vars
  custom_order <- c("yc", "ym", "yp")
  result_ordered <- get_inc_nm(reorder = TRUE) |> 
    dplyr::filter(var %in% custom_order) |>
    dplyr::mutate(var = forcats::fct_relevel(var, custom_order)) |>
    dplyr::arrange(var)
  
  expect_equal(result_ordered$var |> as.character(), custom_order)
})

test_that("get_group_nm respects reorder parameter", {
  result <- get_group_nm(reorder = TRUE)
  
  expect_true(all(c("var", "var_title") %in% colnames(result)))
  expect_s3_class(result$var_title, "factor")
})

test_that("f_var_names_vector creates properly named vector", {
  test_data <- tibble::tibble(
    var = c("a", "b", "c"),
    var_title = c("Label A", "Label B", "Label C")
  )
  
  result <- f_var_names_vector(test_data)
  
  expect_named(result, c("Label A", "Label B", "Label C"))
  expect_equal(as.vector(result), c("a", "b", "c"))
})

test_that("f_var_names_vector works with measure columns", {
  test_data <- tibble::tibble(
    measure = c("m1", "m2", "m3"),
    measure_title = c("Measure 1", "Measure 2", "Measure 3")
  )
  
  result <- f_var_names_vector(test_data)
  
  expect_named(result, c("Measure 1", "Measure 2", "Measure 3"))
  expect_equal(as.vector(result), c("m1", "m2", "m3"))
})

test_that("f_var_names_vector errors with incorrect columns", {
  test_data <- tibble::tibble(
    wrong_col1 = c("a", "b"),
    wrong_col2 = c("x", "y")
  )
  
  expect_error(f_var_names_vector(test_data), "var.*var_title.*measure.*measure_title")
})

test_that("get_var_nm handles non-existent vars gracefully", {
  non_existent <- c("fake_var1", "fake_var2")
  result <- get_var_nm(vars = non_existent, reorder = TRUE)
  
  expect_equal(nrow(result), 0)
})

test_that("get_measure_nm handles non-existent measures gracefully", {
  non_existent <- c("fake_measure1", "fake_measure2")
  result <- get_measure_nm(x = non_existent)
  
  expect_equal(nrow(result), 0)
})

test_that("get_var_nm preserves order with mixed existing and non-existing vars", {
  mixed_order <- c("yf", "fake_var", "ym", "yd")
  existing_vars <- c("yf", "ym", "yd")
  result <- get_var_nm(vars = mixed_order, reorder = TRUE)
  
  # Should only contain existing vars in the specified order
  expect_equal(result$var, existing_vars)
})

test_that("fct_keep_dup_string handles duplicates correctly", {
  test_vals <- c("A", "B", "A", "C", "B", "A")
  result <- fct_keep_dup_string(test_vals)
  
  # All values should be unique after processing
  expect_equal(length(unique(result)), length(result))
  
  # Original values should be preserved (with zero-width spaces)
  expect_true(all(grepl("^[ABC]", result)))
})

test_that("get_var_nm with suffix adds suffix to var names", {
  vars <- c("ym", "yd")
  suffix <- "_adj"
  result <- get_var_nm(vars = vars, suffix = suffix, reorder = TRUE)
  
  expect_equal(result$var, paste0(vars, suffix))
})

test_that("ordering is stable across multiple calls", {
  custom_order <- c("fgt2", "fgt0", "hc", "gini", "fgt1")
  
  result1 <- get_measure_nm(x = custom_order)
  result2 <- get_measure_nm(x = custom_order)
  
  expect_identical(result1, result2)
  expect_equal(result1$measure, custom_order)
})

test_that("get_var_nm uses custom dictionary when provided", {
  # Create a custom dictionary function
  custom_dic <- function() {
    tibble::tribble(
      ~factor, ~var,    ~var_title,
      1,       "x1",    "Custom Variable 1",
      1,       "x2",    "Custom Variable 2",
      1,       "x3",    "Custom Variable 3"
    )
  }
  
  result <- get_var_nm(
    vars = c("x2", "x1", "x3"),
    reorder = TRUE,
    dic_default = custom_dic
  )
  
  expect_equal(result$var, c("x2", "x1", "x3"))
  expect_equal(as.character(result$var_title), 
               c("Custom Variable 2", "Custom Variable 1", "Custom Variable 3"))
})

test_that("get_var_nm respects custom dictionary name parameter", {
  # Define a custom dictionary function in global environment
  f_custom_var_dic <- function() {
    tibble::tribble(
      ~factor, ~var,  ~var_title,
      1,       "a1",  "Alpha 1",
      1,       "a2",  "Alpha 2"
    )
  }
  
  # Temporarily add to global environment
  assign("f_custom_var_dic", f_custom_var_dic, envir = .GlobalEnv)
  
  # on.exit(rm("f_custom_var_dic", envir = .GlobalEnv), add = TRUE)
  
  result <- get_var_nm(
    vars = c("a2", "a1"),
    reorder = TRUE,
    dic_default = function() tibble::tibble(var = character(), var_title = character(), factor = numeric()),
    dic_custom_name = "f_custom_var_dic"
  )
  
  expect_equal(result$var, c("a2", "a1"))
  expect_equal(as.character(result$var_title), c("Alpha 2", "Alpha 1"))
  rm("f_custom_var_dic", envir = .GlobalEnv)
})

test_that("get_measure_nm uses custom measure dictionary", {
  # Create a custom measure dictionary
  custom_measure_dic <- function() {
    tibble::tribble(
      ~measure, ~measure_title,
      "m1",     "Custom Measure 1",
      "m2",     "Custom Measure 2",
      "m3",     "Custom Measure 3"
    )
  }
  
  result <- get_var_nm(
    vars = c("m3", "m1"),
    reorder = TRUE,
    dic_default = custom_measure_dic,
    dic_custom_name = "face_dic"
  )
  
  # Should rename columns back to measure/measure_title
  expect_true("measure" %in% colnames(result))
  expect_equal(result$measure, c("m3", "m1"))
})

test_that("custom dictionary with different structure is handled", {
  # Dictionary without factor column
  custom_dic_no_factor <- function() {
    tibble::tribble(
      ~var,    ~var_title,
      "v1",    "Variable One",
      "v2",    "Variable Two"
    )
  }
  
  result <- get_var_nm(
    vars = c("v2", "v1"),
    reorder = TRUE,
    dic_default = custom_dic_no_factor
  )
  
  expect_equal(result$var, c("v2", "v1"))
  expect_equal(as.character(result$var_title), c("Variable Two", "Variable One"))
})

test_that("custom dictionary overrides default when f_var_dic exists", {
  # Create custom function in global environment
  f_var_dic <- function() {
    tibble::tribble(
      ~factor, ~var,     ~var_title,
      1,       "custom", "Custom Var"
    )
  }
  
  assign("f_var_dic", f_var_dic, envir = .GlobalEnv)
  on.exit(rm("f_var_dic", envir = .GlobalEnv), add = TRUE)
  
  result <- get_var_nm(vars = "custom", reorder = TRUE)
  
  expect_equal(result$var, "custom")
  expect_equal(as.character(result$var_title), "Custom Var")
})

test_that("custom dictionary with measure columns gets renamed correctly", {
  custom_dic_measure <- function() {
    tibble::tribble(
      ~measure,     ~measure_title,
      "custom_m1",  "Custom Measure One",
      "custom_m2",  "Custom Measure Two"
    )
  }
  
  result <- get_var_nm(
    vars = c("custom_m2", "custom_m1"),
    reorder = TRUE,
    dic_default = custom_dic_measure
  )
  
  # Should detect measure columns and rename
  expect_true("measure" %in% colnames(result))
  expect_true("measure_title" %in% colnames(result))
  expect_equal(result$measure, c("custom_m2", "custom_m1"))
})

test_that("custom dictionary preserves var_title factor levels in order", {
  custom_dic <- function() {
    tibble::tribble(
      ~factor, ~var,  ~var_title,
      1,       "z1",  "Zebra",
      1,       "a1",  "Apple",
      1,       "m1",  "Mango"
    )
  }
  
  custom_order <- c("a1", "z1", "m1")
  result <- get_var_nm(
    vars = custom_order,
    reorder = TRUE,
    dic_default = custom_dic,
    dic_custom_name = "face_dic"
  )
  
  expect_s3_class(result$var_title, "factor")
  expect_equal(result$var, custom_order)
  expect_equal(levels(result$var_title), c("Apple", "Zebra", "Mango"))
})
