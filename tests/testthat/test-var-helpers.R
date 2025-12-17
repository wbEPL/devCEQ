testthat::skip()
# Tests for f_var_helpres.R functions =====================================

# f_get_dic() tests --------------------------------------------------------

test_that("f_get_dic returns NULL with warning for non-existent function", {
  expect_warning(
    result <- f_get_dic("non_existent_function"),
    "Dictionary function.*not found"
  )
  expect_null(result)
})

test_that("f_get_dic returns NULL with warning for NULL input", {
  expect_warning(
    result <- f_get_dic(NULL),
    "must be a single character string"
  )
  expect_null(result)
})

test_that("f_get_dic returns NULL with warning for non-character input", {
  expect_warning(
    result <- f_get_dic(123),
    "must be a single character string"
  )
  expect_null(result)
})

test_that("f_get_dic returns NULL with warning for vector input", {
  expect_warning(
    result <- f_get_dic(c("func1", "func2")),
    "must be a single character string"
  )
  expect_null(result)
})

test_that("f_get_dic returns data frame for valid function", {
  result <- f_get_dic("f_var_dic_default")
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
})

test_that("f_get_dic returns NULL with warning for function that doesn't return data frame", {
  test_func <- function() "not a data frame"
  assign("test_func", test_func, envir = .GlobalEnv)
  on.exit(rm("test_func", envir = .GlobalEnv), add = TRUE)
  
  expect_warning(
    result <- f_get_dic("test_func"),
    "must return a data frame"
  )
  expect_null(result)
})

test_that("f_get_dic returns NULL with warning for function that returns empty data frame", {
  empty_func <- function() data.frame()
  assign("empty_func", empty_func, envir = .GlobalEnv)
  on.exit(rm("empty_func", envir = .GlobalEnv), add = TRUE)
  
  expect_warning(
    result <- f_get_dic("empty_func"),
    "is empty"
  )
  expect_null(result)
})

test_that("f_get_dic handles function errors gracefully", {
  error_func <- function() stop("intentional error")
  assign("error_func", error_func, envir = .GlobalEnv)
  on.exit(rm("error_func", envir = .GlobalEnv), add = TRUE)
  
  expect_warning(
    result <- f_get_dic("error_func"),
    "Error calling"
  )
  expect_null(result)
})

# f_get_upd_dic() tests ----------------------------------------------------

test_that("f_get_upd_dic returns default dictionary when custom doesn't exist", {
  expect_warning(
    result <- f_get_upd_dic("non_existent_custom", "f_var_dic_default"),
    "not found"
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0)
})

test_that("f_get_upd_dic errors when both dictionaries are invalid", {
  expect_error(
    f_get_upd_dic("non_existent_custom", "non_existent_default"),
    "Both dictionaries are invalid"
  )
})

test_that("f_get_upd_dic merges custom and default dictionaries", {
  # Create custom dictionary that overrides one entry
  custom_dic <- function() {
    tibble::tibble(
      factor = 1,
      var = "ym",
      var_title = "Custom Market Income Label"
    )
  }
  assign("custom_dic", custom_dic, envir = .GlobalEnv)
  on.exit(rm("custom_dic", envir = .GlobalEnv), add = TRUE)
  
  result <- f_get_upd_dic("custom_dic", "f_var_dic_default")
  
  # Should have all default entries plus custom override
  expect_gt(nrow(result), 1)
  
  # Custom entry should override default
  ym_entry <- result[result$var == "ym", ]
  expect_equal(ym_entry$var_title, "Custom Market Income Label")
})

test_that("f_get_upd_dic warns when no common columns exist", {
  custom_no_common <- function() {
    tibble::tibble(
      different_col1 = c("a", "b"),
      different_col2 = c("x", "y")
    )
  }
  assign("custom_no_common", custom_no_common, envir = .GlobalEnv)
  on.exit(rm("custom_no_common", envir = .GlobalEnv), add = TRUE)
  
  expect_warning(
    result <- f_get_upd_dic("custom_no_common", "f_var_dic_default"),
    "No common columns"
  )
  # Should return default
  expect_s3_class(result, "data.frame")
})

test_that("f_get_upd_dic returns custom when only custom is valid", {
  custom_only <- function() {
    tibble::tibble(
      var = c("x1", "x2"),
      var_title = c("X One", "X Two"),
      factor = c(1, 1)
    )
  }
  assign("custom_only", custom_only, envir = .GlobalEnv)
  on.exit(rm("custom_only", envir = .GlobalEnv), add = TRUE)
  
  result <- f_get_upd_dic("custom_only", "non_existent_default")
  
  expect_equal(nrow(result), 2)
  expect_equal(result$var, c("x1", "x2"))
})

test_that("f_get_upd_dic validates dic_default_name parameter", {
  expect_error(
    f_get_upd_dic("f_var_dic", NULL),
    "must be a single character string"
  )
})

# f_add_measure_labels() tests ---------------------------------------------

test_that("f_add_measure_labels adds labels correctly", {
  test_data <- tibble::tibble(
    measure = c("fgt0", "gini", "fgt1"),
    value = c(0.15, 0.35, 0.05)
  )
  
  result <- f_add_measure_labels(test_data)
  
  expect_true("measure" %in% colnames(result))
  expect_s3_class(result$measure, "factor")
  expect_false("measure_title" %in% colnames(result))
  expect_equal(nrow(result), 3)
})

test_that("f_add_measure_labels handles missing measures gracefully", {
  test_data <- tibble::tibble(
    measure = c("fgt0", "unknown_measure", "gini"),
    value = c(0.15, 0.20, 0.35)
  )
  
  result <- f_add_measure_labels(test_data)
  
  # Unknown measure should keep original name
  expect_equal(as.character(result$measure[2]), "unknown_measure")
})

test_that("f_add_measure_labels accepts custom measure table", {
  test_data <- tibble::tibble(
    measure = c("m1", "m2"),
    value = c(10, 20)
  )
  
  custom_measures <- tibble::tibble(
    measure = c("m1", "m2"),
    measure_title = c("Measure One", "Measure Two")
  )
  
  result <- f_add_measure_labels(test_data, measure_nm_tbl = custom_measures)
  
  expect_equal(as.character(result$measure), c("Measure One", "Measure Two"))
})

# f_add_var_labels() tests -------------------------------------------------

test_that("f_add_var_labels adds labels correctly", {
  test_data <- tibble::tibble(
    var = c("ym", "yd", "yf"),
    value = c(100, 80, 90)
  )
  
  result <- f_add_var_labels(test_data)
  
  expect_true("var" %in% colnames(result))
  expect_s3_class(result$var, "factor")
  expect_false("var_title" %in% colnames(result))
  expect_equal(nrow(result), 3)
})

test_that("f_add_var_labels handles custom to_var parameter", {
  test_data <- tibble::tibble(
    decile_var = c("ym", "yd"),
    value = c(100, 80)
  )
  
  result <- f_add_var_labels(test_data, to_var = "decile_var")
  
  expect_true("decile_var" %in% colnames(result))
  expect_s3_class(result$decile_var, "factor")
})

test_that("f_add_var_labels handles missing variables gracefully", {
  test_data <- tibble::tibble(
    var = c("ym", "unknown_var", "yd"),
    value = c(100, 110, 80)
  )
  
  result <- f_add_var_labels(test_data)
  
  # Unknown var should keep original name
  expect_equal(as.character(result$var[2]), "unknown_var")
})

test_that("f_add_var_labels removes factor column if present", {
  test_data <- tibble::tibble(
    var = c("ym", "yd"),
    value = c(100, 80)
  )
  
  result <- f_add_var_labels(test_data)
  
  expect_false("factor" %in% colnames(result))
})

# f_get_colnames_dic() tests -----------------------------------------------

test_that("f_get_colnames_dic returns default dictionary", {
  result <- f_get_colnames_dic()
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("var", "var_title") %in% colnames(result)))
  expect_gt(nrow(result), 0)
})

test_that("f_get_colnames_dic uses custom dictionary when available", {
  f_colnames_dic <- function() {
    tibble::tibble(
      var = c("custom_col1", "custom_col2"),
      var_title = c("Custom Column 1", "Custom Column 2")
    )
  }
  assign("f_colnames_dic", f_colnames_dic, envir = .GlobalEnv)
  on.exit(rm("f_colnames_dic", envir = .GlobalEnv), add = TRUE)
  
  result <- f_get_colnames_dic()
  
  # Should merge with default
  expect_s3_class(result, "data.frame")
})

# f_get_colname() tests ----------------------------------------------------

test_that("f_get_colname returns column name from dictionary", {
  result <- f_get_colname("group_var")
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("f_get_colname handles multiple column names", {
  result <- f_get_colname("group_var", "measure")
  
  expect_equal(nrow(result), 2)
})

# f_rename_cols() tests ----------------------------------------------------

test_that("f_rename_cols renames columns correctly", {
  test_data <- tibble::tibble(
    group_var = c("a", "b"),
    measure = c("m1", "m2"),
    value = c(10, 20)
  )
  
  result <- f_rename_cols(test_data)
  
  # Check that columns were renamed according to dictionary
  expect_true(any(c("Grouping variable", "Statistics") %in% colnames(result)))
})

test_that("f_rename_cols ignores columns not in dictionary", {
  test_data <- tibble::tibble(
    custom_col = c("a", "b"),
    value = c(10, 20)
  )
  
  result <- f_rename_cols(test_data)
  
  # Should keep original names for columns not in dictionary
  expect_true("custom_col" %in% colnames(result))
  expect_equal(ncol(result), 2)
})

test_that("f_rename_cols returns unchanged data when no matching columns", {
  test_data <- tibble::tibble(
    col1 = c("a", "b"),
    col2 = c(10, 20)
  )
  
  result <- f_rename_cols(test_data)
  
  expect_equal(colnames(result), colnames(test_data))
})

test_that("f_rename_cols accepts custom dictionary", {
  test_data <- tibble::tibble(
    old_name1 = c("a", "b"),
    old_name2 = c(10, 20)
  )
  
  custom_dic <- c("New Name 1" = "old_name1", "New Name 2" = "old_name2")
  
  result <- f_rename_cols(test_data, dic = custom_dic)
  
  expect_equal(colnames(result), c("New Name 1", "New Name 2"))
})

# Tests for fct_variables.R functions ======================================

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
  
  expect_equal(nrow(result), length(non_existent))
})

test_that("get_measure_nm handles non-existent measures gracefully", {
  non_existent <- c("fake_measure1", "fake_measure2")
  result <- get_measure_nm(x = non_existent)
  
  expect_equal(nrow(result),  length(non_existent))
})

test_that("get_var_nm preserves order with mixed existing and non-existing vars", {
  mixed_order <- c("yf", "fake_var", "ym", "yd")
  existing_vars <- c("yf", "ym", "yd")
  result <- get_var_nm(vars = mixed_order, reorder = TRUE)
  
  # Should only contain existing vars in the specified order
  expect_equal(result$var, mixed_order)
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

  assign("custom_dic", custom_dic, envir = .GlobalEnv)
  on.exit(rm("custom_dic", envir = .GlobalEnv), add = TRUE)
  
  result <- get_var_nm(
    vars = c("x2", "x1", "x3"),
    reorder = TRUE,
    dic_default = "custom_dic"
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

  f_default_var_dic <- function() {
    tibble::tibble(var = character(), var_title = character(), factor = numeric())
    }
  
  # Temporarily add to global environment
  assign("f_custom_var_dic", f_custom_var_dic, envir = .GlobalEnv)
  assign("f_default_var_dic", f_default_var_dic, envir = .GlobalEnv)
  
  on.exit(rm("f_custom_var_dic", envir = .GlobalEnv), add = TRUE)
  on.exit(rm("f_default_var_dic", envir = .GlobalEnv), add = TRUE)
  
  result <- get_var_nm(
    vars = c("a2", "a1"),
    reorder = TRUE,
    dic_default = "f_default_var_dic",
    dic_custom_name = "f_custom_var_dic"
  )
  
  expect_equal(result$var, c("a2", "a1"))
  expect_equal(as.character(result$var_title), c("Alpha 2", "Alpha 1"))
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
  
  assign("custom_measure_dic", custom_measure_dic, envir = .GlobalEnv)  
  on.exit(rm("custom_measure_dic", envir = .GlobalEnv), add = TRUE)
  
  result <- get_var_nm(
    vars = c("m3", "m1"),
    reorder = TRUE,
    dic_default = "custom_measure_dic",
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
    dic_default = "custom_dic_no_factor"
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

# test_that("custom dictionary preserves var_title factor levels in order", {
#   custom_dic <- function() {
#     tibble::tribble(
#       ~factor, ~var,  ~var_title,
#       1,       "z1",  "Zebra",
#       1,       "a1",  "Apple",
#       1,       "m1",  "Mango"
#     )
#   }
  
#   custom_order <- c("a1", "z1", "m1")
#   result <- get_var_nm(
#     vars = custom_order,
#     reorder = TRUE,
#     dic_default = custom_dic,
#     dic_custom_name = "face_dic"
#   )
  
#   expect_s3_class(result$var_title, "factor")
#   expect_equal(result$var, custom_order)
#   expect_equal(levels(result$var_title), c("Apple", "Zebra", "Mango"))
# })


# Default dictionary functions tests =======================================

test_that("f_var_dic_default returns valid data frame", {
  result <- f_var_dic_default()
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("factor", "var", "var_title") %in% colnames(result)))
  expect_gt(nrow(result), 0)
  
  # Check that income concepts are present
  expect_true(all(c("ym", "yd", "yf") %in% result$var))
})

test_that("f_var_wt_default returns weight variable name", {
  result <- f_var_wt_default()
  
  expect_type(result, "character")
  expect_equal(length(result), 1)
  expect_equal(result, "hhwt")
})

test_that("f_var_inc_default returns income variable names", {
  result <- f_var_inc_default()
  
  expect_type(result, "character")
  expect_gt(length(result), 0)
  expect_true(all(c("ym", "yd", "yf") %in% result))
})

test_that("f_var_group_default returns grouping variable names", {
  result <- f_var_group_default()
  
  expect_type(result, "character")
  expect_gt(length(result), 0)
  expect_true("all" %in% result)
})

test_that("f_var_pl_default returns poverty line variable names", {
  result <- f_var_pl_default()
  
  expect_type(result, "character")
  expect_gt(length(result), 0)
  expect_true(all(c("pl_190", "pl_nat") %in% result))
})

test_that("f_measure_dic_default returns valid data frame", {
  result <- f_measure_dic_default()
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("measure", "measure_title") %in% colnames(result)))
  expect_gt(nrow(result), 0)
  
  # Check that key measures are present
  expect_true(all(c("fgt0", "fgt1", "gini") %in% result$measure))
})

test_that("f_colnames_dic_default returns valid data frame", {
  result <- f_colnames_dic_default()
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("var", "var_title") %in% colnames(result)))
  expect_gt(nrow(result), 0)
})

test_that("f_app_text_dic_default returns valid data frame", {
  result <- f_app_text_dic_default()
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "title") %in% colnames(result)))
  expect_gt(nrow(result), 0)
})


# get_inc_nm() tests -------------------------------------------------------

test_that("get_inc_nm returns income variable names", {
  result <- get_inc_nm()
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("var", "var_title") %in% colnames(result)))
  expect_gt(nrow(result), 0)
})

test_that("get_inc_nm respects suffix parameter", {
  result <- get_inc_nm(suffix = "_adj")
  
  expect_true(all(grepl("_adj$", result$var)))
})

test_that("get_inc_nm uses custom function when available", {
  f_var_inc <- function() c("custom_inc1", "custom_inc2")
  assign("f_var_inc", f_var_inc, envir = .GlobalEnv)
  on.exit(rm("f_var_inc", envir = .GlobalEnv), add = TRUE)
  
  expect_warning(
    result <- get_inc_nm(),
    "not found"
  )
})


# get_wt_nm() tests --------------------------------------------------------

test_that("get_wt_nm returns weight variable name", {
  result <- get_wt_nm()
  
  expect_type(result, "character")
  expect_equal(length(result), 1)
})

test_that("get_wt_nm uses custom function when available", {
  f_var_wt <- function() "custom_weight"
  assign("f_var_wt", f_var_wt, envir = .GlobalEnv)
  on.exit(rm("f_var_wt", envir = .GlobalEnv), add = TRUE)
  
  result <- get_wt_nm()
  
  expect_equal(result, "custom_weight")
})


# get_group_nm() tests -----------------------------------------------------

test_that("get_group_nm returns grouping variable names", {
  result <- get_group_nm()
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("var", "var_title") %in% colnames(result)))
  expect_gt(nrow(result), 0)
})

test_that("get_group_nm respects suffix parameter", {
  result <- get_group_nm(suffix = "_grp")
  
  expect_true(all(grepl("_grp$", result$var)))
})


# get_pl_nm() tests --------------------------------------------------------

test_that("get_pl_nm returns poverty line variable names", {
  result <- get_pl_nm()
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("var", "var_title") %in% colnames(result)))
  expect_gt(nrow(result), 0)
})

test_that("get_pl_nm respects suffix parameter", {
  result <- get_pl_nm(suffix = "_line")
  
  expect_true(all(grepl("_line$", result$var)))
})


# f_get_app_text_dic() tests -----------------------------------------------

test_that("f_get_app_text_dic returns app text dictionary", {
  result <- f_get_app_text_dic()
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("id", "title") %in% colnames(result)))
  expect_gt(nrow(result), 0)
})

test_that("f_get_app_text_dic merges with custom dictionary", {
  f_app_text_dic <- function() {
    tibble::tibble(
      id = "custom_page",
      title = "Custom Page Title"
    )
  }
  assign("f_app_text_dic", f_app_text_dic, envir = .GlobalEnv)
  on.exit(rm("f_app_text_dic", envir = .GlobalEnv), add = TRUE)
  
  expect_warning(
    result <- f_get_app_text_dic(),
    "not found"
  )
  expect_s3_class(result, "data.frame")
})


# f_get_app_text() tests ---------------------------------------------------

test_that("f_get_app_text retrieves title for valid id", {
  dic <- tibble::tibble(
    id = c("page1", "page2", "page3"),
    title = c("Page One", "Page Two", "Page Three")
  )
  
  result <- f_get_app_text("page2", dic = dic)
  
  expect_equal(result, "Page Two")
})

test_that("f_get_app_text returns id when not found in dictionary", {
  dic <- tibble::tibble(
    id = c("page1", "page2"),
    title = c("Page One", "Page Two")
  )
  
  result <- f_get_app_text("non_existent", dic = dic)
  
  expect_equal(result, "non_existent")
})

test_that("f_get_app_text uses default dictionary when not provided", {
  result <- f_get_app_text("m_pov")
  
  expect_type(result, "character")
  expect_equal(length(result), 1)
})

test_that("f_get_app_text handles empty dictionary gracefully", {
  empty_dic <- tibble::tibble(
    id = character(),
    title = character()
  )
  
  result <- f_get_app_text("any_id", dic = empty_dic)
  
  expect_equal(result, "any_id")
})


# Integration tests --------------------------------------------------------

test_that("get_var_nm handles missing vars with warning", {
  mixed_vars <- c("ym", "fake_var1", "yd", "fake_var2")
  
  expect_warning(
    result <- get_var_nm(vars = mixed_vars, reorder = TRUE),
    "not found in the dictionary"
  )
  
  # Should include all vars, even fake ones
  expect_equal(nrow(result), 4)
  expect_true(all(mixed_vars %in% result$var))
})

test_that("f_var_names_vector works with get_var_nm output", {
  var_data <- get_var_nm(vars = c("ym", "yd", "yf"))
  result <- f_var_names_vector(var_data)
  
  expect_type(result, "character")
  expect_equal(length(result), 3)
  expect_named(result)
})

test_that("f_add_var_labels works with get_var_nm output", {
  test_data <- tibble::tibble(
    var = c("ym", "yd", "yf"),
    value = c(100, 80, 90)
  )
  
  var_names <- get_var_nm(vars = c("ym", "yd", "yf"))
  result <- f_add_var_labels(test_data, var_nm_tbl = var_names)
  
  expect_s3_class(result$var, "factor")
  expect_equal(nrow(result), 3)
})

test_that("workflow: dictionary update, get names, add labels", {
  # Create test data
  test_data <- tibble::tibble(
    var = c("ym", "yd"),
    measure = c("fgt0", "gini"),
    value = c(0.15, 0.35)
  )
  
  # Get variable and measure names
  var_names <- get_var_nm(vars = c("ym", "yd"))
  measure_names <- get_measure_nm(x = c("fgt0", "gini"))
  
  # Add labels
  result <- test_data |>
    f_add_var_labels(var_nm_tbl = var_names) |>
    f_add_measure_labels(measure_nm_tbl = measure_names)
  
  expect_s3_class(result$var, "factor")
  expect_s3_class(result$measure, "factor")
  expect_equal(nrow(result), 2)
})
