
pkgload::load_all()

testthat::test_that("Duplicated factr levels are made unique", {
  var_string <- sample(c(letters[1:5]), 10, rep = T)
  var_fct <- fct_keep_dup_string(var_string) %>% as.factor()
  testthat::expect_equal(length(levels(var_fct)), length(var_string))
  testthat::expect_equal(length(var_fct), length(var_string))
})

