# Here we test how aggregation happens in the package

pkgload::load_all()

# Main quantile function ----------------------------------------------------
big_n <- runif(1000000)
big_wt <- runif(1000000)

all.equal(get_quantiles(big_n, n = 10, wt = big_wt),
          get_quantiles_stata(big_n, n = 10, wt = big_wt))

microbenchmark::microbenchmark(
  get_quantiles(big_n, n = 10, wt = big_wt)[1:10],
  get_quantiles_stata(big_n, n = 10, wt = big_wt)[1:10],
  times = 20
)


# Checking deciles variables adding to the data frame ----------------------

test_that("All deciles variables are produced:", {
  dec_vars <-
    dta_hh |>
    calc_deciles(
      dec_var = get_inc_nm(suffix = NULL)$var,
      wt_var = get_wt_nm(),
      n_dec = 10
    ) |>
    select(contains("decile"))

  expect_equal(ncol(dec_vars), length(get_inc_nm(suffix = NULL)$var))
})