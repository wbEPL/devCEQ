# Here we test how aggregation happens in the package
testthat::skip()
pkgload::load_all()


# Deciles -------------------------------------------------------------------

## Main quantile ----------------------------------------------------
big_n <- runif(1000000)
big_wt <- runif(1000000)

all.equal(get_quantiles(big_n, n = 10, wt = big_wt),
          get_quantiles_stata(big_n, n = 10, wt = big_wt))

microbenchmark::microbenchmark(
  get_quantiles(big_n, n = 10, wt = big_wt)[1:10],
  get_quantiles_stata(big_n, n = 10, wt = big_wt)[1:10],
  times = 20
)

test_that("get_quantiles and get_quantiles_stata produce the same output", {
  expect_equal(
    get_quantiles(big_n, n = 10, wt = big_wt),
    get_quantiles_stata(big_n, n = 10, wt = big_wt)
  )
})

## Checking deciles variables adding to the data frame ----------------------
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
  expect_true(all(sapply(dec_vars, function(x) !is.na(x))))
})


## Aggregation correctness ------------------------------------------------

test_that(
  "Aggregation by deciles produces expected number of rows",
  {
    agg_deciles <-
      dta_hh |>
      calc_deciles(
        dec_var = get_inc_nm(suffix = NULL)$var,
        wt_var = get_wt_nm(),
        n_dec = 10
      ) |>
      calc_agg_by(
        vars = get_var_nm(suffix = NULL)$var,
        by_var = "ym_decile",
        wt_var = get_wt_nm()
      )

    expect_equal(nrow(agg_deciles), 10)
  }
  
)

## Aggregation wors across policies -----------------------------------------------