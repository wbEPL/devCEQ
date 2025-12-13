testthat::skip("Skipping FGT and Gini tests")

# Old poverty and gini module logic tests ------------------------------

library(testthat)
pkgload::load_all()

# Testing individual functions for FGT and Gini calculations --------------

get_dta_gini(
  dta = dta_hh,
  policy_name = "Baseline",
  income_vars_tbl = get_inc_nm(),
  wt_var = get_wt_nm()
)

get_dta_pov(
  dta = dta_hh,
  policy_name = "Baseline",
  income_vars_tbl = get_inc_nm(),
  poverty_line_value = median(dta_hh$ym) * 0.6,
  wt_var = get_wt_nm(),
  group_vars_tbl = get_group_nm()
)

dta_hh |> 
  mutate(povline = median(ym) * 0.6) |>
  get_dta_pov(
    policy_name = "Baseline",
    income_vars_tbl = get_inc_nm(),
    poverty_line_var = "povline",
    wt_var = get_wt_nm(),
    group_vars_tbl = get_group_nm()
  )



# Testing modules ------------------------------------------------

library(shiny)
library(bslib)

test_gini_poverty_old()
