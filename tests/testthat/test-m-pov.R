testthat::skip()

# library(bslib)
# library(shiny)

# bslib::page_sidebar(
#   id = "main_sidebar_panel",
#   sidebar = bslib::sidebar(
#     title = NULL,
#     width = 350,
#     open = "always",
#     sb_content
#   ),
#   fig_content
# )

pkgload::load_all()
library(shiny)
library(shinyWidgets)
library(bslib)

## Poverty aggregation logic --------------------------------------------
f_calc_povineq(
  dta = dta_sim$policy1$policy_sim_raw,
  var_inc = c("ym", "yn", "yp", "yg", "yd", "yc", "yf"),
  var_wt = "hhwt",
  pl_var = "pl_nat",
  group_var = "total"
)

# Poverty by groups --------------------------------------------
f_calc_povineq_by(
  dta = dta_sim$policy1$policy_sim_raw,
  var_inc = c("ym", "yn", "yp", "yg", "yd", "yc", "yf"),
  var_wt = "hhwt",
  pl_var = "pl_nat",
  group_vars = c("total", "group_1", "group_2")
) |> 
  count(group_var, group_val )

# Poverty across all simulations --------------------------------------------
f_calc_povineq_by_sims(
    dta_sim = dta_sim,
    var_inc = c("ym", "yn", "yp", "yg", "yd", "yc", "yf"),
    var_wt = "hhwt",
    pl_var = "pl_nat",
    group_vars = c("total", "group_1")
  )

# Wrapper for all simulaiton with exaustive table of results ---------------
dta_sim |> f_calc_pov_stats()
dta_sim |> f_calc_pov_stats() |> count(Statistics)
dta_sim |> f_calc_pov_stats() |> count(Variable)
dta_sim |> f_calc_pov_stats() |> count(`Groupping variable`, Group)
dta_sim |> f_calc_pov_stats() |> count(Simulation)


# Plotting --------------------------------------------
dta_fig <- dta_sim |> f_calc_pov_stats()

measure_fltr <- get_measure_nm("fgt0")$measure_title
fig_by <- "measure" |> f_get_colname()

dta_fig |>
  filter(if_any(any_of(f_get_colname(fig_by)), ~ . == measure_fltr)) |>
  f_plot_gg(
    x_var = "var",
    y_var = "value",
    color_var = "group_val",
    facet_var = "sim",
    type = "bar"
  )


dta_sim |> f_calc_pov_stats() |> f_plot_pov_by(fig_by = "measure") |> names()

# Building a poverty module --------------------------------------------

dta_fig |>
  pivot_wider(
    names_from = any_of(unname(f_get_colname(c("group_var", "group_val")))),
    values_from = any_of(unname(f_get_colname(c("value")))),
    names_sep = "__"
  ) |>
  f_reactable(separator = "__")



# Apply formatting of numbers by type of the measure -------------------------------------

dta_fig |> f_format_tbl() |> f_format_rt(col_min_groups = 1)




devmode()
test_m_incid(page_ui = f_incid_ui_linear)


