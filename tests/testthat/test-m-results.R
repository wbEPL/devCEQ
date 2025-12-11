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

## Inputs module --------------------------------------------
# test_m_input()

## Diagnostic module --------------------------------------------
# test_m_diagnostics()

## Figure module --------------------------------------------
# test_m_figure()

## Incidences module --------------------------------------------
# test_m_incid(page_ui = f_incid_ui_linear)
# test_m_incid(page_ui = f_incid_ui_card)

## Page switching --------------------------------------------
# test_m_res_switches()

## Insidences results module with switches and all results collection ----------
# test_m_incid_switches()


## Sim results aggregation logic --------------------------------------------

dta <- dta_sim$policy1$policy_sim_raw |> mutate(pl_nat = median(ym) * 0.4)

f_calc_povineq(
  dta = dta,
  var_inc = c("ym", "yn", "yp", "yg", "yd", "yc", "yf"),
  var_wt = "hhwt",
  pl_var = "pl_nat",
  group_var = "total"
)

f_calc_povineq_by(
  dta = dta,
  var_inc = c("ym", "yn", "yp", "yg", "yd", "yc", "yf"),
  var_wt = "hhwt",
  pl_var = "pl_nat",
  group_vars = c("total", "group_1", "group_2")
) |> 
  count(group_var, group_val )


# Calculating for all simulations and plotting ----------------------------
dta_sim |> 
  f_calc_pov_stats()


dta_fig <-
  f_calc_povineq_by_sims(
    dta_sim = dta_sim,
    var_inc = c("ym", "yn", "yp", "yg", "yd", "yc", "yf"),
    var_wt = "hhwt",
    pl_var = "pl_nat",
    group_vars = c("total", "group_1")
  ) |> 
  f_add_measure_labels() |> 
  f_add_var_labels() |> 
  f_add_var_labels(to_var = "group_var") |> 
  f_rename_cols()


x_var <- "var" |> f_get_colname()
y_var <- "value"  |> f_get_colname()
color_var <- "group_val"  |> f_get_colname()
facet_var <- "sim" |> f_get_colname()

measure_fltr <- get_measure_nm("fgt1")$measure_title
fig_by <- "measure" |> f_get_colname()


# dta_fig1 <-
dta_fig |>
  filter(if_any(any_of(f_get_colname(fig_by)), ~ . == measure_fltr)) |>
  f_plot_gg(
    x_var = "var",
    y_var = "value",
    color_var = "group_val",
    facet_var = "sim",
    type = "bar"
  )

dta_fig |>
  group_by(across(any_of(fig_by))) |>
  nest() |> 
  mutate(
    gg = map(
      data,
      ~ f_plot_gg(
        dta = .x,
        x_var = "var",
        y_var = "value",
        color_var = "group_val",
        facet_var = "sim",
        type = "bar"
      )
    )
  )
