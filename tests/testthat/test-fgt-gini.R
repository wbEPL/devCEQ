testthat::skip("Skipping FGT and Gini tests")

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

ui <- page_fluid(
  layout_columns(
    widths = c(6, 6),
    card(
      mod_gini_ui("gini"),
      fill = TRUE
    ),
    card(
      mod_gini_ui("pov"),
      fill = TRUE
    )
  )
)

srv <- function(input, output, session) {
  mod_gini_pov_gen_server(
    id = "gini",
    sim_res = reactive(dta_sim),
    income_vars_tbl = get_inc_nm(),
    poverty_line_value = median(dta_hh$ym) * 0.6,
    wt_var = get_wt_nm(),
    group_vars_tbl = get_group_nm()
  )
  
  mod_gini_pov_gen_server(
    id = "pov",
    sim_res = reactive(dta_sim),
    income_vars_tbl = get_inc_nm(),
    poverty_line_value = median(dta_hh$ym) * 0.6,
    wt_var = get_wt_nm(),
    group_vars_tbl = get_group_nm()
  )
}

shinyApp(ui, srv)
