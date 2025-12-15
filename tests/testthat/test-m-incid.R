testthat::skip()

pkgload::load_all()
library(shiny)
library(shinyWidgets)
library(bslib)

dta_sim_local <- 
  dta_sim |> 
  map(~{
    .x$policy_sim_raw <-
      .x$policy_sim_raw |>
      mutate(
        pl_nat = median(ym) * 0.4,
        pl_190 = median(ym) * 0.6,
        pl_500 = median(ym) * 0.8
      ) #|> 
      # calc_deciles(
      #   dec_var = c("ym", "yn", "yp", "yg", "yd", "yc", "yf"),
      #   wt_var = "hhwt",
      #   n_dec = 10
      # ) 
    .x
  }) 


# Step 1 add deciles to data if not present --------------------------------------------

dta_sim_local$policy0$policy_sim_raw |> 
  f_calc_deciles(
    dec_var = get_inc_nm()$var,
    wt_var = "hhwt",
    n_dec = 4
  ) 

dta_sim_local |>
  f_calc_deciles_by_sim(
    dec_var = c("ym", "yn", "yp", "yg", "yd", "yc", "yf"),
    wt_var = "hhwt",
    n_dec = 10
  )

# Step 2 aggregate stats by decile --------------------------------------------

dta_sim_local$policy0$policy_sim_raw |>
  f_calc_deciles(
    dec_var = get_inc_nm()$var,
    wt_var = "hhwt",
    n_dec = 4
  ) 

# Make a function for aggregating by deciles across simulations and computing relative, absolute and level incidence


agg_sims_by_deciles(
  dec_by = input$by_income,
  dec_vars = dec_vars,
  wt_var = get_wt_nm(),
  n_dec = n_dec_debounced(),
  get_var_fn = get_var_nm,
  ...
) 

## Poverty aggregation logic --------------------------------------------
f_calc_povineq(
  dta = dta_sim_local$policy1$policy_sim_raw,
  var_inc = c("ym"),
  var_wt = "hhwt",
  pl_var = "pl_190",
  group_var = "total"
)

# Poverty by groups --------------------------------------------
f_calc_povineq_by(
  dta = dta_sim_local$policy1$policy_sim_raw,
  var_inc = c("ym", "yn", "yp", "yg", "yd", "yc", "yf"),
  var_wt = "hhwt",
  pl_var = "pl_nat",
  group_vars = c("total", "group_1", "group_2")
) |>
  count(var)
  count(group_var, group_val)

# Poverty across all simulations --------------------------------------------
f_calc_povineq_by_sims(
    dta_sim = dta_sim_local,
    var_inc = c("ym", "yn", "yp", "yg", "yd", "yc", "yf"),
    var_wt = "hhwt",
    pl_var = "pl_nat",
    group_vars = c("total", "group_1")
  )

# Wrapper for all simulaiton with exaustive table of results ---------------
dta_sim_local |> f_calc_pov_stats()
dta_sim_local |> f_calc_pov_stats() |> count(Statistics)
dta_sim_local |> f_calc_pov_stats() |> count(Variable)
dta_sim_local |> f_calc_pov_stats() |> count(`Grouping variable`, Group)
dta_sim_local |> f_calc_pov_stats() |> count(Simulation)


# Plotting --------------------------------------------
dta_fig <- dta_sim_local |> f_calc_pov_stats()


# Filter appropriate groupping variables ------------------------------

dta_fig |> f_filter_grouped_stats("group_1") |> count(`Grouping variable`, Group)
dta_fig |> f_filter_grouped_stats("all") |> count(`Grouping variable`, Group)
dta_fig |> f_filter_grouped_stats("all_groups") |> count(`Grouping variable`, Group)
dta_fig |> f_filter_grouped_stats("all_groups") |> count(Statistics)

# Plotting specific measure --------------------------------------------

measure_fltr <- get_measure_nm("fgt0")$measure_title
fig_by <- "measure" |> f_get_colname()

dta_fig |>
  filter(if_any(any_of(f_get_colname(fig_by)), ~ . == as.character(measure_fltr))) |>
  f_plot_gg(
    x_var = "var",
    y_var = "value",
    color_var = "group_val",
    facet_var = "sim",
    type = "bar"
  ) |>
  plotly::ggplotly(tooltip = "text")


dta_sim |> f_calc_pov_stats() |> f_plot_pov_by(fig_by = "measure") 


# Plot all group-specific plots in a large list ----------------------------

all_groups <- c("all", "group_1", "group_2", "all_groups")

f_plot_gg_groups <- function(dta, group_vars) {
  fig_list <- list()
  for (grp in group_vars) {
    dta_filt <- dta |> f_filter_grouped_stats(grp)
    fig_out <- dta_filt |>
      f_plot_pov_by(
        x_var = "var",
        y_var = "value",
        color_var = "group_val",
        facet_var = "sim",
        type = "bar"
      )
    fig_list[[grp]] <- fig_out
  }
  return(fig_list)
}

figs_all_groups <- f_plot_gg_groups(dta_fig, all_groups)

# Formattin tables  -------------------------------------

dta_fig |> f_format_tbl() |> f_format_rt(col_min_groups = 1)


# Testing the module --------------------------------------------
devmode()
test_m_pov(sim_res = reactive(req(dta_sim)))




