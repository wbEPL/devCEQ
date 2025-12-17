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

dta_deciles <-
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

# Aggregating bsaed on one groupping variable
dta_deciles |>
  f_agg_by_decile_one(
    var_decile = "ym___decile",
    var_agg = c("ym", "dtx_prog1", "dtx_prog2"),
    wt_var = get_wt_nm()
  )

dta_deciles |>
  f_agg_by_decile_one(
    var_decile = "ym___decile",
    var_agg = c("ym", "dtx_prog1", "dtx_prog2"),
    var_group = "group_2",
    wt_var = get_wt_nm()
  )


# Aggregating based on multiple groupping variables
dta_deciles |>
  f_agg_by_decile(
    var_decile = c("ym___decile", "yn___decile", "yp___decile"),
    var_agg = c("ym", "yn", "yp",  "dtx_prog1", "dtx_prog2"),
    var_group = c("all", "group_2", "group_1"),
    wt_var = get_wt_nm()
  ) 


dta_deciles |>
  f_agg_by_decile(
    var_decile = c("ym___decile", "yn___decile", "yp___decile"),
    var_agg = c("ym", "yn", "yp",  "dtx_prog1", "dtx_prog2"),
    var_group = "all",
    wt_var = get_wt_nm()
  )

# Step 3 aggregate by deciles across simulations --------------------------------------------
dec_agg <- 
  dta_sim_local |>
  f_calc_deciles_by_sim(
    dec_var = get_inc_nm()$var[1:2],
    wt_var = get_wt_nm(),
    n_dec = 3
  ) |>
  f_agg_by_decile_by_sim(
    var_decile = str_c(get_inc_nm()$var[1:2], "___decile"),
    var_agg = c(get_inc_nm()$var[2:3], "dtx_prog1"),
    var_group = c("all", "group_1"), 
    wt_var = get_wt_nm()
  )

dec_agg |> count(decile_var, var)


dec_agg |> f_calc_incidence() |> count(decile_var, var, measure)

# Formatting incidences data
dec_agg |> f_calc_incidence() |> f_format_incidence() |> 
  count(`Decile by`, Variable, Statistics)

# Filtering clean incidences data
dec_agg |> 
  f_calc_incidence() |> 
  f_format_incidence() |> 
  f_filter_grouped_stats("all") |> 
  # count(Statistics)
  f_filter_var_generic("Relative incidence, % of Market income", "measure") |> 
  count(Statistics)


# Full process ------------------------------------------------------------

n_dec <- 5
agg_var <- c("ym", "dtx_prog1")
dec_var <- c("ym", "yn") #get_inc_nm()$var
wt_var <- get_wt_nm()
group_var <-  get_group_nm()$var[-length(get_group_nm()$var)]


# Step 1. Calcualte incidences ---------------------------------------------
dta_1_incid <-
  dta_sim_local |>
  f_calc_deciles_by_sim(
    dec_var = dec_var,
    wt_var = wt_var,
    n_dec = n_dec
  ) |>
  f_agg_by_decile_by_sim(
    var_decile = str_c(dec_var, "___decile"),
    var_agg = agg_var,
    var_group = group_var,
    wt_var = wt_var
  ) |>
  f_calc_incidence(force_abs = TRUE) |>
  f_format_incidence()  


# Step 2. Filter appropriate groupping variables ------------------------------


all_measures <- dta_1_incid |> f_get_var_uniq_vals("measure")
measure_fltr <- all_measures[1]

all_decile_by <- dta_1_incid |> select(any_of(f_get_colname("decile_var"))) |> pull(1) |> unique() |> as.character()
decile_fltr <- all_decile_by[1]

all_groups <- dta_1_incid |> select(any_of(f_get_colname("group_var"))) |> pull(1) |> unique() |> as.character()
grpoup_fltr <- all_groups[2]

all_vars <- dta_1_incid |> select(any_of(f_get_colname("var"))) |> pull(1) |> unique() |> as.character()
var_fltr <- all_vars[1]

dta_plot <-
  dta_1_incid |>
  f_filter_grouped_stats(grpoup_fltr) |>
  f_filter_var_generic(measure_fltr, "measure") |>
  f_filter_var_generic(decile_fltr, "decile_var")

dta_plot_one <- 
  dta_plot |>
  f_filter_var_generic(var_fltr, "var") 

dta_plot_one|>
  f_plot_gg(
    x_var = "decile",
    y_var = "value",
    x_lab = f_get_app_text("decile"),
    color_var = "group_val",
    facet_var = "sim",
    type = "bar"
  )

figs <-
  dta_plot |>
  select(any_of(f_get_colname(c("var")))) |>
  distinct() |>
  pull(1) |>
  as.character() |>
  (\(x) set_names(x, x))() |>
  imap(
    ~ {
      # browser()
      dta_plot |>
        f_filter_var_generic(.x, "var") |> # count(Variable)
        f_plot_gg(
          x_var = "decile",
          y_var = "value",
          x_lab = f_get_app_text("decile"),
          color_var = "group_val",
          facet_var = "sim",
          type = "bar"
        )
    }
  )
  
# Formattin tables  -------------------------------------

dta_tbl <- 
  dta_1_incid |>
  f_format_decile_tbl() |> 
  f_format_rt(col_min_groups = 1)




# Testing the module --------------------------------------------
devmode()
test_m_incid(sim_res = reactive(req(dta_sim)))




