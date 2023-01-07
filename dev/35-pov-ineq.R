# Script for developing poverty and inequality functions.

# Setups ------------------------------------------------------------------
library(devCEQ)
library(tidyverse)
library(statar)
library(diffdf)

pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

# Loading simulation example results
sim_res <- read_rds("data-temp/sim_res.rds")

# Exemplary simulation results
one_sim <- sim_res$policy0$policy_sim_raw

# Gini and Theil --------------------------------------------------------------------

calc_gini(x = 1:10, w = 4:13)
calc_theil(x = 1:10, w = 4:13)
calc_gini(x = one_sim$yc_pc, w = one_sim$hhweight)
calc_theil(x = one_sim$yc_pc, w = one_sim$hhweight)

# Wrappers
one_sim %>%
  get_dta_gini(
    policy_name = "test",
    income_vars_tbl = tibble(
      var = c("yn_pc", "yf_pc"),
      var_title = c("var 1", "var 2")
    ),
    wt_var = NULL
  )

# Wrappers
one_sim %>% get_dta_gini(
  policy_name = "test",
  income_vars_tbl = tibble(
    var = c("yn_pc", "yf_pc"),
    var_title = c("var 1", "var 2")
  ),
  wt_var = "hhweight"
)


pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

# For nmultiple scenarios
gini_esimates <-
  sim_res %>%
  purrr::map(~ {
    get_dta_gini(
      dta = .x$policy_sim_raw,
      policy_name = .x$policy_name,
      income_vars_tbl = tibble(
        var = c("yn_pc", "yf_pc"),
        var_title = c("var 1", "var 2")
      ),
      wt_var = "hhweight",
      para_names =
        tibble(
          parameter = c("Gini", "Theil"),
          label = c("Gini index", "Theil index")
        )
    )
  }) %>%
  make_gini_pov(
    y = Gini,
    x = Income,
    color = Simulation,
    title = "Inequality. Gini index",
    xlab = "Income",
    digits = 4
  )

# gini_esimates$tbl_exp
# gini_esimates$gg$`Gini index`
# gini_esimates$gg$`Theil index`
# gini_esimates$ly



# # Gini as a module ----------------------------------------

# library(devCEQ)
library(tidyverse)
library(shiny)
library(esquisse)
library(shinyWidgets)
# sim_res <- read_rds("data-temp/sim_res.rds")
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)


source("dev/35-1-fun-helpers.R")
test_results_mod(
  sim_res = read_rds("data-temp/sim_res.rds"),
  ui_side = mod_gini_ui,
  server_side = mod_gini_pov_gen_server, id = "NULL")



# Poverty -----------------------------------------------------------------
#
# pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
# one_sim %>%
#   get_dta_pov(
#     policy_name = "test",
#     wt_var = "hhweight",
#     poverty_line_var = "line_55"
#     )
#
# pov_esimates <-
#   sim_res %>%
#   purrr::map(~ {
#     get_dta_pov(.x$policy_sim_raw, .x$policy_name, poverty_line_var = "line_55")
#   }) %>%
#   make_gini_pov(
#     # y = Poverty,
#     x = Income,
#     color = Simulation,
#     title = "Poverty",
#     ylab = "Poverty rate, %",
#     xlab = "Income"
#   )
#
# pov_esimates$tbl_dt %>%
#   arrange(Income, Parameter)
#
# pov_esimates$gg$`Poverty rate`

# library(devCEQ)
library(tidyverse)
library(shiny)
library(esquisse)
library(shinyWidgets)
# sim_res <- read_rds("data-temp/sim_res.rds")
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)


source("dev/35-1-fun-helpers.R")
test_results_mod(
  sim_res = read_rds("data-temp/sim_res.rds"),
  ui_side = mod_gini_ui,
  server_side =
    function(id, sim_res, ...) {
      mod_gini_pov_gen_server(
        id = id,
        sim_res = sim_res,
        title = "Poverty",
        export_btn_title = "Save plot",
        pl_choices = c(
          "Seuil pauvrete national" = "zref",
          "International poverty line 5.5 USD (2011 PPP)" = "line_55",
          "International poverty line 3.2 USD (2011 PPP)" = "line_32",
          "International poverty line 1.9 USD (2011 PPP)" = "line_19"
          ),
        pl_title = "Poverty line",
        get_dta_fn = function(dta, policy_name, poverty_line_var, ...) {
          get_dta_pov(
            dta = dta,
            policy_name = policy_name,
            poverty_line_var = poverty_line_var,
            poverty_line_value = 1,
            income_vars_tbl = get_inc_nm(),
            wt_var = get_wt_nm(),
            para_names =
              tibble(
                parameter = c(
                  "rate",
                  # "headcount",
                  "gap",
                  "severity"
                  ),
                label = c(
                  "Poverty rate",
                  # "Poverty headcount",
                  "Poverty gap",
                  "Poverty severity"
                ) %>% factor(., levels = .)
              ),
            ...
          )
        },
        make_plot_fn = function(dta, ...) {
                   make_gini_pov(
                     dta = dta,
                     x = Income,
                     color = Simulation,
                     title = "Poverty",
                     xlab = "Income",
                     ...
                   )
                 },
        ...)
    }, id = "NULL")
