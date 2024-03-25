# This is a pre-simulation step, where we mimic stata code and load
#
library(devCEQ)
library(tidyverse)
library(haven)
library(diffdf)

pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

# Folders
fldr_app <- "."    # Place where the app data is.
fldr <- "..\\contryCEQ"

# Stata - related folders
fldr_raw <- file.path(fldr, "01. Data\\1_raw\\") |> normalizePath()
fldr_presim <- file.path(fldr, "01. Data\\2_pre_sim\\") |> normalizePath()
fldr_temp <- file.path(fldr, "01. Data\\3_temp_sim\\") |> normalizePath()

# Loading inputs
inp_xlsx <- devCEQ::load_input_xlsx(file.path(fldr_app, "./data-app/simple-inputs-structure.xlsx"))
inps_app <- inp_xlsx %>% filter(!is.na(inputId)) %>% filter(include) %>% get_all_inps()
inps_all <- inp_xlsx %>% get_all_inps()
inps <- add_missing_inp_generic(inps_app, inps_all)

# # Loading pre-sim data
presim <- read_rds("data-app/presim.rds")

# Starting the simulation -------------------------------------------------

sim <- list()

# 02. Income Tax.do ------------------------------------------------------

sim <- step_1_income_tax(sim, inps, presim)

# profiling
# profvis::profvis({get.02_income_tax(sim, inps, presim)})

# # Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# inctx_collapsed <-
#   file.path(fldr_temp, "income_tax_collapse.dta") |>
#   haven::read_dta() %>%
#   haven::zap_labels()%>%
#   haven::zap_label()%>%
#   haven::zap_formats()
#
# sim$income_tax_collapse %>%
#   diffdf(inctx_collapsed, tolerance = 0.0001, keys = "hhid") %>%
#   get_diffdf_sum()
#
# dt_complete <-
#   file.path(fldr_temp, "Direct_taxes_complete_Senegal.dta") |>
#   haven::read_dta() %>%
#   haven::zap_labels()%>%
#   haven::zap_label()%>%
#   haven::zap_formats()
#
# sim$direct_tax_complete %>%
#   diffdf(dt_complete, tolerance = 0.0001) %>%
#   get_diffdf_sum()

# 03. SocialSecurityContributions.do ------------------------------------------

sim <- step_2_vat(sim, inps, presim)

# # Checks --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# social_security_contribs_stata <-
#   file.path(fldr_temp, "social_security_contribs.dta") %>%
#   haven::read_dta() %>%
#   haven::zap_labels()%>%
#   haven::zap_label()%>%
#   haven::zap_formats()
#
# sim$social_security_contribs %>%
#   diffdf(social_security_contribs_stata, tolerance = 0.0001) %>%
#   get_diffdf_sum()

# csp_fnr and csh_ipm are two irrelevant simulations as users do not change them


# 10. Income_Aggregate_cons_based.do  --------------------------------------

output_ref <- step_99_agg_results(sim, inps, presim)

# output_ref
# # Check
# output_stata <-
#   file.path(fldr_temp, "..", "4_sim_output", "output_ref.dta") %>%
#   haven::read_dta() %>%
#   haven::zap_labels() %>% haven::zap_label() %>% haven::zap_formats()
#
# out_diff <-
#   output_ref %>%
#   mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
#   diffdf(output_stata, tolerance = 1, keys = c("hhid"))
# # #
# out_diff %>% get_diffdf_sum() %>% print(n = 50)
# out_diff$VarDiff_income_tax
# out_diff$VarDiff_dirtax_total_pc
# out_diff$VarDiff_line_2


# # # # Check --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# output_ref_stata <-
#   file.path(tempdta_root, "..", "4_sim_output", "output_ref.dta") %>%
#   haven::read_dta() %>%
#   haven::zap_labels()%>%
#   haven::zap_label()%>%
#   haven::zap_formats()
#
# output_ref_diff <- output_ref %>% diffdf(output_ref_stata, tolerance = 1, keys = "hhid")
# output_ref_diff %>%  get_diffdf_sum()


# Complete CEQ ------------------------------------------------------------

presim <- read_rds("data-app/presim.rds")
sen_out <- full_ceq(inps, presim)
sen_out %>% write_rds("data-app/baseline.rds", compress = "xz")

# # Check
# output_stata <-
#   file.path(fldr_temp, "..", "4_sim_output", "output_ref.dta") %>%
#   haven::read_dta() %>%
#   haven::zap_labels() %>% haven::zap_label() %>% haven::zap_formats()
#
# out_diff <-
#   sen_out %>%
#   mutate(across(where(is.numeric), ~ifelse(is.na(.), 0, .))) %>%
#   diffdf(output_stata, tolerance = 0.01, keys = c("hhid"))
#
# out_diff %>% get_diffdf_sum() %>% print(n = 50)
# out_diff$VarDiff_trimf

