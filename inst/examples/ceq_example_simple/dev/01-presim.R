# This is a pre-simulation step, where we mimic stata code and load
library(devCEQ)
library(tidyverse)
library(haven)
library(diffdf)

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

# Temp and presim data -----------------------------------------------------------
presim <- list()

# 02. Income Tax.do -----------------------------------
# presim$income_tax_input <-
#   file.path(fldr_presim, "02_Income_tax_input.dta") %>%
#   read_dta() %>%
#   haven::zap_labels() %>%
#   select(hhid, hhweight, hhsize, age,
#          s01q07, s04q38, s04q39, s04q30c, s04q30d, s04q31, s04q42, s05q02, s05q04,
#          s05q08, inclab,
#          pension_invalidite_widow, chi_h, total_income_apportant, formal_definitivo)

# 03. SocialSecurityContributions.do -----------------------------------

# 04. Direct_transfers.do -----------------------------------
# presim$`07_dir_trans_PMT` <-
#   file.path(fldr_presim, "07_dir_trans_PMT.dta") %>%
#   read_dta() %>% haven::zap_labels()  %>%
#   select(hhid, hhsize, hhweight, region, departement, PMT, pmt_seed)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# Re-saving pre-sim data --------------------------------------------------

presim %>% write_rds(file.path(fldr_app, "data-app", "presim.rds"), compress = "gz")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
