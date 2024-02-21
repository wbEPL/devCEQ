
# reload package and all functions
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)


# Setups ---------------------------------------------------------------------
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)


# UI template
# path <- "inst/simple-inputs-structure.xlsx"
# inp_raw_str <- path %>% load_input_xlsx()

# test_mod_inputs(
#   "inst/simple-inputs-structure.xlsx",
#   n_policy = c(1,3,1)#,
#   # n_policy_type = "numeric",
#   # type = "full"
# )


test_mod_inputs(
  "../Senegal/senCEQapp/data-app/sen-inputs-structure-new.xlsx",
  n_policy = c(1,3,1),
  n_policy_type = "slider",
  type = "full"
)
