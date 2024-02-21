
# reload package and all functions
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)


# Setups ---------------------------------------------------------------------
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)

# Simple test for users

## If the UI is generating
test_mod_inputs("inst/simple-inputs-structure.xlsx")

test_mod_inputs(
  "../Senegal/senCEQapp/data-app/sen-inputs-structure-new.xlsx",
  n_policy = c(1,3,1),
  n_policy_type = "slider",
  type = "full"
)

# Additional UI tests

## Buttons module
test_mod_inp_n_choices()
test_mod_inp_n_choices_all_ui()
test_mod_inputs_btn_static_ui()
test_mod_inputs_btn()
