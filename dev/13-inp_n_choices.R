# Space for testing the n simulation module

golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

library(tidyverse)
library(shiny)

# The module
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

# Testing input UI
test_mod_inp_n_choices_all_ui()

# Testing full module with server logic
test_mod_inp_n_choices()
test_mod_inp_n_choices("someNS")
