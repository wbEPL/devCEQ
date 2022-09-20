# Space for testing the inputs butts simulation module

golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

library(tidyverse)
library(shiny)

# The module
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

# Testing input UI
test_mod_inputs_btn_static_ui()
test_mod_inputs_btn()




