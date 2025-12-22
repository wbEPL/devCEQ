testthat::skip()

pkgload::load_all()
library(shiny)
library(shinyWidgets)
library(bslib)



# Testing the module --------------------------------------------
devmode()
test_m_incid(sim_res = reactive(req(dta_sim)))
