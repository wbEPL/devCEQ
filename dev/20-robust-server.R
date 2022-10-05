# Developing and testing a robust server runner

golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

library(tidyverse)
library(shiny)

# The logic.
# Server function is represented by `mod_generic_run_sim_server`
# it has inside some arguments that are passed to it from the app server
# `run`, `presim`, `inps`, `all_inps`, and `ceq_progress`. Other three functions
# are supplied by the user to the

# Testing if everything works is possible with a simple shiny app ----------
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
test_mod_run_sim()

# Calculating inputs based on saved inputs from shiny



# testServer(
#   mod_generic_run_sim_server,
#   args = list(
#     run = reactive(1),
#     presim = reactive(tibble(presim = c(1:3))),
#     inps = reactive(
#       list(policy0 = list(policy_choices = list(a = 1, b = 2)),
#            policy1 = list(policy_choices = list(a = 3, b = 4)))),
#     all_inps = reactive(list(c = 3))
#   ),
#   {
#     session$setInputs(a = 1)
#   }
# )

