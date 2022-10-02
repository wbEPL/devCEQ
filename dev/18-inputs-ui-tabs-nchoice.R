# Space for testing the inputs tabs

golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

library(tidyverse)
library(shiny)


# Testing input UI generation with complete re-activity  ------------------------

pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

test_mod_inp_n_choices_tabs("data-raw/ceq-inputs-idn-2022.xlsx")
test_mod_inp_n_choices_tabs("data-raw/simple-inputs-structure.xlsx")

# Seting full input UI ----------------------------------------------------------


pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
test_mod_inputs("data-raw/ceq-inputs-idn-2022.xlsx", type = "full")

# # Optimizing gen_one_inp_table --------------------------------------------------
#
# path <- "data-raw/ceq-inputs-idn-2022.xlsx"
#
# inp_raw_str <- path %>% load_input_xlsx()
# inp_tab_str <- path %>% load_inputtabs_xlsx()
# inp_table_str <- path %>% load_inputtables_xlsx()
#
# local_inp_str = gen_inp_str_front(inp_table_str = inp_table_str)
# local_ui_fn = gen_tabinp_ui_front(inp_tab_str = inp_tab_str,
#                                   inp_table_str = inp_table_str)
#
#
#
# ind_str_new <- local_inp_str(inp_raw_str, 3)
# # profvis::profvis({
# #   ind_str_new <- local_inp_str(inp_raw_str, 10)
# #   gen_all_inp_tables(inp_ui_str = ind_str_new, inp_table_str)
# # })
#
# # # profvis::profvis({
# #   inp_table_str %>%
# #     map(~{
# #       browser()
# #       gen_one_inp_table(inp_ui_str = ind_str_new, .x)
# #     })
# #
# # # })


