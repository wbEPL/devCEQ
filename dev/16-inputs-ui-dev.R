# golem::detach_all_attached()
# golem::document_and_reload()

library(shiny)
library(tidyverse)

# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

path <- "../data-raw/complex-inputs-structure.xlsx"
inp_raw_str <- path %>% load_input_xlsx()
inp_tab_str <- path %>% load_inputtabs_xlsx()
inp_table_str <- path %>% load_inputtables_xlsx()

# === === === === === === === === === === === === === === === === === === ===
# Function that we develop:   === === === === === === === === === === === ===
# === === === === === === === === === === === === === === === === === === ===

local_inp_str_fn <- gen_inp_str_front(inp_table_str = inp_table_str)
# inp_ui_str <- local_inp_str_fn(inp_raw_str, n_choices = 2, ns = NS(NULL))
#
local_tab_ui_fn <- gen_tabinp_ui_front(inp_tab_str, inp_table_str)

all_outs <-
  local_inp_str_fn(inp_raw_str, n_choices = 3, ns = NS(NULL)) %>%
  local_tab_ui_fn()

# gen_one_inp_table(inp_ui_str, inp_table_str[[2]])

ui <- fluidPage(
  all_outs$tabs %>% slice(1) %>% pull(tab_ui)
)

server <- function(input, output, session) {

}


shinyApp(ui, server)


# # === === === === === === === === === === === === === === === === === === ===
# # Manual testing if it works  === === === === === === === === === === === ===
# # === === === === === === === === === === === === === === === === === === ===
#
# local_inp_str_fn <- gen_inp_str_front(inp_table_str = inp_table_str)
# local_tab_ui_fn <- gen_tabinp_ui_front(inp_tab_str, inp_table_str)
#
# all_outs <-
#   local_inp_str_fn(inp_raw_str, n_choices = 5, ns = NS(NULL)) %>%
#   local_tab_ui_fn()
#
# # Running simple static UI part
# ui <- fluidPage(
#   all_outs$tabs %>% slice(1) %>% pull(tab_ui)
# )
#
# server <- function(input, output, session) {}
#
# shinyApp(ui, server)
#
# # gen_inp_str(inp_raw_str, n_choices = 1, ns = NS(NULL))
# # test_genui_fn(inp_raw_str,
# #               gen_inp_str = gen_inp_str,
# #               gen_ui_fn = local_tab_ui_fn,
# #               full = T)
#
#
# # === === === === === === === === === === === === === === === === === === ===
# # Running reloading the app:  === === === === === === === === === === === ===
# # === === === === === === === === === === === === === === === === === === ===
# #
# # server <- function(input, output, session) {
# #   mod_dyn_inp_srv(
# #     NULL,
# #     inp_raw_str = inp_raw_str,
# #     n_choices = reactive(2),
# #     n_max_choices = reactive(3),
# #     upd_inp = reactive(NULL),
# #     reseter = reactive(NULL),
# #     inp_str_fn = gen_inp_str,
# #     ui_gen_fn = local_tab_ui_fn
# #   )
# # }
# #
# #
# #
# # fluidPage(column(2, wellPanel(mod_inp_switches_ui(NULL))),
# #           column(10, mod_dyn_inp_ui(NULL))) %>%
# #   shinyApp(., server)

