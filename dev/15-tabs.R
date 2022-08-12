

# # Simple concept
# library(shiny)
# library(tidyverse)
# ui <- fluidPage(
#   column(
#     4,
#     h2("Inputs"),
#     radioButtons("controller", "Controller", 1:3, 1)
#     ),
#   column(
#     8,
#     h2("Tabs"),
#     tabsetPanel(
#       id = "hidden_tabs",
#       type = "pills",
#       tabPanelBody("panel1", "Panel 1 content"),
#       tabPanelBody("panel2", "Panel 2 content"),
#       tabPanelBody("panel3", "Panel 3 content")
#     )
#   )
# )
#
# server <- function(input, output, session) {
#   observeEvent(input$controller, {
#     updateTabsetPanel(session, "hidden_tabs", selected = paste0("panel", input$controller))
#   })
# }
#
# if (interactive()) {
#   shinyApp(ui, server)
# }
#

# # Switches layout test ---------------------------------------------------------
# ui <- fluidPage(
#   column(
#     4,
#     h2("Inputs"),
#     shinyWidgets::radioGroupButtons(
#       inputId = "controller",
#       label = "Label",
#       choices = c("panel1", "panel2", "Summary"),
#       direction = "vertical",
#       justified = TRUE
#     )
#     ),
#   column(
#     8,
#     h2("Tabs"),
#     tabsetPanel(
#       id = "hidden_tabs",
#       type = "hidden",
#       tabPanelBody("panel1", "Panel 1 content"),
#       tabPanelBody("panel2", "Panel 2 content"),
#       tabPanelBody("Summary", "Summary Panel")
#     )
#   )
# )
#
# server <- function(input, output, session) {
#   observeEvent(input$controller, {
#     updateTabsetPanel(session, "hidden_tabs", selected = paste0(input$controller))
#   })
# }
#
# if (interactive()) {
#   shinyApp(ui, server)
# }

# # Test 1. if inputs generate UI ----------------------------------------------
#
# golem::detach_all_attached()
# golem::document_and_reload()
# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
#
# library(shiny)
# library(tidyverse)
#
# path <- "data-raw/complex-inputs-structure.xlsx"
# inp_raw_str <- path %>% load_input_xlsx()
#
# inp_str <- inp_str_test(inp_raw_str)
# # profvis::profvis(inp_str <- inp_str_test(inp_raw_str))


# Test 2. If simple app launches ----------------------------------------------

# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
#
# path <- "data-raw/complex-inputs-structure.xlsx"
# inp_raw_str <- path %>% load_input_xlsx()
#
# server <- function(input, output, session) {
#   mod_inputs_server(
#     "test",
#     inp_raw_str = inp_raw_str,
#     inp_str_fn = gen_inp_str,
#     ui_gen_fn = gen_inp_ui
#   )
#   # callModule(profvis::profvis_server, "prof1")
# }
#
# navbarPage(
#   id = "main_sidebar",
#   title = "Navbar title",
#   theme =  bslib::bs_theme(version = 4, bootswatch = "flatly", "enable-rounded" = TRUE),
#   windowTitle = "windowTitle",
#   collapsible = TRUE,
#   tabPanel(
#     title = "Policy",
#     # shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#     mod_inputs_ui_wrapper("test"),
#     profvis::profvis_ui(id = "prof1")
#   )
# ) %>%
#   shinyApp(., server)

# # Writing a shiny app for testing the layout funcitons -------------------------
#
# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
#
# path <- "data-raw/complex-inputs-structure.xlsx"
# inp_raw_str <- path %>% load_input_xlsx()
# inp_tab_str <- path %>% load_inputtabs_xlsx()
#
#
# # # Generate UI Simple manual process
# # ui_parts <-
# #   gen_inp_str(inp_raw_str, 2) %>%
# #   gen_inp_ui(type = "fluid")
# #
# # ui_parts %>% str(max.level = 1)
# #
# # server <- function(input, output, session) {
# #   mod_dyn_inp_srv(
# #     NULL,
# #     inp_raw_str,
# #     inp_str_fn = gen_inp_str,
# #     ui_gen_fn = gen_inp_ui)
# # }
# #
# # fluidPage(
# #   column(2, wellPanel(ui_parts$switches$ui)),
# #   column(10, ui_parts$tabs$ui)
# # ) %>%
# #   shinyApp(., server)


# The testing function ---------------------------------------------------------

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
test_genui_fn(inp_raw_str, full = T)

# library(reactlog)
# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# reactlog_enable()
# test_genui_fn(inp_raw_str)
# shiny::reactlogShow()


# Test 3. Loading tabs structure from input file ----------------------------------------------

# library(shiny)
# library(tidyverse)
#
# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
#
# path <- "data-raw/complex-inputs-structure.xlsx"
# inp_raw_str <- path %>% load_input_xlsx()
# inp_tab_str <- path %>% load_inputtabs_xlsx()
#
# test_ui <- function(ui) {
#   shinyApp(fluidPage(ui), function(input, output, session) {})
# }
#
#
#
# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
#
# # Deveoping and testing underlining function.
# # options(shiny.launch.browser = .rs.invokeShinyPaneViewer)
# # options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
# options(shiny.launch.browser = .rs.invokeShinyWindowExternal)
#
# gen_ui <- gen_tabinp_ui_front()
# ui_parts <-
#   gen_inp_str(inp_raw_str, 2) %>%
#   gen_ui(.)
#
# # Attepmt to test UI
# test_ui(ui_parts$tab_header$ui)
# test_ui(ui_parts$switches$ui)
# # test_ui(ui_parts$tabs$ui)


# # Test 4. Testing full and the data table =======================================
#
# library(shiny)
# library(tidyverse)
#
# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
#
# path <- "data-raw/complex-inputs-structure.xlsx"
# inp_raw_str <- path %>% load_input_xlsx()
# inp_tab_str <- path %>% load_inputtabs_xlsx()
#
#
# pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
#
# local_tab_ui_fn <- gen_tabinp_ui_front(inp_tab_str)
#
# test_genui_fn(inp_raw_str,
#               gen_ui_fn = gen_tabinp_ui_front(inp_tab_str),
#               full = TRUE)

# # Developing tables in inputs =================================================


golem::detach_all_attached()
golem::document_and_reload()

library(shiny)
library(tidyverse)

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

path <- "data-raw/complex-inputs-structure.xlsx"
inp_raw_str <- path %>% load_input_xlsx()
inp_tab_str <- path %>% load_inputtabs_xlsx()
inp_table_str <- path %>% load_inputtables_xlsx()

# load_inputtables_xlsx()


pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

local_inp_str_fn <- gen_inp_str_front(inp_table_str)
local_tab_ui_fn <- gen_tabinp_ui_front(inp_tab_str, inp_table_str)

all_outs <-
  local_inp_str_fn(inp_raw_str, n_choices = 5, ns = NS(NULL)) %>%
  local_tab_ui_fn()

# gen_inp_str(inp_raw_str, n_choices = 1, ns = NS(NULL))


test_genui_fn(inp_raw_str,
              gen_inp_str = gen_inp_str,
              gen_ui_fn = local_tab_ui_fn,
              full = T)

# debug(gen_inp_str)
#
# gen_inp_str(inp_raw_str = inp_raw_str,
#             n_choices = n_poly(),
#             ns = ns)

#
#
#
#
# server <- function(input, output, session) {
#   mod_inputs_server(
#     "test",
#     inp_raw_str = inp_raw_str,
#     inp_str_fn = gen_inp_str,
#     ui_gen_fn = gen_tabinp_ui_front(inp_tab_str = inp_tab_str)
#   )
#   # callModule(profvis::profvis_server, "prof1")
# }
#
#
# options(shiny.fullstacktrace = TRUE)
# options(shiny.reactlog=TRUE)
# navbarPage(
#   id = "main_sidebar",
#   title = "Navbar title",
#   theme =  bslib::bs_theme(version = 4, bootswatch = "flatly", "enable-rounded" = TRUE),
#   windowTitle = "windowTitle",
#   collapsible = TRUE,
#   tabPanel(
#     title = "Policy",
#     # shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#     mod_inputs_ui_wrapper("test"),
#     profvis::profvis_ui(id = "prof1")
#   )
# ) %>%
#   shinyApp(., server)


# Developing tabs and wells grouping ------------------------------------

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)

path <- "data-raw/complex-inputs-structure.xlsx"
inp_raw_str <- path %>% load_input_xlsx()
inp_str <- inp_str_test(inp_raw_str)
# inp_str$`1`$all_structures

server <- function(input, output, session) {
  # mod_dyn_inp_srv("NULL", inp_raw_str, inp_str_fn = gen_inp_str, ui_gen_fn = gen_inp_ui)
  observe({
    updateTabsetPanel(session, "input_tabs", selected = paste0(input$selected_input_tab))
  })
}

fluidPage(
  shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
  column(
    2,
    wellPanel(mod_inputs_btns_ui(NULL),
              inp_str$`1`$all_uis$switches$ui)
  ),
  column(10,
         inp_str$`1`$all_uis$tabs$ui)
) %>%
  shinyApp(., server)


## ### ### ###
# Running input module alone ----------------------------------------------








# Developing complexity --------------------------------------------------

library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(tidyselect)
library(stringr)
library(magrittr)
library(shiny)
library(fs)
library(bsplus)
library(tippy)
library(htmltools)

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)

# UI template
path <- "data-raw/simple-inputs-structure.xlsx"
inp_raw_str <- path %>% load_input_xlsx()

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)

server <- function(input, output, session) {
  mod_dyn_inp_srv("Nodule1", inp_raw_str, inp_str_fn = gen_inp_str, ui_gen_fn = gen_inp_ui)
}

fluidPage(
  shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
  column(2, wellPanel(h3("Numbers placeholder"))),
  column(10, mod_dyn_inp_ui("Nodule1"))
) %>%
  shinyApp(., server)
