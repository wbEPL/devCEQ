# Space for testing the inputs tabs

golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

library(tidyverse)
library(shiny)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### Building the UI for the tabs switches and updates
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# # Helpers for testin input UI -----------------------------------------------
# pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
# get_test_tab_switches()
# get_test_tabs()
# get_test_inp_tab_str_raw()
# fct_inp_make_dummy_tabs(get_test_tab_switches())

# Rendering a simple static page ----------------------------------------------
# pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
# test_mod_inp_tabs_simple()

# Adding horizontal line class of the text. ---------------------------------
# pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
# sw2 <- get_test_tab_switches()
# names(sw2$choices)[4] <- as.character(hr(class = "hr-small-line"))
# test_mod_inp_tabs_simple(id = NULL, switches = sw2)

# Testing for different layouts -----------------------------------------------
pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)

# Test one
"data-raw/ceq-inputs-idn-2022.xlsx" %>%
  load_inputtabs_xlsx() %>%
  fct_inp_tab_order() %>%
  fct_inp_tab_str() %>%
  test_mod_inp_tabs_simple(id = NULL, switches = .)

# Test Two
"data-raw/ben-inputs-structure.xlsx" %>%
  load_inputtabs_xlsx() %>%
  fct_inp_tab_order() %>%
  fct_inp_tab_str() %>%
  test_mod_inp_tabs_simple(id = NULL, switches = .)

# Test thre
"data-raw/simple-inputs-structure.xlsx" %>%
  load_inputtabs_xlsx() %>%
  fct_inp_tab_order() %>%
  fct_inp_tab_str() %>%
  test_mod_inp_tabs_simple(id = NULL, switches = .)

# Test four
get_test_inp_tab_str_raw() %>%
  fct_inp_tab_order() %>%
  fct_inp_tab_str() %>%
  test_mod_inp_tabs_simple(id = NULL, switches = .)

# Test four
get_test_inp_tab_str_raw(3) %>%
  fct_inp_tab_order() %>%
  fct_inp_tab_str() %>%
  test_mod_inp_tabs_simple(id = NULL, switches = .)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### Integrating tabs switches to the full page UI
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

test_app_ui <- function(inp_str_path) {

}

#
# ### ### CONTINUE HERE!!!! ### ### ### ### ### ## ###
#
#
# pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
#
# inp_tab_str <- get_test_inp_tab_str_raw()
# inp_tab_str_ordered <- inp_tab_str %>% fct_inp_tab_order()
#
#
# # browser()
# out_wells_tabs <-
#   inp_groups %>%
#   left_join(inp_tab_str_ordered, "group_order") %>%
#   tidyr::replace_na(list(tab_name = "Other Policy Choices",
#                          tab_order = 9999,
#                          tab_id = "panel9999")) %>%
#   arrange(tab_order, group_order) %>%
#   group_by(tab_order, tab_name, tab_id) %>%
#   summarise(single_well = tagList(single_well)) %>%
#   ungroup()
#
# out_tabs <-
#   out_wells_tabs %>%
#   mutate(#
#     tab_ui =
#       purrr::pmap(., ~ {
#         dts <- rlang::dots_list(...)
#         dts$single_well %>%
#           shiny::column(., width = sum(input_cols_spec$width)) %>%
#           rowwing_fn(style = scroll_panel_style,
#                      id = ns("policy-options-inputs")) %>%
#           shiny::tabPanelBody(value = paste0("panel", dts[[2]]), .)
#       }))
#
#
#
# dta <- get_test_inp_tab_order()
#
#
# pkgload::load_all(export_all = TRUE, helpers = FALSE, attach_testthat = FALSE)
# test_mod_inp_tabs_simple()
#
#
#
#
# # # # Simple concept
# # # library(shiny)
# # # library(tidyverse)
# # # ui <- fluidPage(
# # #   column(
# # #     4,
# # #     h2("Inputs"),
# # #     radioButtons("controller", "Controller", 1:3, 1)
# # #     ),
# # #   column(
# # #     8,
# # #     h2("Tabs"),
# # #     tabsetPanel(
# # #       id = "hidden_tabs",
# # #       type = "pills",
# # #       tabPanelBody("panel1", "Panel 1 content"),
# # #       tabPanelBody("panel2", "Panel 2 content"),
# # #       tabPanelBody("panel3", "Panel 3 content")
# # #     )
# # #   )
# # # )
# # #
# # # server <- function(input, output, session) {
# # #   observeEvent(input$controller, {
# # #     updateTabsetPanel(session, "hidden_tabs", selected = paste0("panel", input$controller))
# # #   })
# # # }
# # #
# # # if (interactive()) {
# # #   shinyApp(ui, server)
# # # }
# # #
# #
# # # # Switches layout test ---------------------------------------------------------
# # # ui <- fluidPage(
# # #   column(
# # #     4,
# # #     h2("Inputs"),
# # #     shinyWidgets::radioGroupButtons(
# # #       inputId = "controller",
# # #       label = "Label",
# # #       choices = c("panel1", "panel2", "Summary"),
# # #       direction = "vertical",
# # #       justified = TRUE
# # #     )
# # #     ),
# # #   column(
# # #     8,
# # #     h2("Tabs"),
# # #     tabsetPanel(
# # #       id = "hidden_tabs",
# # #       type = "hidden",
# # #       tabPanelBody("panel1", "Panel 1 content"),
# # #       tabPanelBody("panel2", "Panel 2 content"),
# # #       tabPanelBody("Summary", "Summary Panel")
# # #     )
# # #   )
# # # )
# # #
# # # server <- function(input, output, session) {
# # #   observeEvent(input$controller, {
# # #     updateTabsetPanel(session, "hidden_tabs", selected = paste0(input$controller))
# # #   })
# # # }
# # #
# # # if (interactive()) {
# # #   shinyApp(ui, server)
# # # }
# #
# # # # Test 1. if inputs generate UI ----------------------------------------------
# # #
# # # golem::detach_all_attached()
# # # golem::document_and_reload()
# # # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# # #
# # # library(shiny)
# # # library(tidyverse)
# # #
# # # path <- "data-raw/complex-inputs-structure.xlsx"
# # # inp_raw_str <- path %>% load_input_xlsx()
# # #
# # # inp_str <- inp_str_test(inp_raw_str)
# # # # profvis::profvis(inp_str <- inp_str_test(inp_raw_str))
# #
# #
# # # Test 2. If simple app launches ----------------------------------------------
# #
# # # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# # #
# # # path <- "data-raw/complex-inputs-structure.xlsx"
# # # inp_raw_str <- path %>% load_input_xlsx()
# # #
# # # server <- function(input, output, session) {
# # #   mod_inputs_server(
# # #     "test",
# # #     inp_raw_str = inp_raw_str,
# # #     inp_str_fn = gen_inp_str,
# # #     ui_gen_fn = gen_inp_ui
# # #   )
# # #   # callModule(profvis::profvis_server, "prof1")
# # # }
# # #
# # # navbarPage(
# # #   id = "main_sidebar",
# # #   title = "Navbar title",
# # #   theme =  bslib::bs_theme(version = 4, bootswatch = "flatly", "enable-rounded" = TRUE),
# # #   windowTitle = "windowTitle",
# # #   collapsible = TRUE,
# # #   tabPanel(
# # #     title = "Policy",
# # #     # shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
# # #     mod_inputs_ui_wrapper("test"),
# # #     profvis::profvis_ui(id = "prof1")
# # #   )
# # # ) %>%
# # #   shinyApp(., server)
# #
# # # # Writing a shiny app for testing the layout funcitons -------------------------
# # #
# # # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# # #
# # # path <- "data-raw/complex-inputs-structure.xlsx"
# # # inp_raw_str <- path %>% load_input_xlsx()
# # # inp_tab_str <- path %>% load_inputtabs_xlsx()
# # #
# # #
# # # # # Generate UI Simple manual process
# # # # ui_parts <-
# # # #   gen_inp_str(inp_raw_str, 2) %>%
# # # #   gen_inp_ui(type = "fluid")
# # # #
# # # # ui_parts %>% str(max.level = 1)
# # # #
# # # # server <- function(input, output, session) {
# # # #   mod_dyn_inp_srv(
# # # #     NULL,
# # # #     inp_raw_str,
# # # #     inp_str_fn = gen_inp_str,
# # # #     ui_gen_fn = gen_inp_ui)
# # # # }
# # # #
# # # # fluidPage(
# # # #   column(2, wellPanel(ui_parts$switches$ui)),
# # # #   column(10, ui_parts$tabs$ui)
# # # # ) %>%
# # # #   shinyApp(., server)
# #
# #
# # # The testing function ---------------------------------------------------------
# #
# # library(tidyverse)
# # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# #
# # path <- "data-raw/complex-inputs-structure.xlsx"
# # inp_raw_str <- path %>% load_input_xlsx()
# # inp_tab_str <- path %>% load_inputtabs_xlsx()
# #
# # test_genui_fn(inp_raw_str, full = T)
# #
# # # library(reactlog)
# # # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# # # reactlog_enable()
# # # test_genui_fn(inp_raw_str)
# # # shiny::reactlogShow()
# #
# #
# # # Test 3. Loading tabs structure from input file ----------------------------------------------
# #
# # # library(shiny)
# # # library(tidyverse)
# # #
# # # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# # #
# # # path <- "data-raw/complex-inputs-structure.xlsx"
# # # inp_raw_str <- path %>% load_input_xlsx()
# # # inp_tab_str <- path %>% load_inputtabs_xlsx()
# # #
# # # test_ui <- function(ui) {
# # #   shinyApp(fluidPage(ui), function(input, output, session) {})
# # # }
# # #
# # #
# # #
# # # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# # #
# # # # Deveoping and testing underlining function.
# # # # options(shiny.launch.browser = .rs.invokeShinyPaneViewer)
# # # # options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
# # # options(shiny.launch.browser = .rs.invokeShinyWindowExternal)
# # #
# # # gen_ui <- gen_tabinp_ui_front()
# # # ui_parts <-
# # #   gen_inp_str(inp_raw_str, 2) %>%
# # #   gen_ui(.)
# # #
# # # # Attepmt to test UI
# # # test_ui(ui_parts$tab_header$ui)
# # # test_ui(ui_parts$switches$ui)
# # # # test_ui(ui_parts$tabs$ui)
# #
# #
# # # # Test 4. Testing full and the data table =======================================
# # #
# # # library(shiny)
# # # library(tidyverse)
# # #
# # # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# # #
# # # path <- "data-raw/complex-inputs-structure.xlsx"
# # # inp_raw_str <- path %>% load_input_xlsx()
# # # inp_tab_str <- path %>% load_inputtabs_xlsx()
# # #
# # #
# # # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# # #
# # # local_tab_ui_fn <- gen_tabinp_ui_front(inp_tab_str)
# # #
# # # test_genui_fn(inp_raw_str,
# # #               gen_ui_fn = gen_tabinp_ui_front(inp_tab_str),
# # #               full = TRUE)
# #
# # # # Developing tables in inputs =================================================
# #
# #
# # golem::detach_all_attached()
# # golem::document_and_reload()
# #
# # library(shiny)
# # library(tidyverse)
# #
# # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# #
# # path <- "data-raw/complex-inputs-structure.xlsx"
# # inp_raw_str <- path %>% load_input_xlsx()
# # inp_tab_str <- path %>% load_inputtabs_xlsx()
# # inp_table_str <- path %>% load_inputtables_xlsx()
# #
# # # load_inputtables_xlsx()
# #
# #
# # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# #
# # local_inp_str_fn <- gen_inp_str_front(inp_table_str)
# # local_tab_ui_fn <- gen_tabinp_ui_front(
# #   bind_rows(
# #     tibble(tab_name = "dta"),
# #     inp_tab_str
# #     ),
# #   inp_table_str)
# #
# # all_outs <-
# #   local_inp_str_fn(inp_raw_str, n_choices = 5, ns = NS(NULL)) %>%
# #   local_tab_ui_fn()
# #
# # # gen_inp_str(inp_raw_str, n_choices = 1, ns = NS(NULL))
# #
# #
# # test_genui_fn(inp_raw_str,
# #               gen_inp_str = gen_inp_str,
# #               gen_ui_fn = local_tab_ui_fn,
# #               full = T)
# #
# # # debug(gen_inp_str)
# # #
# # # gen_inp_str(inp_raw_str = inp_raw_str,
# # #             n_choices = n_poly(),
# # #             ns = ns)
# #
# # #
# # #
# # #
# # #
# # # server <- function(input, output, session) {
# # #   mod_inputs_server(
# # #     "test",
# # #     inp_raw_str = inp_raw_str,
# # #     inp_str_fn = gen_inp_str,
# # #     ui_gen_fn = gen_tabinp_ui_front(inp_tab_str = inp_tab_str)
# # #   )
# # #   # callModule(profvis::profvis_server, "prof1")
# # # }
# # #
# # #
# # # options(shiny.fullstacktrace = TRUE)
# # # options(shiny.reactlog=TRUE)
# # # navbarPage(
# # #   id = "main_sidebar",
# # #   title = "Navbar title",
# # #   theme =  bslib::bs_theme(version = 4, bootswatch = "flatly", "enable-rounded" = TRUE),
# # #   windowTitle = "windowTitle",
# # #   collapsible = TRUE,
# # #   tabPanel(
# # #     title = "Policy",
# # #     # shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
# # #     mod_inputs_ui_wrapper("test"),
# # #     profvis::profvis_ui(id = "prof1")
# # #   )
# # # ) %>%
# # #   shinyApp(., server)
# #
# #
# # # Developing tabs and wells grouping ------------------------------------
# #
# # pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
# #
# # path <- "data-raw/complex-inputs-structure.xlsx"
# # inp_raw_str <- path %>% load_input_xlsx()
# # inp_str <- inp_str_test(inp_raw_str)
# # # inp_str$`1`$all_structures
# #
# # server <- function(input, output, session) {
# #   # mod_dyn_inp_srv("NULL", inp_raw_str, inp_str_fn = gen_inp_str, ui_gen_fn = gen_inp_ui)
# #   observe({
# #     updateTabsetPanel(session, "input_tabs", selected = paste0(input$selected_input_tab))
# #   })
# # }
# #
# # fluidPage(
# #   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
# #   column(
# #     2,
# #     wellPanel(mod_inputs_btns_ui(NULL),
# #               inp_str$`1`$all_uis$switches$ui)
# #   ),
# #   column(10,
# #          inp_str$`1`$all_uis$tabs$ui)
# # ) %>%
# #   shinyApp(., server)
# #
# #
# # ## ### ### ###
# # # Running input module alone ----------------------------------------------
# #
# #
# #
# #
# #
# #
# #
# #
# # # Developing complexity --------------------------------------------------
# #
# # library(readxl)
# # library(dplyr)
# # library(purrr)
# # library(tidyr)
# # library(tidyselect)
# # library(stringr)
# # library(magrittr)
# # library(shiny)
# # library(fs)
# # library(bsplus)
# # library(tippy)
# # library(htmltools)
# #
# # pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
# #
# # # UI template
# # path <- "data-raw/simple-inputs-structure.xlsx"
# # inp_raw_str <- path %>% load_input_xlsx()
# #
# # pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
# #
# # server <- function(input, output, session) {
# #   mod_dyn_inp_srv("Nodule1", inp_raw_str, inp_str_fn = gen_inp_str, ui_gen_fn = gen_inp_ui)
# # }
# #
# # fluidPage(
# #   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
# #   column(2, wellPanel(h3("Numbers placeholder"))),
# #   column(10, mod_dyn_inp_ui("Nodule1"))
# # ) %>%
# #   shinyApp(., server)
