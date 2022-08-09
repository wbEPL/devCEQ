# Running apps UI ------------------------------------------------------------

# reload
golem::detach_all_attached()
golem::document_and_reload()
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
# fs::dir_ls("R") %>% map(source)



#
# Setups ---------------------------------------------------------------------
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


# UI template
path <- "data-raw/ben-inputs-structure.xlsx"
inp_raw_str <- path %>% load_input_xlsx()


## === === === === === === === === === === === === === === === === === === ===
## Testing the UI generation ==================================
## === === === === === === === === === === === === === === === === === === ===

# testrun_input_ui_generation(path)
# testrun_input_ui_page(path)

## Key generator functions
gen_inp_str(inp_raw_str, 2) %>%
  gen_inp_ui(type = "fluid") %>%
  str(max.level = 1)


## === === === === === === === === === === === === === === === === === === ===
## Only input UI in the test mode ==================================
## === === === === === === === === === === === === === === === === === === ===

# options(ceq_dev = TRUE)
# options(scipen = 16)
# options(shiny.reactlog = TRUE)
# options(shiny.fullstacktrace = TRUE)
#
# server <- function(input, output, session) {
#   mod_dyn_inp_srv(
#     NULL,
#     inp_raw_str,
#     inp_str_fn = gen_inp_str,
#     ui_gen_fn = gen_inp_ui)
# }
#
# fluidPage(
#   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#   column(2, wellPanel(mod_inputs_btns_ui(NULL))),
#   column(10, mod_dyn_inp_ui(NULL))
# ) %>%
#   shinyApp(., server)


## === === === === === === === === === === === === === === === === === === ===
## Key launch of the inputs module all in one ==================================
## === === === === === === === === === === === === === === === === === === ===

# Full module on inputs with UI and server logic in buckets.
options(ceq_dev = TRUE)
options(scipen = 16)
options(shiny.reactlog=TRUE)
options(shiny.fullstacktrace = TRUE)

server <- function(input, output, session) {
  run_inputs <-
    mod_inputs_server(
      NULL,
      inp_raw_str,
      inp_str_fn = gen_inp_str,
      ui_gen_fn = gen_inp_ui
      )

  observe({
    list(
      key = run_inputs$key(),
      run = run_inputs$run()
    ) #%>%
      # readr::write_rds("data-raw/input-module-output.rds", compress = "gz")
    # run_inputs$run_guide
    # browser()
  })
}

fluidPage(
  # header = list(cicerone::use_cicerone()),
  shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
  mod_inputs_ui_wrapper(NULL)
) %>%
  shinyApp(., server)



# # # Testing inputs UI 1. --------------------------------------------------------------

# out_ui <-
#   gen_inp_str(inp_raw_str, 2) %>%
#   gen_inp_ui(type = "fluid")
# server <- function(input, output, session) {}
# fluidPage(column(2, wellPanel(numericInput("111", "exmpl", 1))),
#           column(10, out_ui$ui)) %>%
#   shinyApp(., server)
#
# ## Profiling
# # profvis::profvis({gen_inp_str(inp_raw_str, 4) %>% gen_inp_ui() })
# # test_opt <- gen_inp_str(inp_raw_str, 4)
# # profvis::profvis({ gen_inp_str(inp_raw_str, 4) })
# # profvis::profvis({ gen_inp_ui(test_opt) })

# Wrapping UI into a self-generated module -----------------------------------

options(ceq_dev = TRUE)
server <- function(input, output, session) {
  n_ch <- reactive(input$n_choices_inp)
  mod_build_inp_srv(NULL,
                    inp_raw_str,
                    inp_str_fn = gen_inp_str,
                    ui_gen_fn = gen_inp_ui)
}

fluidPage(column(2, wellPanel(numericInput("n_choices_inp", "exmpl", 1))),
          column(10, mod_dyn_inp_ui(NULL)),
          use_bs_popover()
          ) %>%
  shinyApp(., server)

# #  # Same but with modules names
# options(ceq_dev = TRUE)
#
# server <- function(input, output, session) {
#   n_ch <- reactive(input$n_choices_inp)
#   mod_build_inp_srv("nod_1", inp_raw_str,
#                     inp_str_fn = gen_inp_str,
#                     ui_gen_fn = gen_inp_ui,
#                     n_choices = n_ch)
# }
#
# fluidPage(column(2, wellPanel(numericInput("n_choices_inp", "exmpl", 1))),
#           column(10, mod_dyn_inp_ui("nod_1"))
#           ) %>%
#   shinyApp(., server)




# UI self-generation plus collecting all inputs ---------------------------

# # Option without modules names
# options(ceq_dev = TRUE)
# server <- function(input, output, session) {
#   n_ch <- reactive(input$n_choices_inp)
#   inp_raw_str <- indonesia.ceq.app.v2::inp_str_test_dta
#   inp_str <- mod_build_inp_srv(NULL,
#                                inp_raw_str,
#                                inp_str_fn = gen_inp_str,
#                                ui_gen_fn = gen_inp_ui)
#   mod_reset_scenarios(NULL, inp_str = inp_str)
#   current_inp <- mod_coll_inp_srv(NULL, inp_str = inp_str)
#   output$test_out <- renderPrint({str(current_inp())})
# }
#
# fluidPage(column(2, wellPanel(numericInput("n_choices_inp", "exmpl", 1))),
#           column(10, mod_dyn_inp_ui(NULL), shiny::verbatimTextOutput("test_out"))
# ) %>%
#   shinyApp(., server)
#
# # mod_check_inp_srv


# Adding check / collect inputs from ui module -------------------------------------------

# # Option with modules names
# options(scipen = 16)
# options(ceq_dev = FALSE)
# server <- function(input, output, session) {
#   n_ch <- reactive(input$n_choices_inp)
#   inp_str <- mod_build_inp_srv("NULL", inp_raw_str,
#                                inp_str_fn = gen_inp_str,
#                                ui_gen_fn = gen_inp_ui)
#   current_inp <- mod_coll_inp_srv("NULL", inp_str = inp_str)
#   mod_check_inp_srv("NULL", current_inp)
#   mod_reset_scenarios("NULL", inp_str = inp_str)
#   output$test_out <- renderPrint({glimpse(current_inp()$inp)})
# }
#
# fluidPage(
#   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#   column(2, wellPanel(numericInput("n_choices_inp", "exmpl", 1))),
#   column(10, mod_dyn_inp_ui("NULL"), shiny::verbatimTextOutput("test_out"))
# ) %>%
#   shinyApp(., server)



# TO DO: ----------------------------------------------------------------------------

# ---- 1. Logic for saving last valid input and recovering it
# ---- 3. Load previous values when extra policy scenario is added.
# 4. Integrate with the buttons UI.
# 2. Update all inputs according to a template.
# 3. Reset all input to default.
# 7. Download all input in a file.
# ---- 5. Implement shinyFeedback::hideFeedback() on exit.
# 6. Add info tooltips to each input
# 7.




# # Adding internal validation of inputs -----------------------------------------

# options(ceq_dev = TRUE)
# options(shiny.fullstacktrace = TRUE)
#
# # Local server function
# server <- function(input, output, session) {
#
#   n_ch <- reactive(input$n_choices_inp)
#   cur_clean_inp <- reactiveValues(inp = NULL)
#
#   ## ## ## Gen inputs UI
#   cur_clean_inp$inp_str <- mod_build_inp_srv(
#     NULL,
#     inp_raw_str,
#     inp_str_fn = gen_inp_str,
#     ui_gen_fn = gen_inp_ui,
#     n_choices = n_ch
#   )
#
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   ## ## ## reset scenarios to the baseline after scenario-specific button click
#   mod_reset_scenarios(NULL, cur_clean_inp$inp_str)
#
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   # ## ## ## Collecting and validating inputs
#   cur_unvalidated_inp <- mod_coll_inp_srv(NULL, cur_clean_inp$inp_str)
#   cur_clean_inp$inp <- mod_check_inp_srv(NULL, cur_unvalidated_inp)
#
#   # In case we need to by-pass the `mod_check_inp_srv`
#   # cur_clean_inp$inp <- mod_coll_inp_srv(NULL, cur_clean_inp$inp_str)
#
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   ## ## ## Update new inputs UI with previous values (if n changes)
#   mod_upd_old_vals_to_exist_inp(NULL, cur_clean_inp)
#
#
#   observeEvent(input$browser,{
#     browser()
#   })
#
#   output$inputs_out <- renderPrint({str(cur_clean_inp$inp())})
# }
#
#
# fluidPage(
#   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#   column(2, wellPanel(numericInput("n_choices_inp", "exmpl", 1),
#                       if(getOption("ceq_dev", FALSE)) {actionButton("browser", "browser")}
#   )),
#   column(10, mod_dyn_inp_ui(NULL), shiny::verbatimTextOutput("inputs_out"))
# ) %>%
#   shinyApp(., server)






# # Adding update inputs based on previous values -----------------------------------------
# # Logic necessary for new inputs to not overwrite old values

# options(ceq_dev = TRUE)
# options(shiny.fullstacktrace = TRUE)
#
# # Local server function
# server <- function(input, output, session) {
#
#   n_ch <- reactive(input$n_choices_inp)
#   cur_clean_inp <- reactiveValues(inp = NULL)
#
#   ## ## ## Gen inputs UI
#   cur_clean_inp$inp_str <- mod_build_inp_srv(
#     NULL,
#     inp_raw_str,
#     inp_str_fn = gen_inp_str,
#     ui_gen_fn = gen_inp_ui,
#     n_choices = n_ch
#   )
#
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   # ## ## ## Collecting and validating inputs
#   cur_unvalidated_inp <- mod_coll_inp_srv(NULL, cur_clean_inp$inp_str)
#   cur_clean_inp$inp <- mod_check_inp_srv(NULL, cur_unvalidated_inp)
#
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   ## ## ## Update new inputs UI with previous values (if n changes)
#   mod_upd_old_vals_to_exist_inp(NULL, cur_clean_inp)
#
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   ## ## ## reset scenarios to the baseline after scenario-specific button click
#   mod_reset_scenarios(NULL, cur_clean_inp$inp_str)
#
#   observeEvent(input$browser,{
#     browser()
#   })
#
#   output$inputs_out <- renderPrint({str(cur_clean_inp$inp())})
# }
#
#
# fluidPage(
#   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#   column(2, wellPanel(numericInput("n_choices_inp", "exmpl", 1),
#                       if(getOption("ceq_dev", FALSE)) {actionButton("browser", "browser")}
#   )),
#   column(10, mod_dyn_inp_ui(NULL), shiny::verbatimTextOutput("inputs_out"))
# ) %>%
#   shinyApp(., server)




# # Adding export of inputs ------------------------------------------

# # # # Option with modules names
# options(ceq_dev = TRUE)
# options(ceq_inmodule_dev = FALSE)
# options(scipen = 16)
#
# server <- function(input, output, session) {
#
#   # Reactive values with inputs
#   cur_clean_inp <- reactiveValues(inp = NULL)
#   cur_clean_inp$n_ch <- reactive(input$n_choices_inp)
#
#   ## ## ## Gen inputs UI
#   cur_clean_inp$inp_str <- mod_build_inp_srv(
#     NULL,
#     inp_raw_str,
#     inp_str_fn = gen_inp_str,
#     ui_gen_fn = gen_inp_ui,
#     n_choices = cur_clean_inp$n_ch
#   )
#
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   ## ## ## Update new inputs UI with previous values (if n changes)
#   mod_upd_old_vals_to_exist_inp(NULL, cur_clean_inp)
#
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   ## ## ## reset scenarios to the baseline after scenario-specific button click
#   mod_reset_scenarios(NULL, cur_clean_inp$inp_str)
#
#
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   # ## ## ## Collecting and validating inputs
#   cur_unvalidated_inp <- mod_coll_inp_srv(NULL, cur_clean_inp$inp_str)
#   cur_clean_inp$inp <- mod_check_inp_srv(NULL, cur_unvalidated_inp)
#
#   clean_inp_values <- reactive(cur_clean_inp$inp()$inp)
#   ## ## ## Exportable inputs table
#   cur_clean_inp$export <- mod_export_inp_srv(NULL, clean_inp_values)
#
#   ## ## ## Extracting key inputs
#   cur_clean_inp$key <- mod_key_inp_srv(NULL, clean_inp_values)
#
#   ## ## ## Rendering the data table
#   output$inputs_ui_values <-
#     DT::renderDT({
#       req(cur_clean_inp$export()) %>%
#         fct_config_gen_dt("Policy scenarios summary")
#     },
#     server = FALSE)
#
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   # ## ## ## Testing
#   ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
#   observeEvent(input$browser,{
#     browser()
#
#   })
#
#   output$inputs_out <- renderPrint({
#     cur_clean_inp$key() %>%
#       str()
#   })
# }
#
#
# fluidPage(
#   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#   column(2, wellPanel(numericInput("n_choices_inp", "exmpl", 1),
#                       if(getOption("ceq_dev", FALSE)) {actionButton("browser", "browser")}
#                       )),
#   column(10,
#          mod_dyn_inp_ui(NULL),
#          hr(),
#          shiny::verbatimTextOutput("inputs_out")
#          )
# ) %>%
#   shinyApp(., server)


# Wrapping all around one function ----------------------------------------

# # # Option with modules names
# options(ceq_dev = TRUE)
# options(scipen = 16)
#
# server <- function(input, output, session) {
#   mod_dyn_inp_srv(NULL, inp_raw_str, inp_str_fn = gen_inp_str, ui_gen_fn = gen_inp_ui)
# }
#
# fluidPage(
#   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#   column(2, wellPanel(h3("Numbers placeholder"))),
#   column(10, mod_dyn_inp_ui(NULL))
# ) %>%
#   shinyApp(., server)


# Wrapping all around one function but with the pronounced namespace --------

# # Option with modules names
# options(ceq_dev = TRUE)
# options(scipen = 16)
#
# server <- function(input, output, session) {
#   mod_dyn_inp_srv("Nodule1", inp_raw_str, inp_str_fn = gen_inp_str, ui_gen_fn = gen_inp_ui)
# }
#
# fluidPage(
#   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#   column(2, wellPanel(h3("Numbers placeholder"))),
#   column(10, mod_dyn_inp_ui("Nodule1"))
# ) %>%
#   shinyApp(., server)


# Module wrapper for all inputs with the valid side panel ===============

# # Not connected to the main logic
# options(ceq_dev = TRUE)
# options(scipen = 16)
# options(shiny.reactlog=TRUE)
# options(shiny.fullstacktrace = TRUE)
#
# server <- function(input, output, session) {
#   mod_dyn_inp_srv("NULL", inp_raw_str, inp_str_fn = gen_inp_str, ui_gen_fn = gen_inp_ui)
# }
#
# fluidPage(
#   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#   column(2, wellPanel(mod_inputs_btns_ui(NULL))),
#   column(10, mod_dyn_inp_ui("NULL"))
# ) %>%
#   shinyApp(., server)


# # Full module on inputs with UI and server logic in buckets.
# options(ceq_dev = TRUE)
# options(scipen = 16)
#
# options(shiny.reactlog=TRUE)
# options(shiny.fullstacktrace = TRUE)
#
# server <- function(input, output, session) {
#   run_inputs <-
#     mod_inputs_server(NULL,
#                       inp_raw_str,
#                       inp_str_fn = gen_inp_str,
#                       ui_gen_fn = gen_inp_ui)
#
#
#   observe({
#     list(
#       key = run_inputs$key(),
#       run = run_inputs$run()
#     ) #%>%
#       # readr::write_rds("data-raw/input-module-output.rds", compress = "gz")
#     # run_inputs$run_guide
#     # browser()
#   })
# }
#
# fluidPage(
#   # header = list(cicerone::use_cicerone()),
#   shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
#   mod_inputs_ui_wrapper(NULL)
# ) %>%
#   shinyApp(., server)




# Current clean input check ------------------------------------------------
# # We make sure here that
library(readr)
curr_inp <-
  read_rds("data-raw/cur_clean_inp.rds")

curr_inp4 <-
  read_rds("data-raw/cur_clean_inp_4policy.rds")

curr_inp %>% fct_prep_key_inp()

curr_inp4 %>% fct_prep_key_inp() %>% map("policy_as_base")
#
#
#
#
#
# curr_inp4 %>%
#   filter(policy_choice  == "policy2") %>%
#   fct_base_key_inp
#   fct_compare_key_inp(., "policy0")
#
#
#   filter(type != "textInput") %>%
#   select(id, current_value, base_value) %>%
#   mutate(current_value = as.numeric(current_value))
# policy_choices = setNames(for_list$current_value, for_list$id)
# policy_base_choices = setNames(for_list$base_value , for_list$id)


