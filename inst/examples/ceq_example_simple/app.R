# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Loading all functions in this app-folder
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)

# Disabling all development parameters 
options(scipen = 16)
options(ceq_dev = FALSE)
options(ceq_run_dev = FALSE)
options(ceq_results_dev = FALSE)
options(golem.app.prod = TRUE)

# Disabling debugging
options(shiny.fullstacktrace = FALSE)
options(shiny.error = NULL)

# Loading packages

# Un-comment the row below and install the package 'devCEQ' if it is not installed
# You may need to re-install the package if it gets updated.

# remotes::install_github("wbEPL/devCEQ", force = TRUE, dependencies = TRUE)
library(devCEQ)
library(dplyr)
library(purrr)
library(shiny)
library(stringr)
library(tidyr)

# Loading input structure from the excel file
inputs_path <- "./data-app/simple-inputs-structure.xlsx"
inputs_str <- load_input_xlsx(inputs_path)
inputs_tab_str <- load_inputtabs_xlsx(inputs_path )
inputs_table_str <- load_inputtables_xlsx(inputs_path) 

# Loading pre-simulation and baseline data into the app
presim_dta <- reactive({
  presim <- list()
  # presim = readr::read_rds("./data-app/presim.rds")
  presim$baseline = NULL # readr::read_rds("./data-app/baseline.rds")
  presim
})

# Function for generating inputs UI
# 1. We provide function for generating inputs that are in tables without labels
local_inp_str_fn <- gen_inp_str_front(inp_table_str = inputs_table_str)

# 2. We provide function that creates UI based on all inputs.
local_tab_ui_fn <- gen_tabinp_ui_front(inp_tab_str = inputs_tab_str, 
                                       inp_table_str = inputs_table_str)

# 3. Complete UI wrapper
local_ceq_ui <- devCEQ::gen_ceq_ui(inp_nav_width = 3,
                                   fn_results_ui =  mod_results_ui)

# # 4. Server side logic
local_run_sim_server <-
  devCEQ::make_run_sim_server(fn_ceq_sim = full_ceq)

# App parameters ...
options(current.app.name = "Simple CEQ app example")

# App running function
devCEQ::CEQ_run(
  
  # Key data
  inputs_str = inputs_str,
  presim = presim_dta,
  
  # info page
  info_page_md = "inst/app/info-page.md",
  info_page_size = "m",
  
  # Key policy number options
  n_policy = c(1, 3, 2),
  n_policy_type = "dropdown", # Use devCEQ::get_n_policy_types() to see the options.
  
  # key functions
  ui_fn = local_ceq_ui, # User interface part
  
  inp_str_fn = local_inp_str_fn, # Inputs structuring
  ui_gen_fn = local_tab_ui_fn,   # Inputs UI 
  
  # Server simulation function 
  fn_sim_srvr = local_run_sim_server, # Server-side logic
  
  # server side results:
  fn_res_disp_srvr = mod_results_server # Results side logic
)
