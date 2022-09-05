options(golem.app.prod = FALSE)
options(shiny.port = httpuv::randomPort())
options(shiny.fullstacktrace = TRUE)
options(shiny.reactlog=TRUE)
# golem::detach_all_attached()
# golem::document_and_reload()

# Dev paras
options(scipen = 16)
options(ceq_dev = FALSE)
options(ceq_run_dev = TRUE)
options(ceq_results_dev = TRUE)

library(shiny)
# remotes::install_github("CEQwb/devCEQ")
devtools::load_all()
# library(devCEQ)

inputs_path <- "./data-raw/ceq-inputs-idn-2022.xlsx"
inputs_raw_str <- inputs_path %>% load_input_xlsx()
inputs_tab_str <- inputs_path %>% load_inputtabs_xlsx()
inputs_table_str <- inputs_path %>% load_inputtables_xlsx()


presim <- reactive({
  out <- list() #read_rds("./data-app/presim-2022.rds")
  out$bl_res <- NULL #read_rds("./data-app/sim-baseline-2022.rds")
  out$bl_tbl <- NULL
  out
})

# Function for generating inputs UI

# 1. We provide function for generating inputs that are in tables without labels
local_inp_str_fn <- gen_inp_str_front(inp_table_str = inputs_table_str)

# 2. We provide function that creates UI based on all inputs.
local_tab_ui_fn <- gen_tabinp_ui_front(
  inputs_tab_str,
  inp_table_str = inputs_table_str
  )

# 3. Complete UI wrapper
local_ceq_ui <- gen_ceq_ui(inp_nav_width = 3)

# Title of the App
options(current.app.name = "CEQ")
options(ceq_results_dev = TRUE)

# Running the CEQ
devCEQ::CEQ_run(
  inputs_str = inputs_raw_str,
  presim = presim,
  ui_fn = local_ceq_ui,
  inp_str_fn = local_inp_str_fn,
  ui_gen_fn = local_tab_ui_fn,
  n_policy = c(1, 3, 2),
  n_policy_type = "slider", #c("numericInline", "numeric", "slider", "none"),
)


