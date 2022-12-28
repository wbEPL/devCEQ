options(golem.app.prod = FALSE)
options(shiny.port = httpuv::randomPort())
options(shiny.fullstacktrace = TRUE)
options(shiny.reactlog=TRUE)
# golem::detach_all_attached()
# golem::document_and_reload()

# Dev paras
options(scipen = 16)

library(shiny)
# remotes::install_github("CEQwb/devCEQ")
devtools::load_all()
# library(devCEQ)

# inputs_path <- "./data-raw/ceq-inputs-idn-2022.xlsx"
inputs_path <- "./data-raw/ben-inputs-structure.xlsx"
inputs_raw_str <- inputs_path %>% load_input_xlsx()
inputs_tab_str <- inputs_path %>% load_inputtabs_xlsx()
inputs_table_str <- inputs_path %>% load_inputtables_xlsx()

# inp_tab_str_ordered <- fct_inp_tab_order(inputs_tab_str)

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
local_ceq_ui <-
  gen_ceq_ui(
    inp_nav_width = 3,
    fn_results_ui = fn_results_ui_dummy2
  )

# 4. Server side logic
local_run_sim_server <- make_run_sim_server()

# Title of the App
options(current.app.name = "CEQ app name that is very long")
devtools::load_all()

# Running the CEQ
devCEQ::CEQ_run(
  inputs_str = inputs_raw_str,
  presim = presim,
  ui_fn = local_ceq_ui,
  inp_str_fn = local_inp_str_fn,
  ui_gen_fn = local_tab_ui_fn,
  n_policy = c(1, 3, 2),
  n_policy_type = "dropdown",
  info_page_md = "./inst/app/info-page.md",

  fn_sim_srvr = local_run_sim_server
)


