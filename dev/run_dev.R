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

inputs_path <- "./data-raw/complex-inputs-structure.xlsx"
inputs_str <- devCEQ::load_input_xlsx(inputs_path)
inputs_tab_str <- devCEQ::load_inputtabs_xlsx(inputs_path)


presim <- reactive({
  out <- list() #read_rds("./data-app/presim-2022.rds")
  out$bl_res <- NULL #read_rds("./data-app/sim-baseline-2022.rds")
  out$bl_tbl <- NULL
  out
})

# Function for generating inputs UI
gen_local_ui_fn <- gen_tabinp_ui_front(inputs_tab_str)

# Title of the App
options(current.app.name = "CEQ")

devCEQ::CEQ_run(
  inputs_str = inputs_str,
  presim = presim,
  ui_fn = CEQ_ui,
  ui_gen_fn = gen_local_ui_fn,
  n_policy = c(1, 3, 2),
  n_policy_type = "slider", #c("numericInline", "numeric", "slider", "none"),
)
