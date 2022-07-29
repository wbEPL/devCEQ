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

inputs_str <- devCEQ::load_input_xlsx("./data-raw/ben-inputs-structure.xlsx")

presim <- reactive({
  out <- list() #read_rds("./data-app/presim-2022.rds")
  out$bl_res <- NULL #read_rds("./data-app/sim-baseline-2022.rds")
  out$bl_tbl <- NULL
  out
})


# App related parameters

# Title of the App
options(current.app.name = "Benin CEQ")

devCEQ::CEQ_run(
  inputs_str = inputs_str,
  presim = presim,
  choice_max = 2, choice_type = "slider",
  ui_fn = CEQ_ui
)
