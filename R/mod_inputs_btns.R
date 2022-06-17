#' inputs_btns UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @export
mod_inputs_btns_ui <- function(id = NULL) {
  ns <- NS(id)
  # nsim <- 
  #   numericInput(ns("n_choices"), 
  #                "Number of policy choices", 
  #                value = 2, min = 1, max = 6, step = 1, round = TRUE)
  nsim <- 
    sliderInput(ns("n_choices"), 
                 "Number of policies", 
                 value = 2, min = 1, max = 4, step = 1, round = TRUE)
  run_button <-
    actionButton(ns("run_sim"),
                 "Run",
                 class = "btn-success btn-sm",
                 width = "100%")
  reset_button <-
    actionButton(ns("reset_sim"),
                 "Reset",
                 class = "btn-danger btn-sm",
                 width = "100%")
  download_sim <-
    downloadButton(
      ns("download_sim"),
      label = "Save inputs",
      class = "btn-info btn-sm",
      style = "width:100%;"
    )
  
  upload_sim_file <-
    fileInput(
      ns("upload_sim"),
      "Upload inputs",
      accept = c(".ceqsim"),
      buttonLabel = "Browse",
      width = "100%"
    )
  upload_sim_file[["children"]][[2]][["children"]][[1]][["children"]][[1]][["attribs"]][["class"]] <-
    "btn btn-primary btn-file"
  
  upload_sim_file <- 
    upload_sim_file %>% 
    div(id = ns("upload_sim_holder"))
  
  list(
    # nsim,
    (run_button),
    tags$hr(),
    (reset_button),
    # tags$hr(),
    (download_sim),
    (upload_sim_file),
    # tags$hr(),
    if (getOption("ceq_dev", FALSE))
      actionButton(ns("run_guide"), "Run guide", class = "btn-info btn-sm")
  )
}

# Function for showing the dev output of UI buttons
mod_inputs_btns_devout_ui <- function(id = NULL) {
  ns <- NS(id)
  shiny::verbatimTextOutput(ns("inputs_btns_devout"))
}
    
#' inputs_btns Server Functions
#'
#' @import purrr
#' @noRd 
#' @export
mod_inputs_btns_server <- function(id = NULL, sim_export_dta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    inp_btns_inp <- reactiveValues(run = NULL,
                                   reset = NULL, 
                                   upload_sim = NULL,
                                   n_choices = 2)
    
    # Data download module
    output$download_sim <- downloadHandler(
        filename = function() {
          paste("Policy-choices-", format(Sys.time(),'%Y%m%d%H%M%S'), ".ceqsim", sep="")
        },
        content = function(file) {
          req(sim_export_dta()) 
          readr::write_rds(sim_export_dta(), file, compress = "gz")
        }
      )
    
    # Buttons
    observeEvent(#
      input$reset_sim,
      {
        inp_btns_inp$reset <- input$reset_sim
      })
    
    observeEvent(#
      input$run_sim,
      {
        inp_btns_inp$run <- input$run_sim
      })
    
    observeEvent(#
      input$run_guide,
      {
        inp_btns_inp$run_guide <- input$run_guide
      })
    
    # Number of policies validation
    # n_choices_react <- reactive(input$n_choices) 
    observeEvent(#
      input$n_choices, {
        if (isTruthy(input$n_choices))
          inp_btns_inp$n_choices <- input$n_choices %>%  ceiling %>% as.integer()
        else
          inp_btns_inp$n_choices <- 2
      },
      ignoreNULL = FALSE,
      ignoreInit = FALSE)
        
    observeEvent(#
      inp_btns_inp$upload_sim,
      {
        req(inp_btns_inp$upload_sim$inp)
        new_n_choices <- inp_btns_inp$upload_sim$inp %>% distinct(policy_choice) %>% nrow()
        updateSliderInput(session, "n_choices", value = new_n_choices)
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE)
    
    observeEvent(#
      input$upload_sim,
      {
        file <- input$upload_sim
        ext <- tools::file_ext(file$datapath)
        req(file)
        if (ext != "ceqsim") {
          shinyFeedback::feedbackDanger(
            "upload_sim", 
            show = TRUE, 
            text = "Please upload a '.ceqsim' file."
            )
        }
        validate(
          need(
            ext == "ceqsim",
            "Please upload a '.ceqsim' file, which was previously creted in this tool."
          )
        )
        if (ext == "ceqsim") shinyFeedback::hideFeedback("upload_sim")
        inp_btns_inp$upload_sim <- file$datapath %>% readr::read_rds()
      }, 
      ignoreInit = TRUE, 
      ignoreNULL = TRUE)
    
    
    # output$inputs_btns_devout <- shiny::renderPrint({
    #   # browser()
    #   c("run_sim", "reset_sim", "download_sim", "upload_sim") %>% 
    #     map(~{
    #       set_names(x = list(input[[.x]]), .x)
    #     }) %>% 
    #     unlist(recursive = F)
    # })
    
    inp_btns_inp
    
  })
}
    
## To be copied in the UI
# mod_inputs_btns_ui("inputs_btns_ui_1")
    
## To be copied in the server
# mod_inputs_btns_server("inputs_btns_ui_1")


#' @noRd 
#' @importFrom rlang dots_list
#' @importFrom magrittr extract
#' @importFrom shiny numericInput
make_num_inpt_ui <- function(...) {
  rlang::dots_list(...) %>%
    unlist(recursive = T) %>% 
    as.list() %>% 
    magrittr::extract(names(.) %in% 
                        c("inputId", "label", "value", "min", "max", "step", "width")) %>%
    magrittr::extract(!is.na(.)) %>%
    do.call(what = shiny::numericInput, args = .)
}

