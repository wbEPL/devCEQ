#' inputs_btns UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param choice_type type of the input UI for number of choices. One of
#' slider, numeric and none.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @export
mod_inputs_btns_ui <- function(id = NULL, ...) {
  ns <- NS(id)

  nsim <- shiny::uiOutput(ns("n_policy_ui"))

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

  input_tabs <- mod_inp_switches_ui(id = id)

  list(
    nsim %>% div(id = ns("input_sim_number_holder")),
    input_tabs %>%
      div(id = ns("input_tabs_nav_holder")) %>%
      div(id = ns("input_tabs_nav_holder_2")) %>%
      div(id = ns("input_tabs_nav_holder_3")),
    tags$hr(),
    run_button %>%
      div(id = ns("run_btn_holder"))%>%
      div(id = ns("run_btn_holder_2")),
    # tags$hr(),
    reset_button %>% div(id = ns("reset_btn_0")),
    # tags$hr(),
    download_sim %>% div(id = ns("download_sim_holder")),
    upload_sim_file %>% div(id = ns("upload_sim_holder")),
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
#' @param n_policy vector of 3 numeric. First and second are min and max
#' number of choices. The last is the initialized number of choices.
#'
#' @param n_policy_type character, one of c("numericInline", "numeric", "slider", "none")
#' @import purrr
#' @noRd
#' @export
mod_inputs_btns_server <-
  function(id = NULL,
           sim_export_dta,
           n_policy = c(1, 2, 1),
           n_policy_type = c("numericInline", "numeric", "slider", "none"),
           ... ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (!isTruthy(n_policy_type)) {n_policy_type <- "slider"}

    if (!isTruthy(n_policy)) {
      n_policy <- c(1, 1, 1)
      n_policy_type <- "none"
    }

    if (length(n_policy) == 1) {
      n_policy <- rep(n_policy, 3)
      n_policy_type <- "none"
    }

    inp_btns_inp <- reactiveValues(
      run = NULL,
      reset = NULL,
      upload_sim = NULL,
      n_choices = n_policy[[length(n_policy)]],
      n_max_choices = n_policy[[2]]
    )

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # Data download module
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    output$download_sim <- downloadHandler(
        filename = function() {
          paste("Policy-choices-", format(Sys.time(),'%Y%m%d%H%M%S'), ".ceqsim", sep="")
        },
        content = function(file) {
          req(sim_export_dta())
          readr::write_rds(sim_export_dta(), file, compress = "gz")
        }
      )

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # Butons
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    observeEvent(input$reset_sim, {inp_btns_inp$reset <- input$reset_sim})
    observeEvent(input$run_sim, {inp_btns_inp$run <- input$run_sim})
    observeEvent(input$run_guide, {inp_btns_inp$run_guide <- input$run_guide})

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # Number of policy choices
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

    # Generating the UI
    output$n_policy_ui <-
      renderUI({
        make_n_choice_ui(value = n_policy[[length(n_policy)]],
                         min = n_policy[[1]],
                         max = n_policy[[2]],
                         choice_type = n_policy_type[[1]],
                         ns = ns)
        })

    # Observing and checking that number of policies do not exceed maximum
    n_choices_react <- reactive(input$n_choices)# %>% debounce(100)
    observeEvent(#
      n_choices_react(), {
        if (isTruthy(n_choices_react())) {

          if (n_choices_react() > n_policy[[2]]) {
            shiny::showNotification(
              stringr::str_c("Maximum number of policy choices is ", n_policy[[2]]),
              duration = 10,
              type = "warning"
            )
            # updateNumericInput(session, "n_choices", value = min(n_policy[[2]], n_choices_react()))
            # updateSliderInput(session, "n_choices", value = min(n_policy[[2]], n_choices_react()))
          }

          if (n_choices_react() < n_policy[[1]]) {
            shiny::showNotification(
              stringr::str_c("Minimum number of policy choices is ", n_policy[[1]]),
              duration = 10,
              type = "warning"
            )
            # updateNumericInput(session, "n_choices", value = max(n_policy[[1]], n_choices_react()))
            # updateSliderInput(session, "n_choices", value = max(n_policy[[1]], n_choices_react()))
          }

          inp_btns_inp$n_choices <- n_choices_react() %>% ceiling %>% as.integer()

        } else{
          inp_btns_inp$n_choices <-  n_policy[[length(n_policy)]]
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = FALSE)

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # Simulation upload
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    observeEvent(#
      inp_btns_inp$upload_sim,
      {
        req(inp_btns_inp$upload_sim$inp)
        new_n_choices <- inp_btns_inp$upload_sim$inp %>% distinct(policy_choice) %>% nrow()
        updateSliderInput(session, "n_choices", value = new_n_choices)
        updateNumericInput(session, "n_choices", value = new_n_choices)
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



#' @noRd
#' @importFrom rlang dots_list
#' @importFrom magrittr extract
#' @importFrom shiny numericInput
#' @export
make_n_choice_ui <-
  function(value = 1,
           min = 1,
           max = 2,
           choice_type = "slider",
           ns = NS(NULL)) {

    nsim <- tagList()
    if (choice_type == "slider") {
      nsim <- sliderInput(
        ns("n_choices"),
        "Number of policies",
        value = value,
        min = min,
        max = max,
        step = 1,
        round = TRUE
      )

    } else if (choice_type == "numeric") {
      nsim <-
        numericInput(
          ns("n_choices"),
          "Number of policy choices",
          value = value,
          min = min,
          max = max,
          step = 1
        )

    } else if (choice_type == "numericInline") {
      nsim <-
        tags$div(
          id = "inline2",
          class = "inline",
          numericInput(
            ns("n_choices"),
            label = "Number of policy choices:  ",
            value = value,
            min = min,
            max = max,
            step = 1,
            width = "100%"
          )
        ) %>%
        tagList(., tags$hr())

    } else if (choice_type == "none") {
      nsim <- tagList()

    } else {
      nsim <-
        numericInput(
          ns("n_choices"),
          "Number of policy choices",
          value = value,
          min = min,
          max = max,
          step = 1
        )
    }

    nsim
  }

#' @describeIn mod_inputs_btns_server update_n_choice_ui
#' @noRd
update_n_choice_ui <-
  function(value = 1,
           min = 1,
           max = 2,
           choice_type = "slider",
           ns = NS(NULL)) {

    nsim <- tagList()
    if (choice_type == "slider") {
      nsim <- updateSliderInput(
        ("n_choices"),
        value = value,
        min = min,
        max = max
      )

    } else if (choice_type == "numeric") {
      nsim <-
        updateNumericInput(
          ("n_choices"),
          value = value,
          min = min,
          max = max
        )

    } else if (choice_type == "numericInline") {
      nsim <-
        tags$div(
          id = "inline2",
          class = "inline",
          updateNumericInput(
            ("n_choices"),
            value = value,
            min = min,
            max = max
          )
        ) %>%
        tagList(., tags$hr())

    } else if (choice_type == "none") {
      nsim <- tagList()

    } else {
      nsim <-
        updateNumericInput(
          ("n_choices"),
          value = value,
          min = min,
          max = max
        )
    }

    nsim
  }
