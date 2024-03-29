#' (internal) Module with buttons logic for the input page
#'
#' @description
#'
#' `mod_inputs_btns_ui()` UI part of the module.
#'
#' `mod_inputs_btns_server()` UI part of the module.
#'
#' `test_mod_inputs_btn_static_ui()` test-function for the static buttons
#' appearance. Launches a simple shiny app that displays a static buttons UI.
#'
#' `test_mod_inputs_btn()` test-function for checking buttons with
#' the server-generated number of policies. Launches a shiny app with
#'
#' @param id namespace id (internal shiny parameter) used in the the UI module.
#'
#' @param n_policy vector of 3 numeric values c(1,2,1) by default.
#'    The first and the second are minimum and maximum number of policy choices.
#'    The numeric is the initialized number of policy choices.
#'
#' @param n_policy_type character that defines types of the the changer of the
#'    the number of policy scenarios. Could be one of "numericInline" (default),
#'    "numeric", "slider", "dropdown", and "none".
#'
#' @param sim_export_dta Reactive internal with all the simulation data
#'    that is used by the server to export simulation inputs results.
#'
#' @param ... not used
#'
#' @import purrr
#'
#' @examplesIf interactive()
#'
#' # Launches an app with all the buttons in a static form
#' test_mod_inputs_btn_static_ui()
#'
#' # Launches an app with all types of policy number input and extra diagnostic
#' test_mod_inputs_btn()
mod_inputs_btns_server <-
  function(id = NULL,
           sim_export_dta = reactive(NULL),
           n_policy = c(1, 2, 1),
           n_policy_type = c("numericInline", "numeric", "slider", "dropdown", "none"),
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
    current_n_choices <-
      mod_inp_n_choices_server(
        id = NULL,
        value = reactive(n_policy[[length(n_policy)]]),
        min = reactive(n_policy[[1]]),
        max = reactive(n_policy[[2]]),
        n_policy_type = reactive(n_policy_type[[1]]),
        n_policy_update = reactive({
          req(inp_btns_inp$upload_sim$inp)
          inp_btns_inp$upload_sim$inp %>% distinct(policy_choice) %>% nrow()
        })
      )

    observe({
      req(current_n_choices())
      isolate({
        inp_btns_inp$n_choices <- current_n_choices()
      })
    })

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # Simulation upload
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
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

    output$inputs_btns_devout <- shiny::renderPrint({
        c("run_sim", "reset_sim", "download_sim", "upload_sim") %>%
        map(~{set_names(x = list(input[[.x]]), .x)}) %>%
        append(reactiveValuesToList(inp_btns_inp)) %>%
        unlist(recursive = F)

    })

    inp_btns_inp

  })
}

#' @title test-function for input buttons with the server-generated number of policies
#' @rdname mod_inputs_btns_server
#' @export
test_mod_inputs_btn <-
  function(id = NULL, n_policy_type = "slider") {
    options(golem.app.prod = FALSE)
    get_n_policy_types() %>%
      map(~{
        mod_inputs_btns_ui(id = .x) %>%
          column(width = 2)
      }) %>%
      tagList() %>%
      fluidPage() %>%
      shinyApp(
        server =
          function(input, output, session) {
            all_servers <-
              get_n_policy_types() %>%
              map(~{
                mod_inputs_btns_server(
                  id = .x,
                  sim_export_dta = reactive(tibble(inp = 1)),
                  n_policy = c(1, 5, 3),
                  n_policy_type = .x
                )
              })

            observe({
              all_servers %>% map(~{.x %>% reactiveValuesToList()})
            })

          })
  }

#' @title test-function for the static buttons appearance
#' @rdname mod_inputs_btns_server
#' @export
test_mod_inputs_btn_static_ui <- function(di = NULL) {
  fluidPage(column(3, mod_inputs_btns_ui(NULL))) %>%
    shinyApp(function(input, output, session) {
    })
}

#' @title inputs_btns UI part of the module with input buttons
#' @rdname mod_inputs_btns_server
#' @importFrom shiny NS tagList actionButton downloadButton fileInput
mod_inputs_btns_ui <- function(id = NULL, ...) {
  ns <- NS(id)

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

  input_tabs <- mod_inp_tab_switches_ui(id = id)

  tagList(
    ## Number of simulations
    mod_inp_n_choices_ui(id) %>%
      div(id = ns("input_sim_number_holder")),

    ## Input tabs switchers
    input_tabs %>%
      div(id = ns("input_tabs_nav_holder")) %>%
      div(id = ns("input_tabs_nav_holder_2")) %>%
      div(id = ns("input_tabs_nav_holder_3")),

    ## Buttons:
    tags$hr(),

    ### Run:
    run_button %>%
      div(id = ns("run_btn_holder"))%>%
      div(id = ns("run_btn_holder_2")),
    # tags$hr(),

    ### Reset:
    reset_button %>% div(id = ns("reset_btn_0")),
    # tags$hr(),

    ### Download:
    download_sim %>% div(id = ns("download_sim_holder")),

    ### Upload:
    upload_sim_file %>% div(id = ns("upload_sim_holder")),

    ## CEQ dev options:
    mod_inputs_btns_devout_ui(id)
  )
}

#' @title Debugging input buttons activated up on `options("golem.app.prod") <- FALSE`
#' @noRd
mod_inputs_btns_devout_ui <- function(id = NULL) {
  ns <- NS(id)
  if (golem::app_dev()) {
    tagList(
      mod_browser_button_ui(NULL, FALSE),
      actionButton(ns("run_guide"), "Run guide", class = "btn-info btn-sm"),
      shiny::verbatimTextOutput(ns("inputs_btns_devout"))
    )
  } else {
    tagList(
      mod_browser_button_ui(NULL)
    )
  }
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


