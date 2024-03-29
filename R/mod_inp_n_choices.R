#' (internal) Sub-module for the policy number inputs
#'
#' @description
#'
#' `mod_inp_n_choices_server()` Server logic
#'
#' `mod_inp_n_choices_ui()` UI of the module
#'
#' `get_n_policy_types()` returns valid types of the n-policy input
#'
#' `make_n_choice_ui()` internal function that creates UI for the n-policy input#'
#'
#' `update_n_choice_ui()` internal function used to update the n-policy input.
#'
#' `test_mod_inp_n_choices_all_ui()` and `test_mod_inp_n_choices()` miscellaneous
#' testing functions that rerun shiny apps.
#'
#' @param min,max,value,n_policy_type,n_policy_update reactive numeric and
#' strings similar to params in `make_n_choice_ui()`
#'
#' @param value,min,max numeric with number of choices selected, minimum
#' and maximum
#'
#' @param n_policy_type reactive string with the type of the n-policy input,
#' one of "numericInline", "numeric", "slider", "dropdown", and "none".
#' Use `get_n_policy_types()` to check valid types.
#'
#' @param new_n_choices new value of policy choices to update to.
#'
#' @param ns namespace function to wrap into IDs around `NS(...)`.
#'
#' @param ... not used
#' @examplesIf interactive()
#'
#' test_mod_inp_n_choices()
#' test_mod_inp_n_choices_all_ui()
#'
mod_inp_n_choices_server <-
  function(id,
           min = reactive(1),
           max = reactive(4),
           value = reactive(2),
           n_policy_type = reactive(get_n_policy_types()[[1]]),
           n_policy_update = reactive(NULL),
           ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # Rendering UI
      output$n_policy_ui <-
        renderUI({
          make_n_choice_ui(
            value = value(),
            min = min(),
            max = max(),
            n_policy_type = n_policy_type(),
            ns = ns,
            ...
          )
        })

      # Updating to the externally uploaded simulation
      observeEvent( #
        n_policy_update(),
        {
          req(n_policy_update())
          update_n_choice_ui(
            input, output, session,
            new_n_choices = n_policy_update(),
            n_policy_type = n_policy_type()
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      # Observing and checking that number of policies do not exceed maximum
      n_choices_react <- reactive(input$n_choices) # %>% debounce(100)

      eventReactive( #
        n_choices_react(),
        {
          out <- NULL
          if (isTruthy(n_choices_react())) {
            if (n_choices_react() > max()) {
              shiny::showNotification(
                stringr::str_c("Maximum number of policy choices is ", max()),
                duration = 10,
                type = "warning"
              )
            }

            if (n_choices_react() < min()) {
              shiny::showNotification(
                stringr::str_c("Minimum number of policy choices is ", min()),
                duration = 10,
                type = "warning"
              )
            }

            out <-
              n_choices_react() %>%
              as.numeric() %>%
              ceiling() %>%
              as.integer()
          } else {
            out <- value()
          }
          out
        },
        ignoreNULL = FALSE,
        ignoreInit = FALSE
      ) %>%
        debounce(500)
    })
  }

#' @rdname mod_inp_n_choices_server
#' @title UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @importFrom shiny NS tagList
mod_inp_n_choices_ui <- function(id) {
  ns <- NS(id)
  tagList(shiny::uiOutput(ns("n_policy_ui")))
}


#' @rdname mod_inp_n_choices_server
#' @title Function for updating the UI with number of choices input
#' @importFrom rlang dots_list
#' @importFrom magrittr extract
#' @importFrom shiny numericInput
#' @importFrom shinyWidgets pickerInput
make_n_choice_ui <-
  function(ns = NS(NULL),
           value = 1,
           min = 1,
           max = 2,
           n_policy_type = "slider",
           ...) {
    nsim <- tagList()
    if (n_policy_type == "slider") {
      nsim <- sliderInput(
        ns("n_choices"),
        "Number of policies",
        value = value,
        min = min,
        max = max,
        step = 1,
        round = TRUE
      )
    } else if (n_policy_type == "numeric") {
      nsim <-
        numericInput(
          ns("n_choices"),
          "Number of policy choices",
          value = value,
          min = min,
          max = max,
          step = 1
        )
    } else if (n_policy_type == "numericInline") {
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
        tagList(tags$hr())
    } else if (n_policy_type == "dropdown") {
      nsim <-
        tags$div(
          id = "inline2",
          class = "inline",
          shinyWidgets::pickerInput(
            ns("n_choices"),
            label = "Number of policy choices:  ",
            choices = seq(min, max),
            selected = value,
            width = "fit",
            inline = TRUE
          )
        )
    } else if (n_policy_type == "none") {
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

#' @title returns valid types for the policy choices
#' @rdname mod_inp_n_choices_server
#' @export
get_n_policy_types <- function() {
  c("numericInline", "numeric", "slider", "dropdown", "none")
}

#' @rdname mod_inp_n_choices_server
#' @importFrom shinyWidgets updatePickerInput
update_n_choice_ui <-
  function(input,
           output,
           session,
           new_n_choices = NULL,
           n_policy_type = get_n_policy_types()[[1]],
           ...) {
    if (n_policy_type %in% c("slider")) {
      updateSliderInput(session = session,
                        inputId = "n_choices",
                        value = new_n_choices)
    } else if (n_policy_type %in% c("dropdown")) {
      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "n_choices",
                                      selected = new_n_choices)
    } else {
      updateNumericInput(session = session,
                         inputId = "n_choices",
                         value = new_n_choices)
    }
  }


#' @title Test n_choices module
#' @rdname mod_inp_n_choices_server
#' @export
test_mod_inp_n_choices <- function(id = NULL) {
  tst_ui <-
    fluidPage(
      column(4, mod_inp_n_choices_ui(id)),
      column(
        6,
        numericInput("min", "min", 1),
        numericInput("max", "max", 3),
        numericInput("value", "value", 2),
        numericInput("update_to", "update_to", value = NULL),
        selectInput(
          "n_policy_type",
          "n_policy_type",
          choices = get_n_policy_types()
        ),
        shiny::verbatimTextOutput("nchoices_out")
      )
    )
  tst_srv <- function(input, output, session) {
    nc_out <- mod_inp_n_choices_server(
      id = id,
      min = reactive(input$min),
      max = reactive(input$max),
      value = reactive(input$value),
      n_policy_type = reactive(input$n_policy_type),
      n_policy_update = reactive(input$update_to)
    )

    output$nchoices_out <- renderPrint({
      nc_out()
    })
  }

  shiny::shinyApp(tst_ui, tst_srv)
}

#' @title another n-policy test function.
#' @rdname mod_inp_n_choices_server
#' @export
#' @importFrom purrr map
test_mod_inp_n_choices_all_ui <- function(id = NULL) {
  tst_ui <- get_n_policy_types() %>%
    map(~ {
      make_n_choice_ui(
        ns = NS(id),
        min = 1,
        max = 5,
        value = 3,
        n_policy_type = .x
      )
    }) %>%
    tagList %>%
    column(width = 4) %>%
    fluidPage()

  tst_srv <- function(input, output, session) {
  }

  shiny::shinyApp(tst_ui, tst_srv)
}
