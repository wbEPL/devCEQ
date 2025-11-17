#' Helper modules for common UI components generation
#' 
#' @name m_helpers
NULL

#' @describeIn m_helpers Title generation module
#' 
#' @importFrom shiny NS uiOutput renderUI
#' 
m_title_ui <- function(id, ...) {
  ns <- NS(id)
  uiOutput(ns("title"))
}

#' @describeIn m_helpers Title server module
#' @importFrom shiny moduleServer reactive renderUI
m_title_srv <- function(id, title_reactive = reactive(title)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$title <-
      renderUI({
        if (isTruthy(title_reactive()) && !is.null(title_reactive())) {
          title_reactive()
        } else {
          if (in_devmode()) ns("Incidences tab title") |> h3()
        }
      })
  })
}

#' @describeIn m_helpers mNumber of deciles selection UI + server
#' 
#' @param id Module id
#' @param label Label for the numeric input
#' @param value Default value for the numeric input
#' @inheritParams shiny::numericInput
#' @param ... Additional arguments passed to `numericInput()`
#' 
#' @importFrom shiny NS numericInput
#' @export
#' 
f_numericInput_ui <- function(id, label = NULL, value = NULL, ...) {
  ns <- NS(id)
  
  # # Check if label is provided
  # if (missing(label) || is.null(label)) {
  #   label <- "Number of deciles:"
  # }

  # # Check if value is provided
  # if (missing(value) || is.null(value)) {
  #   value <- 5
  # }

  # Remove from ... any arguments not accepted in shiny::numericInput
  valid_args <- c("min", "max", "step", "width")
  args <- list(...)
  args <- args[names(args) %in% valid_args]
  
  do.call(
    shiny::numericInput,
    c(
      list(
        inputId = ns("n_dec"),
        label = label,
        value = value
      ),
      args
    )
  )

}

#' @describeIn m_helpers mNumber of deciles selection server
#' @param id Module id
#' @export
#' 
m_numericInput_srv <- function(id, label = NULL, value = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Generate the UI of input
    output$ui <- renderUI(f_numericInput_ui(ns(NULL), label, value, ...))
    
    # Collect the value as reactive
    reactive(input$n_dec)
  })
}

#' @describeIn m_helpers Income deciles by selection UI wrapper
#' @param id Module id
#' @export
#' 
m_input_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui"))
}

#' @describeIn m_helpers Income deciles by selection server wrapper
#' @param id Module id
#' @export
#' 
m_input_srv <- function(id, type = "numericInput", ...) {  
  fn_mod <- switch(
    type,
    "numericInput" = m_numericInput_srv,
    "selectInput" = m_selectInput_srv,
    "radioGroupButtons" = m_radioGroupButtons_srv,
    stop("Unknown module type")
  )
  fn_mod(id, ...)  
}



#' @describeIn m_helpers Income deciles by selection UI
#' @param id Module id
#' @param dec_by_label Label for the select input
#' @param choices Reactive expression returning a character vector of choices
#' @importFrom shiny NS selectInput
#' @export
#'
f_selectInput_ui <- function(
  id,
  label = NULL, #"Deciles by:",
  choices = NULL, #f_var_names_vector(get_inc_nm()),
  selected = choices[1],
  ...
) {
  ns <- NS(id)

  valid_args <- c("multiple", "width", "size", "selectize")
  args <- list(...)
  args <- args[names(args) %in% valid_args]

  if (!isTruthy(choices) || is.null(choices)) {
    return(NULL)
  } else {
    do.call(
      shiny::selectInput,
      c(
        list(
          inputId = ns("sel_imp"),
          label = label,
          choices = choices,
          selected = selected
        ),
        args
      )
    )
  }
}

#' @describeIn m_helpers Income deciles by selection server
#' @param id Module id
#' @export
m_selectInput_srv <- function(id, label = NULL, choices = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Check if choices is not a reactive and make it reactive
    choices_reactive <- reactive({
      req(choices)
      if (!is.reactive(choices)) {
        return(choices)
      } else {
        return(choices())
      }
    })

    # Collect the value as reactive
    out <- reactiveVal()
    
    # Set initial value
    observe(out(choices_reactive()[1]))
    
    # Generate the UI of input
    output$ui <- renderUI(f_selectInput_ui(
      ns(NULL),
      label,
      choices = choices_reactive(),
      ...
    ))

    # Collect the value as reactive
    observeEvent(
      input$sel_imp,
      {
        new_out <- input$sel_imp
        if (class(choices_reactive()) == "numeric") {
          new_out <- as.numeric(new_out)
        }        
        if (class(choices_reactive()) == "integer") {
          new_out <- as.numeric(new_out)
        }
        out(new_out)
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    out
  })
}


#' @describeIn m_helpers generate radiogroup buttons UI
#'
#'
f_radioGroupButtons_ui <- function(
  id,
  label = NULL,
  choices = c("Bar" = "bar", "Line" = "line", "Scatter" = "scatter"),
  ...
) {
  ns <- NS(id)
  if (!is.null(choices) && length(choices) > 1) {
    shinyWidgets::radioGroupButtons(
      inputId = ns("plot_var"),
      label = label,
      choices = choices,
      selected = choices[[1]],
      status = "primary",
      size = "sm",
      individual = TRUE,
      direction = "horizontal",
    )
  }
}

m_radioGroupButtons_srv <- function(id, choices = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    # reactive(input$plot_var)
    out <- reactiveVal(NULL)
    observeEvent(choices[[1]], {
      out(choices[[1]])
    }, ignoreNULL = TRUE)
    observeEvent(
      input$plot_var,
      {
        out(input$plot_var)
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )
    out    
  })
}
              

# Generate random data based gg plot also choosing between bar, line and scatter randomly

fig_gg_random <- function() {
  
  type <- sample(c("bar", "line", "scatter"), 1)
  n <- sample(5:15, 1)
  dta <- data.frame(x = 1:n, y = sample(1:100, n))
  dta |> 
    ggplot(aes(x = x, y = y)) +
    {
      if (type == "bar") {
        geom_bar(stat = "identity")
      } else if (type == "line") {
        geom_line()
      } else {
        geom_point()
      }
    } +
    theme_minimal()
}


