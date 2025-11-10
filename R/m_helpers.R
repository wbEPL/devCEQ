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
#' @param id Module id
#' @param label Label for the numeric input
#' @param range Range for the numeric input
#' @param ... Additional arguments passed to `numericInput()`
#' 
#' @importFrom shiny NS numericInput
#' @export
#' 

f_numericInput_ui <- function(id, label = "Number of deciles", range = c(5, 50), ...) {
  ns <- NS(id)
  numericInput(
    ns("n_dec"),
    label = label,
    value = 10,
    min = range[1],
    max = range[2],
    ...
  )
}

#' @describeIn m_helpers mNumber of deciles selection server
#' @param id Module id
#' @export
#' 
m_numericInput_srv <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    reactive({
      input$n_dec
    })
  })
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
  label = "Deciles by:",
  choices = f_var_names_vector(get_inc_nm()),
  ...
) {
  ns <- NS(id)
  # is is not reactive

  if (!isTruthy(choices) || is.null(choices)) {
    return(NULL)
  } else {
    selectInput(
      ns("sel_imp"),
      label = label,
      choices = choices,
      selected = choices[1],
      width = "350px",
      ...
    )
  }
}

#' @describeIn m_helpers Income deciles by selection server
#' @param id Module id
#' @export
m_selectInput_srv <- function(id, choices = NULL) {
  moduleServer(id, function(input, output, session) {
    out <- reactiveVal(choices[[1]])
    observeEvent(
      input$sel_imp,
      {
        out(input$sel_imp)
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


