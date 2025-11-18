#' Helper modules for common UI components generation
#' 
#' @name m_helpers
NULL

#' @describeIn m_helpers Title generation UI function
#' 
#' @param id Module id
#' @param title Title/Label for the input
#' @param ... not used
#' @export
f_title_ui <- function(id, title = NULL, ...) {
  ns <- NS(id)
  if (isTruthy(title) && !is.null(title)) {
    title
  } else {
    if (in_devmode()) ns("Incidences tab title") |> h3()
  }
}

#' @describeIn m_helpers Number of deciles selection UI + server
#' 
#' @param id Module id
#' @param title Title/Label for the input
#' @param choices A name vector of choices for input. If all values are numeric, the first one is used as default value.
#' @param ... Additional arguments passed to `numericInput()`
#' @inheritParams shiny::numericInput
#' 
#' @importFrom shiny NS numericInput
#' @export
#' 
f_numericInput_ui <- function(id, title = NULL, choices = NULL, ...) {
  ns <- NS(id)  

  # Skip if choices and value is not provided, or all choises are not numeric
  if (
    (!isTruthy(choices) || is.null(choices))) {
    return(NULL)
  }

  # Skip if choices are provided but not numeric and no value is provided
  if (isTruthy(choices) && !is.null(choices) && !all(is.numeric(choices))) {
    return(NULL)
  }

  # Set value from choices if choices are numeric and provided. Default 10 otherwise
  if (isTruthy(choices) && !is.null(choices) && all(is.numeric(choices))) {
    value <- as.numeric(choices[1])
  } else {
    return(NULL)
  }

  # Remove from ... any arguments not accepted in shiny::numericInput
  valid_args <- c("min", "max", "step", "width")
  args <- list(...)
  args <- args[names(args) %in% valid_args]
  args <- c(inputId = ns("inputId"), label = title, value = value, args)
  do.call(shiny::numericInput, args)
}


#' @describeIn m_helpers Income deciles by selection UI generator factory
#' @param fn Function to use for input generation. Either `shiny::selectInput` or `shiny::selectizeInput`
#' @return A function generating the desired input UI
#' @export
#' 
f_selegenInput_ui <- function(fn = shiny::selectizeInput) {
  function(
    id,
    title = NULL,
    choices = NULL,
    selected = choices[1],
    ...
  ) {
    ns <- NS(id)

    if (!isTruthy(choices) || is.null(choices)) {
      return(NULL)
    }

    if (length(choices) <= 1) {
      return(NULL)
    }

    valid_args <- c("multiple", "width", "size", "options")
    args <- list(...)
    args <- args[names(args) %in% valid_args]
    args <- c(
      list(
        inputId = ns("inputId"),
        label = title,
        choices = choices,
        selected = selected
      ),
      args
    )
    do.call(fn, args)
  }
}

#' @describeIn m_helpers Income deciles by selection UI using selectInput
#'
f_selectInput_ui <- f_selegenInput_ui(shiny::selectInput)

#' @describeIn m_helpers Income deciles by selection UI using selectizeInput
f_selectizeInput_ui <- f_selegenInput_ui(shiny::selectizeInput)

#' @describeIn m_helpers generate radiogroup buttons UI
#'
#'
f_radioGroupButtons_ui <- function(
  id,
  title = NULL,
  choices = NULL, #c("Bar" = "bar", "Line" = "line", "Scatter" = "scatter"),
  selected = choices[1],
  status = "primary",
  size = "sm",
  individual = TRUE,
  direction = "horizontal",
  ...
) {
  ns <- NS(id)

  # Skip is choices are missing or length less than 2
  if (is.null(choices) || length(choices) <= 1) {
    return(NULL)
  }

  if (!is.null(choices) && length(choices) > 1) {
    shinyWidgets::radioGroupButtons(
      inputId = ns("inputId"),
      label = title,
      choices = choices,
      selected = selected,
      status = status,
      size = size,
      individual = individual,
      direction = direction,
      ...
    )
  }
}
          
