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