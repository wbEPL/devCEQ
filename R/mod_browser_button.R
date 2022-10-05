#' browser_button UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @export
mod_browser_button_ui <- function(id, hide = TRUE){
  ns <- NS(id)
  tagList(
    actionButton("browser", "browser"),
    if (hide) tags$script("$('#browser').hide();")
  )
}

#' browser_button Server Functions
#'
#' @noRd
#' @export
mod_browser_button_server <- function(id, ...){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observeEvent(input$browser,{
      browser()
    })
  })
}

## To be copied in the UI
# mod_browser_button_ui("browser_button_1")

## To be copied in the server
# mod_browser_button_server("browser_button_1")
