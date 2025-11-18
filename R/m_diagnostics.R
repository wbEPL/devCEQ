#' Module for simple diagnostics
#' 
#' @name m_diagnostics
#' 
NULL


#' @describeIn m_diagnostics Diagnostics UI
#' 
#' 
m_diagnostics_ui <-
  function(id) {
    ns <- NS(id)
    if (in_devmode()) {
      card(
        card_title("Diagnostics"),
        card_header("NS: ", ns(NULL)),        
        full_screen = TRUE,
        actionButton(ns("browse"), "Browse diagnositcs"),
        verbatimTextOutput(ns("diag_text"))
      )
    }
  }

#' @describeIn m_diagnostics Diagnostics server logic
#' @param out A reactive expression returning the object to diagnose
#' 
m_diagnostics_srv <-
  function(
    id,
    out = reactive(NULL),
    ...
  ) {
    moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns
        observeEvent(
          input$browse,
          {
            if (in_devmode()) {
              browser()
            }
          },
          ignoreInit = TRUE,
          ignoreNULL = TRUE
        )

        output$diag_text <- renderPrint({
          if (in_devmode()) {
            str(out())
          } 
        })
      }
    )
  }




