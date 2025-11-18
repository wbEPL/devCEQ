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
            out <- out()
            out$ggs <- names(out$ggs)
            out$fts <- names(out$fts)

            out <-
              out |>
              map(
                ~ {
                  .x$ggs <- names(.x$ggs)
                  .x$fts <- names(.x$fts)
                  .x
                }
              )

            str(out, max.level = 3)
          }
        })
      }
    )
  }


#' @describeIn m_diagnostics Test app for m_diagnostics module
#' @export
test_m_diagnostics <- function() {
  library(shiny)

  ui <- page_fluid(
    m_diagnostics_ui("diag1")
  )

  server <- function(input, output, session) {
    test_obj <- reactive({
      list(
        a = rnorm(100),
        b = data.frame(x = rnorm(50), y = runif(50)),
        c = list(sub1 = letters[1:10], sub2 = matrix(1:9, nrow = 3))
      )
    })

    m_diagnostics_srv("diag1", out = test_obj)
  }

  shinyApp(ui, server)
}



