# #' Module for rendering table outputs
# #'
# #' @name m_table
# NULL

# #' @describeIn m_table Table output UI
# m_table_ui <-
#   function(id) {
#     ns <- NS(id)
#     tagList(
#       uiOutput(ns("table_ui"))
#     )
#   }


# #' @describeIn m_table Table output server logic that handles list of tables to display and dataframe, DT::datatable and flextable formats
# #' @param id Module id
# #' @param table A reactive expression returning a data.frame or tibble to display
# #' @param selected A reactive expression returning the name of the selected figure to display
# #' @export
# #'
# m_table_srv <- function(
#   id,
#   tables = reactive(head(mtcars)),
#   selected = reactive(NULL)
# ) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns

#     table_show <- reactive({
#       tbl <- tables()
#       sel <- selected()
#       if (
#         !is.null(tbl) &&
#           is.list(tbl) &&
#           !is.data.frame(tbl) &&
#           sel %in% names(tbl)
#       ) {
#         tbl[[sel]]
#       } else {
#         tables()
#       }
#     })

#     last_class <- reactiveVal(NULL)
#     observe({
#       req(table_show())
#       if (is.data.frame(table_show()) && !identical(last_class(), "tbl")) {
#         isolate(last_class("tbl"))
#       }
#       if (
#         inherits(table_show(), "flextable") &&
#           !identical(last_class(), "flextable")
#       ) {
#         isolate(last_class("flextable"))
#       }
#       if (
#         inherits(table_show(), "datatables") &&
#           !identical(last_class(), "datatables")
#       ) {
#         isolate(last_class("datatables"))
#       }
#     })

#     table_ui <- reactive({
#       req(last_class())
#       switch(
#         last_class(),
#         "tbl" = shiny::tableOutput(ns("tbl")) |> shinycssloaders::withSpinner(),
#         "flextable" = shiny::uiOutput(ns("ft")) |>
#           shinycssloaders::withSpinner(),
#         "datatables" = DT::dataTableOutput(ns("dt")) |>
#           shinycssloaders::withSpinner()
#       )
#     })

#     output$table_ui <- renderUI({
#       req(table_ui())
#       table_ui()
#     })

#     output$dt <- DT::renderDataTable({
#       req(table_show())
#       req(last_class() == "datatables")
#       table_show()
#     })

#     output$ft <- shiny::renderUI({
#       req(table_show())
#       req(last_class() == "flextable")
#       htmltools_value(table_show())
#     })

#     output$tbl <- renderTable({
#       req(table_show())
#       req(last_class() == "tbl")
#       table_show()
#     })
#   })
# }

# #' @describeIn m_table Test app for m_table module
# #' @export
# #'
# test_m_table <- function() {
#   library(shiny)
#   library(DT)
#   library(flextable)
#   library(htmltools)
#   library(bslib)

#   ui <- page_fixed(
#     selectInput(
#       inputId = "tbl_select",
#       label = "Select table:",
#       choices = c("Table 1 (df)", "Table 2 (DT)", "Table 3 (flextable)")
#     ),
#     m_table_ui("table_mod")
#   )

#   server <- function(input, output, session) {
#     test_obj <- reactive({
#       list(
#         `Table 1 (df)` = head(mtcars),
#         `Table 2 (DT)` = DT::datatable(head(iris)),
#         `Table 3 (flextable)` = flextable::flextable(head(mtcars))
#       )
#     })

#     selected <- reactive({
#       input$tbl_select
#     })

#     m_table_srv("table_mod", table = test_obj, selected = selected)
#   }

#   shinyApp(ui, server)
# }