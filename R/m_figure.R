#' module for automatically updating the figure placeholders and figure up on selection
#' 
#' @name m_figure
NULL


#' Module UI for figure updating
#' 
m_figure_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("figure_ui"))
  )
}

#' Module Server for figure updating
#' @param id Module id
#' @param figures A reactive expression returning a named list of figures (ggplot or plotly objects)
#' @param selected A reactive expression returning the name of the selected figure to display
#' 
#' @importFrom shiny moduleServer NS uiOutput renderUI plotOutput renderPlot tableOutput renderTable
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom DT renderDT DTOutput
#' @importFrom flextable flextable htmltools_value
#' @export
#'
m_figure_server <- function(
  id,
  figures = reactive(
    list(
      `Figure 1` = fig_gg_random(),
      `Figure 2` = fig_gg_random(),
      `Figure 3` = fig_gg_random() |> plotly::ggplotly(),
      `DT` = DT::datatable(head(mtcars)),
      `Flextable` = flextable::flextable(head(mtcars)),
      `DF` = head(mtcars)
    )
  ),
  selected = reactive(
    "Figure 1"
  ),
  force_ly = TRUE,
  ...
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fig_show <- reactive({
      req( figures() )
      req( selected() )
      figs <- figures()
      sel <- selected()

      # Check if figs is a data frame
      if (
        is.data.frame(figs) |
          inherits(figs, "datatables") |
          inherits(figs, "flextable") |
          inherits(figs, "reactable")
      ) {
        return(figs)
      }

      if (!is.null(figs) && sel %in% names(figs)) {
        out <- figs[[sel]]
      } else if (length(figs) == 0) {
        out <- NULL
      } else {
        out <- figures()[[1]]
      }
      if (force_ly) {
        out <- plotly::ggplotly(out, tooltip = "text") |>
          layout(
            # autosize = TRUE,
            height = "100%"
          ) |> 
          format_plotly()
      }
      out
    })

    # identify the class of the figure and create the appropriate rendering function
    last_fig_class <- reactiveVal(NULL)
    observe({
      req(fig_show())

      if (
        inherits(fig_show(), "ggplot") && !identical(last_fig_class(), "ggplot")
      ) {
        isolate(last_fig_class("ggplot"))
      }

      if (inherits(fig_show(), "plotly")) {
        isolate(last_fig_class("plotly"))
      }

      if (
        inherits(fig_show(), "flextable") &&
          !identical(last_fig_class(), "flextable")
      ) {
        isolate(last_fig_class("flextable"))
      }

      if (
        inherits(fig_show(), "datatables") &&
          !identical(last_fig_class(), "datatables")
      ) {
        isolate(last_fig_class("datatables"))
      }

      if (
        inherits(fig_show(), "reactable") &&
          !identical(last_fig_class(), "reactable")
      ) {
        isolate(last_fig_class("reactable"))
      }

      # If a react table is provided

      if (is.data.frame(fig_show()) && !identical(last_fig_class(), "tbl")) {
        isolate(last_fig_class("tbl"))
      }
    })

    # Generating UI for figure output
    fn_figui <- reactive({
      req(last_fig_class())
      switch(
        last_fig_class(),
        "ggplot" = plotOutput(ns("fig_gg")) ,
        "plotly" = plotly::plotlyOutput(ns("fig_ly")),
        "flextable" = uiOutput(ns("fig_ft")),
        "datatables" = DT::DTOutput(ns("fig_dt")),
        "tbl" = tableOutput(ns("fig_tbl")),
        "reactable" = reactable::reactableOutput(ns("fig_rt"))
      )
    })

    output$figure_ui <- renderUI(fn_figui() |> shinycssloaders::withSpinner())

    # Rendering figure based on its class
    output$fig_gg <- renderPlot({
      req(fig_show())
      req(inherits(fig_show(), "ggplot"))
      fig_show()
    })

    output$fig_ly <- plotly::renderPlotly({
      req(fig_show())
      req(inherits(fig_show(), "plotly"))
      fig_show() |> plotly_config()
    })

    output$fig_ft <- renderUI({
      req(fig_show())
      req(inherits(fig_show(), "flextable"))
      flextable::htmltools_value(fig_show())
    })

    output$fig_dt <- DT::renderDT({
      req(fig_show())
      req(inherits(fig_show(), "datatables"))
      fig_show()
    })

    output$fig_tbl <- renderTable({
      req(fig_show())
      req(is.data.frame(fig_show()))
      fig_show()
    })

    output$fig_rt <- reactable::renderReactable({
      req(fig_show())
      req(inherits(fig_show(), "reactable"))
      fig_show()
    })
  })
}


#' @describeIn m_figure Test app for m_figure module
#' @export
#' 
test_m_figure <- function() {
  library(shiny)
  library(plotly)
  library(ggplot2)
  library(bslib)

  ui <- page_fixed(
    selectInput(
      inputId = "fig_select",
      label = "Select figure:",
      choices = c("Figure 1", "Figure 2", "Figure 3", "DT", "Flextable", "DF"),
    ),
    m_figure_ui("fig_mod")
  )

  server <- function(input, output, session) {
    m_figure_server(
      "fig_mod",
      figures = reactive(
        list(
          `Figure 1` = fig_gg_random(),
          `Figure 2` = fig_gg_random(),
          `Figure 3` = fig_gg_random() |> plotly::ggplotly(),
          `DT` = DT::datatable(head(mtcars)),
          `Flextable` = flextable::flextable(head(mtcars)),
          `DF` = head(mtcars)
        )
      ),
      selected = reactive(input$fig_select)
    )
  }

  shinyApp(ui, server)
}




#' @describeIn m_figure Helper function to generate random gg plot
#' @export
#' 
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


