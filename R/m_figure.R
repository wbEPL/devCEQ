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

#' Helper function to generate sample figures for testing
#' @return A named list of sample figures including ggplot, plotly, DT, flextable, reactable, and data frame
#' @export
f_get_sample_figures <- function() {
    list(
      `Figure 1 (gg)` = fig_gg_random(),
      `Figure 3 (ly)` = fig_gg_random() |> plotly::ggplotly(),
      `DT` = DT::datatable(head(mtcars)),
      `Flextable` = flextable::flextable(head(mtcars)),
      `Reactable` = reactable::reactable(head(mtcars)),
      `DF` = head(mtcars)
    )

}

#' Check the type of an object
#'
#' Determines if an object is a data frame, flextable, datatables, reactable,
#' a list of ggplot objects, a list of plotly objects, or other types.
#'
#' @param a An object to check
#'
#' @return A character string indicating the object type:
#'   \itemize{
#'     \item \code{"data.frame"} - A data frame
#'     \item \code{"flextable"} - A flextable object
#'     \item \code{"datatables"} - A DT datatables object
#'     \item \code{"reactable"} - A reactable object
#'     \item \code{"ggplot"} - A single ggplot object
#'     \item \code{"plotly"} - A single plotly object
#'     \item \code{"list_of_ggplot"} - A list where all elements are ggplot objects
#'     \item \code{"list_of_plotly"} - A list where all elements are plotly objects
#'     \item \code{"empty_list"} - An empty list
#'     \item \code{"mixed_list"} - A list with mixed object types
#'     \item \code{"unknown"} - None of the above types
#'   }
#'
#' @export
check_object_type <- function(a) {
  # Check for data frame first
  if (inherits(a, "flextable")) {
    return("flextable")
  }
  if (inherits(a, "datatables")) {
    return("datatables")
  }
  if (inherits(a, "reactable")) {
    return("reactable")
  }
  if (is.data.frame(a) && !inherits(a, c("flextable", "datatables", "reactable"))) {
    return("data.frame")
  }

  # Check for single ggplot or plotly objects
  if (inherits(a, "ggplot") && !is.list(a)) {
    return("ggplot")
  }
  if (inherits(a, "plotly")) {
    return("plotly")
  }

  # Check if it's a list
  if (is.list(a)) {
    if (length(a) == 0) {
      return("empty_list")
    }

    # Check if all elements are ggplot objects
    if (all(sapply(a, function(x) inherits(x, "ggplot")))) {
      return("list_of_ggplot")
    }

    # Check if all elements are plotly objects
    if (all(sapply(a, function(x) inherits(x, "plotly")))) {
      return("list_of_plotly")
    }

    return("mixed_list")
  }

  return("unknown")
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
  figures = reactive(f_get_sample_figures()),
  selected = reactive(names(f_get_sample_figures())[1]),
  force_ly = TRUE,
  ...
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fig_show <- reactive({
      req(figures())
      req(selected())
      figs <- figures()
      sel <- selected()

      # Check if figs is a data frame
      if (
        check_object_type(figs) %in%
          c("data.frame", "flextable", "datatables", "reactable")
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
        out <- plotly::ggplotly(out, tooltip = "text", height = 425) |>
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
        "ggplot" = plotOutput(ns("fig_gg")),
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
      choices = c(
        "Figure 1 (gg)",
        "Figure 2 (gg list)",
        "Figure 3 (ly)",
        "Figure 4 (ly list)",
        "DT",
        "Flextable",
        "Reactable",
        "DF"
      )
    ),
    m_figure_ui("fig_mod")
  )

  server <- function(input, output, session) {
    m_figure_server(
      "fig_mod",
      # figures = reactive(
      #   list(
      #     `Figure 1` = fig_gg_random(),
      #     `Figure 2` = fig_gg_random(),
      #     `Figure 3` = fig_gg_random() |> plotly::ggplotly(),
      #     `DT` = DT::datatable(head(mtcars)),
      #     `Flextable` = flextable::flextable(head(mtcars)),
      #     `DF` = head(mtcars)
      #   )
      # ),
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
