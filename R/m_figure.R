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
#' @export
#' 
m_figure_server <- function(
  id,
  figures = reactive(
    list(`Figure 1` = fig_gg_random(), `Figure 1` = fig_gg_random())
  ),
  selected = reactive(
    "Figure 1"
  ),
  ...
){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    fig_show <- reactive({      
      figs <- figures()
      sel <- selected()
      if (!is.null(figs) && sel %in% names(figs)) {
        figs[[sel]]
      } else {
        figures()[[1]]
      }
    })

    # identify the class of the figure and create the appropriate rendering function
    last_fig_class <- reactiveVal(NULL)
    observe({
      req(fig_show())
      if (inherits(fig_show(), "ggplot") && !identical(last_fig_class(), "ggplot")) {
        isolate(last_fig_class("ggplot"))
      }
      if (inherits(fig_show(), "plotly")) {
        isolate(last_fig_class("plotly"))
      }
    })

    # Generating UI for figure output
    fn_figui <- reactive({
      req(last_fig_class())
      switch(
        last_fig_class(),
        "ggplot" = plotOutput(ns("fig_gg")) |> shinycssloaders::withSpinner(),
        "plotly" = plotly::plotlyOutput(ns("fig_ly"))  |> shinycssloaders::withSpinner()
      )
    })

    output$figure_ui <- renderUI(fn_figui())

    # Rendering figure based on its class
    output$fig_gg <- renderPlot({
      req(fig_show())
      req(inherits(fig_show(), "ggplot"))
      fig_show()
    })

    output$fig_ly <- plotly::renderPlotly({
      req(fig_show())
      req(inherits(fig_show(), "plotly"))
      fig_show() %>%
        plotly_config()
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
    m_figure_ui("fig_mod"),
    selectInput(
      inputId = "fig_select",
      label = "Select figure:",
      choices = c("Figure 1", "Figure 2")
    )
  )

  server <- function(input, output, session) {
    m_figure_server(
      "fig_mod",
      figures = reactive(
        list(
          `Figure 1` = fig_gg_random(),
          `Figure 2` = fig_gg_random() |> plotly::ggplotly()
        )
      ),
      selected = reactive(input$fig_select)
    )
  }

  shinyApp(ui, server)
}




#' @descibeIn m_figure Helper function to generate random gg plot
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


