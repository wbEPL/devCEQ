#' @param plot_rv A `reactiveValues` with a slot `plot` containing a `ggplot` object.
#'
#' @export
#' @noRd
#'
#' @importFrom shiny moduleServer observeEvent req renderPlot isTruthy
#' @importFrom shinyWidgets updateNumericInputIcon
save_ggplot_server2 <- function(id, plot_rv, dpi = 450, scale = 2) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- session$ns
      plot_width <- paste0("output_", ns("plot"), "_width")
      plot_height <- paste0("output_", ns("plot"), "_height")

      observeEvent(input$hidden, {
        activate_resizer(id = ns("plot"), modal = isTRUE(input$modal))
      })

      observeEvent(input$update_preview, {
        if (isTruthy(input$width) & isTruthy(input$height)) {
          resize(
            id = ns("plot"),
            width = input$width,
            height = input$height
          )
        }
      })
      observeEvent(session$clientData[[plot_width]], {
        updateNumericInputIcon(
          session = session,
          inputId = "width",
          value = session$clientData[[plot_width]]
        )
      })
      observeEvent(session$clientData[[plot_height]], {
        updateNumericInputIcon(
          session = session,
          inputId = "height",
          value = session$clientData[[plot_height]]
        )
      })

      output$plot <- renderPlot({
        req(plot_rv$plot)
        plot_rv$plot
      })

      output$png <- download_plot_rv2(input, plot_rv, "png", dpi = dpi, scale = scale)
      output$pdf <- download_plot_rv2(input, plot_rv, "pdf", dpi = dpi, scale = scale)
      output$bmp <- download_plot_rv2(input, plot_rv, "bmp", dpi = dpi, scale = scale)
      output$svg <- download_plot_rv2(input, plot_rv, "svg", dpi = dpi, scale = scale)
      output$tiff <- download_plot_rv2(input, plot_rv, "tiff", dpi = dpi, scale = scale)
      output$eps <- download_plot_rv2(input, plot_rv, "eps", dpi = dpi, scale = scale)
      output$jpeg <- download_plot_rv2(input, plot_rv, "jpeg", dpi = dpi, scale = scale)

      return(NULL)
    }
  )
}


#' @importFrom shiny downloadHandler
#' @importFrom ggplot2 ggsave
#' @noRd
download_plot_rv2 <- function(input, rv, device, dpi = 150, scale = 1) {
  downloadHandler(
    filename = function() {
      filename <- input$filename
      if (endsWith(filename, paste0("\\.", device)))
        filename
      else
        paste0(filename, ".", device)
    },
    content = function(file) {
      width <- input$width
      height <- input$height
      ggsave(
        filename = file,
        plot = rv$plot,
        device = device,
        dpi = dpi,
        width = width / dpi,
        height = height / dpi,
        scale = scale,
        bg = "white"
      )
    }
  )
}


#' Resizer handlers
#' @noRd
#'
activate_resizer <- function(id,
                             ...,
                             modal = FALSE,
                             container = "body",
                             session = shiny::getDefaultReactiveDomain()) {
  if (isTRUE(modal))
    container <- ".modal-body"
  session$sendCustomMessage("resize", list(
    id = id,
    container = container,
    ...,
    modal = modal
  ))
}


#' Resizer handlers 2
#' @noRd
resize <- function(id,
                   width,
                   height,
                   session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage(paste0("resize-", id), list(
    width = width,
    height = height
  ))
}
