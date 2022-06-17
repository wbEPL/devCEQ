#' plotly
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom plotly config
plotly_config <- function(plt) {
  plt %>%
    plotly::config(
      displaylogo = FALSE,
      showAxisDragHandles = FALSE,
      modeBarButtonsToRemove = c(
        'pan2d',
        'select2d',
        'lasso2d',
        "toggleSpikelines",
        "resetScale2d"
      )
    )
}
