#' Run the Shiny Application
#'
#'
#' @param inputs_str UI inputs structure that is read with `load_input_xlsx` or
#'   another custom function.
#' @param presim reactive with the list of pre-simulation parameters relevant
#'   to the analysis
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @noRd
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
CEQ_run <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  inputs_str,
  presim,
  ...
) {
  golem::with_golem_options(
    app = shinyApp(
      ui = CEQ_ui,
      server =
        function(input, output, session, ...) {
          CEQ_server(inputs_str = inputs_str,
                     presim = presim,
                     ...)
        },
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
