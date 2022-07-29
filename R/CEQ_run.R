#' Run the Shiny Application
#'
#'
#' @param inputs_str UI inputs structure that is read with `load_input_xlsx` or
#'   another custom function.
#' @param presim reactive with the list of pre-simulation parameters relevant
#'   to the analysis
#'
#' @param choice_type the type of the number of simulation shoices. One of
#'   "slider", "numeric" or "none"
#'
#' @param choice_max maximum number of policy choices in one simulation.
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams CEQ_ui
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
  baseline_dta = reactive(tibble(var = "Baseline from `CEQ_run`")),
  choice_max = 3,
  choice_type = "slider",
  server_fn = CEQ_server,
  ui_fn = CEQ_ui,
  ceq_fn = function(inps, presim) {tibble(var = "Results from `CEQ_run`")},
  ...
) {

  if (max(choice_max) == 1) choice_type <- 'none'

  golem::with_golem_options(
    app = shinyApp(
      ui = ui_fn(choice_type = choice_type, choice_max = choice_max),
      server =
        function(input, output, session, ...) {
          server_fn(inputs_str = inputs_str,
                    presim = presim,
                    baseline_dta = baseline_dta,
                    ceq_fn = ceq_fn,
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
