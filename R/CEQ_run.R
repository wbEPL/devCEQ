#' Run the Shiny Application
#'
#'
#' @param inputs_str UI inputs structure that is read with `load_input_xlsx` or
#'   another custom function.
#' @param presim reactive with the list of pre-simulation parameters relevant
#'   to the analysis
#' @param ... arguments to pass to golem_opts.
#' @inheritParams mod_inputs_btns_server
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
  n_policy = c(1, 2, 1),
  n_policy_type = c("numericInline", "numeric", "slider", "none"),
  server_fn = CEQ_server,
  ui_fn = CEQ_ui,
  inp_str_fn = gen_inp_str,
  ui_gen_fn = gen_tabinp_ui,
  ceq_fn = function(inps, presim) {tibble(var = "Results from `CEQ_run`")},
  info_page_md = NULL,
  info_page_size = "l",
  ...
) {

  golem::with_golem_options(
    app = shinyApp(
      ui = ui_fn(),
      server =
        function(input, output, session, ...) {
          server_fn(
            inputs_str = inputs_str,
            presim = presim,
            baseline_dta = baseline_dta,
            ceq_fn = ceq_fn,
            inp_str_fn = inp_str_fn,
            ui_gen_fn = ui_gen_fn,
            n_policy = n_policy,
            n_policy_type = n_policy_type,
            info_page_md = info_page_md,
            info_page_size = info_page_size,
            ...
          )
        },
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
