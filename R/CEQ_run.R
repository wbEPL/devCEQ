#' @title Run the Shiny Application
#'
#' @param inputs_str UI inputs structure that is read with `load_input_xlsx` or
#'   another custom function.
#' @param presim reactive with the list of pre-simulation parameters relevant
#'   to the analysis
#' @param ... arguments to pass to golem_opts.
#'
#' @inheritParams mod_inputs_btns_server
#' @inheritParams CEQ_ui
#'
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
  n_policy_type = c("numericInline", "numeric", "slider", "dropdown", "none"),
  server_fn = CEQ_server,
  ui_fn = ceq_ui_new,
  inp_str_fn = gen_inp_str,
  ui_gen_fn = gen_tabinp_ui,
  ceq_fn = function(inps, presim) {tibble(var = "Results from `CEQ_run`")},
  info_page_md = NULL,
  info_page_size = "l",
  fn_sim_srvr = make_run_sim_server(),
  fn_add_missing_inp = NULL,
  fn_ceq_sim = NULL,
  fn_ceq_pre_postsim = NULL,
  fn_postsim_srvr = mod_generic_run_postsim_server,
  fn_ceq_postsim = function(x, ...) x,
  fn_res_disp_srvr = fn_results_display_server_dummy,
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

            # key functions simulation
            fn_sim_srvr = fn_sim_srvr,
            fn_add_missing_inp = fn_add_missing_inp, # Not important
            fn_ceq_sim = fn_ceq_sim, # Not important
            fn_ceq_pre_postsim = fn_ceq_pre_postsim, # Not important
            fn_postsim_srvr = fn_postsim_srvr,
            fn_ceq_postsim = fn_ceq_postsim, # Not important
            fn_res_disp_srvr = fn_res_disp_srvr, # Not important
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


#' @describeIn CEQ_run Run example shiny app with an elementary CEQ analysis
#' @param ... not used
#' @importFrom shiny runApp
#' @export
CEQ_run_example <-
  function(...) {
    system.file("examples", "ceq_example_simple", package = "devCEQ") |>  shiny::runApp()
  }
