#' run_sim UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_test_input_sim_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' run_sim Server Functions
#'
#' @noRd
mod_test_input_sim_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}




test_input_sim <- function(
    inputs_str,
    presim,
    baseline_dta = reactive(tibble(var = "Baseline from `CEQ_run`")),
    n_policy = c(1, 2, 1),
    n_policy_type = c("numericInline", "numeric", "slider", "dropdown", "none"),

    server_fn = CEQ_server,
    ui_fn = CEQ_ui,

    inp_str_fn = gen_inp_str,
    ui_gen_fn = gen_tabinp_ui,
    ceq_fn = function(inps, presim) {tibble(var = "Results from `CEQ_run`")},

    info_page_md = NULL,
    info_page_size = "l",

    fn_sim_srvr = fn_simrun_server_dummy,
    fn_postsim_srvr = fn_postsimrun_server_dummy,
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
            fn_sim_srvr = fn_sim_srvr,
            fn_postsim_srvr = fn_postsim_srvr,
            fn_res_disp_srvr = fn_res_disp_srvr,
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

## To be copied in the UI
# mod_run_sim_ui("run_sim_1")

## To be copied in the server
# mod_run_sim_server("run_sim_1")
