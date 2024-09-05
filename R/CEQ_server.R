#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @param mod_generic_run_ceq_sim_server function that runs the seq simulation
#'     usually should stay fixed and only its component could be passed into
#'     as separate functions.
#'
#' @inheritParams mod_inputs_btns_server
#' @import shiny
#' @importFrom profvis profvis_server
#' @importFrom dplyr tibble
#' @importFrom readr read_rds
#' @noRd
#' @export
CEQ_server <- function(
    input,
    output,
    session,
    inputs_str = reactive(NULL),
    presim_dta = reactive(list(NULL)),
    inp_str_fn = gen_inp_str,
    ui_gen_fn = gen_tabinp_ui,
    n_policy = c(1, 2, 1),
    n_policy_type = c("numericInline", "numeric", "slider", "dropdown", "none"),
    info_page_md = NULL,
    info_page_size = "l",

    fn_sim_srvr = make_run_sim_server(),

    fn_add_missing_inp = NULL,
    fn_ceq_sim = NULL,
    fn_ceq_pre_postsim = NULL,

    fn_postsim_srvr = mod_generic_run_postsim_server,
    fn_ceq_postsim = function(x, ...) x,
    fn_res_disp_srvr = fn_results_display_server_dummy,
    ...) {

  # # Loading underlining data for 2022 simulation
  inputs_str_ui <-
    inputs_str %>%
    filter(if_any(any_of("include"), ~ .x)) %>%
    select(-any_of("include"))

  inps_all <- inputs_str %>% get_all_inps()

  # Info-page, guides and blackouts ==========================================
  active_tab <- reactive(NULL)
  active_tab <- mod_info_page_server(first_tab = "pc2019",
                                     how_to_tab = "howto",
                                     info_page_md = info_page_md,
                                     info_page_size = info_page_size,
                                     ui_ns = NS("generic_inputs"))

  # Inputs UI server =========================================================
  ceq_inputs <- mod_inputs_server(
    'generic_inputs',
    inp_raw_str = inputs_str_ui,
    inp_str_fn = inp_str_fn,
    ui_gen_fn = ui_gen_fn,
    active_tab = active_tab,
    target_tab = "pc2019",
    source_tab = "howto",
    n_policy = n_policy,
    n_policy_type = n_policy_type
  )

  # observeEvent(ceq_inputs$run(), {browser()}, ignoreInit = TRUE)
  # Simulation runner module ==================================================
  ceq_progress <-
    moduleServer(NULL, function(input, output, session) {
      eventReactive(ceq_inputs$run(), {
        fct_make_ceq_progress(session = session, 3)
      })
    })

  sim_results <-
    fn_sim_srvr(
      id = 'ceqsim',
      run = ceq_inputs$run,
      inps = ceq_inputs$key,
      presim = presim_dta,
      all_inps = reactive(inps_all),
      ceq_progress = ceq_progress,
      fn_add_missing_inp = fn_add_missing_inp,
      fn_ceq_sim = fn_ceq_sim,
      fn_ceq_pre_postsim = fn_ceq_pre_postsim,
      ...
    )

  # Post simulation ===========================================================
  postsim_results <-
    fn_postsim_srvr(
      id = 'ceqsim',
      sim_res = sim_results,
      fn_ceq_postsim = fn_ceq_postsim,
      ceq_progress = ceq_progress
    )

  # Results display ==================================================
  fn_res_disp_srvr(
    id = "ceqsim",
    sim_res = sim_results,
    postsim_res = postsim_results,
    ceq_progress = ceq_progress,
    active_tab = active_tab,
    presim = presim_dta,
    ...
  )

  # Dev results display  ==================================================
  mod_dev_res_server("devres", sim_results)

  # # Browser for the button ================================================
  mod_browser_button_server(
    NULL,
    sim_results = sim_results,
    postsim_results = postsim_results,
    ceq_inputs = ceq_inputs
  )

  # observe({
  #   req(input$browser)
  #   browser()
  # })

  # Observer to make things run
  reactive({
    list(
      ceq_inputs$run,
      ceq_inputs$key,
      sim_results(),
      postsim_results
    )
  })



}

#' dummy simulaiton run function
#' @noRd
fn_simrun_server_dummy <-
  function(id = "sim_id",
           run = reactive(1),
           inps = reactive(list()),
           presim = reactive(list()),
           all_inps = reactive(list()),
           ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # observe({
      #   run()
      #   browser()
      # })
      reactive({
        req(run())
        list(
          id = id,
          run = run(),
          inps = inps(),
          presim = presim(),
          # all_inps = all_inps(),
          result = "fn_simrun_server_dummy() result"
        )
      })
    })
  }


#' dummy post-simulation run function
#' @noRd
fn_postsimrun_server_dummy <-
  function(id = "sim_id",
           sim_res = reactive(list()),
           ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      reactive({
        req(sim_res())
        sim_res()
      })
    })
  }

#' dummy results visualization
#' @noRd
fn_results_display_server_dummy <-
  function(id = "sim_id",
           sim_res = reactive(list()),
           postsim_res = reactive(list()),
           ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    observe({
      req(postsim_res)
      postsim_res
    })
  })

}




# Generic internal CEQ runner  --------------------------------------------




# Progress-related --------------------------------------------------------

