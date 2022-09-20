#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @inheritParams mod_inputs_btns_server
#' @import shiny
#' @importFrom profvis profvis_server
#' @importFrom dplyr tibble
#' @importFrom readr read_rds
#' @noRd
#' @export
CEQ_server <- function(input, output, session,
                       inputs_str = reactive(NULL),
                       presim_dta = reactive(list(NULL)),
                       inp_str_fn = gen_inp_str,
                       ui_gen_fn = gen_tabinp_ui,
                       n_policy = c(1, 2, 1),
                       n_policy_type = c("numericInline", "numeric", "slider", "dropdown", "none"),
                       info_page_md = NULL,
                       info_page_size = "l",
                       fn_sim_srvr = fn_simrun_server_dummy,
                       fn_postsim_srvr = fn_postsimrun_server_dummy,
                       fn_res_disp_srvr = fn_results_display_server_dummy,
                       ...) {

  # # Loading underlining data for 2022 simulation
  inputs_str_ui <-
    inputs_str %>%
    filter(if_any(any_of("include"), ~ .x)) %>%
    select(-any_of("include"))

  inps_all <-
    inputs_str %>%
    mutate(base_value = base_value * factor) %>%
    select(inputId, base_value) %>%
    pmap(~ set_names(list(.y), .x)) %>%
    unlist(recursive = F)

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
      ceq_progress = ceq_progress
    )

  # Post simulation ===========================================================
  postsim_results <-
    fn_postsim_srvr(
      id = 'ceqsim',
      sim_res = sim_results,
      ceq_progress = ceq_progress
    )

  # Results display ==================================================
  fn_res_disp_srvr(
    id = "ceqsim",
    sim_res = sim_results,
    postsim_res = postsim_results,
    ceq_progress = ceq_progress
  )

  # Observer to make things run
  observe({
    ceq_inputs$run
    ceq_inputs$key
    sim_results()
    postsim_results
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
          all_inps = all_inps(),
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
      req(postsim_res())
      postsim_res()
    })
  })

}

#' Generic CEQ runner server that uses CEQ functions to run the analysis per simulation.
#'
#' @description It makes sure that if the inputs to the policy choices did not
#'   change or the policy choice are identical to the baseline, the simulation
#'   is not being executed again and again.
#'
#' @param ceq_fn a function with two inputs: `inps` and `presim`. It must return
#'   a single data frame with the simulation results.
#'
#' @param misspolicy_fn a function that takes two vectors with policy choices in
#'   (`inps` and `all_inps`) and adds missing elements to the first policy chice
#'   by name.
#'
#'
#' @importFrom shinyWidgets show_alert
#' @export
mod_runceq_server <-
  function(id,
           run = reactive(NULL),
           inps = reactive(NULL),
           presim = reactive(NULL),
           baseline = tibble(var = "Baseline default"),
           all_inps = reactive(NULL),
           ceq_fn = function(inps, presim) {tibble(var = "`ceq_fn` results default")},
           misspolicy_fn = devCEQ::add_missing_inp_generic ,
           ...) {

    moduleServer(id, function(input, output, session) {

      ns <- session$ns
      sim_res <- reactiveVal(NULL)
      last_res <- reactiveVal(list(NULL))

      # Add missing policy choices =========================================
      inps_local <-
        reactive({
          inps() %>%
            map(~{
              .x$policy_choices <-
                add_missing_inp(inps = .x$policy_choices, all_inps = all_inps())
              .x
            })
        },
        label = "ceq19_run-add-inps")

      # Run simulations =====================================================
      observeEvent(#
        run(),
        {
          req(run())
          req(inps_local())
          req(length(inps_local()) > 0)

          # Simulation progress --- --- --- --- --- --- --- --- ---
          sim_prog <-
            shiny::Progress$
            new(session,
                min = 0,
                max = length(inps_local()) + 1
            )

          prog_upd <-
            function(policy = NULL) {
              sim_prog$inc(amount = 1, detail = policy)
            }

          # Loading presim --- --- --- --- --- --- --- --- ---
          sim_prog$set(
            message = 'Loading pre-simulation data',
            detail = "Some data files are large",
            value = 0.5
          )
          req(presim())
          sim_prog$set(
            message = 'Running CEQ simulaitons:',
            detail = " ",
            value = 0.75
          )

          # Running the simulation --- --- --- --- --- --- ---
          inps_local() %>%
            # First check if the policy identical to the previous run.
            imap(~ {
              last <- last_res()[[.y]]
              .x$run <- TRUE
              if (isTruthy(last)) {
                same_policy <-
                  all.equal(.x$policy_choices, last$policy_choices,
                            check.attributes = FALSE) %>%
                  isTRUE() %>%
                  all()

                if (same_policy) {
                  .x$run <- FALSE
                  .x$policy_sim_raw <- last$policy_sim_raw
                  .x$run_timestamp <- last$run_timestamp
                }
              }
              .x
            }) %>%

            map(~{
              # Update sim progress --- --- --- --- --- --- --- --- ---
              prog_upd(.x$policy_name)

              # # Run simulation --- --- --- --- --- --- --- --- --- ---
              # # Skip running if it is like base.
              if (.x$run && .x$policy_as_base) {
                .x$policy_sim_raw <- baseline()
                .x$run_timestamp <- Sys.time()
              }

              # Run if it is not like base
              if (.x$run && !.x$policy_as_base) {
                .x$policy_sim_raw <- ceq_fn(inp = .x$policy_choices, presim = presim())
                .x$run_timestamp <- Sys.time()
              } else {
                # browser()
              }
              .x
            }) %>%
            sim_res()

          sim_prog$close()

          # Simulation completed alert --- --- --- --- --- --- --- --- --- ---
          shinyWidgets::show_alert(
            title = "Simulation is completed",
            type = "success"
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE,
        label = "CEQ SIMULATION RUN")

      # Saving recent run to the last run.
      observeEvent(#
        sim_res(), {
          sim_res() %>% last_res()
        },
        ignoreInit = TRUE,
        ignoreNULL = TRUE)

      sim_res
    })
  }



#' @describeIn mod_runceq_server Initialize progress.
fct_make_ceq_progress <- function(session, prog_length = 4) {
  shiny::Progress$new(session = session,
                      min = 0,
                      max = prog_length + 1)
}



#' @describeIn mod_runceq_server big step progress
#' @export
fct_big_step_ceq_progress <- function(prog,
                                      message = NULL,
                                      detail =  NULL, ...) {
  fct_samll_step_ceq_progress(prog,
                              n_small = 1,
                              message = message,
                              detail =  detail, ...)
}


#' @describeIn mod_runceq_server small step progress
#' @export
fct_samll_step_ceq_progress <- function(prog,
                                        n_small = 4,
                                        message = NULL,
                                        detail =  NULL, ...) {
  prog$set(message = message,
           detail = detail,
           value = prog$getValue() + 1 / n_small)
}



#' @describeIn mod_runceq_server small step progress
#' @export
fct_close_ceq_progress <- function(prog,
                                   title = "Simulation is completed",
                                   ...) {
  prog$close()

  # Simulation completed alert --- --- --- --- --- --- --- --- --- ---
  shinyWidgets::show_alert(
    title = "Simulation is completed",
    type = "success",
    ...
  )
}

