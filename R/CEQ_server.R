#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
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
                       ui_gen_fn = gen_inp_ui,
                       choice_max = 2,
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
                                     how_to_tab = "howto")

  # Inputs UI server =========================================================
  ceq_inputs <- mod_inputs_server(
    'generic_inputs',
    inp_raw_str = inputs_str_ui,
    inp_str_fn = inp_str_fn,
    ui_gen_fn = ui_gen_fn,
    choice_max = choice_max,
    active_tab = active_tab,
    target_tab = "pc2019",
    source_tab = "howto"
  )

  # observeEvent(ceq_inputs$run(), {
  #   browser()
  # }, ignoreInit = TRUE)


  # Analysis runner module ==================================================
  # sim19_results <-
  #   mod_ceq2019_run_server(
  #     id = 'ceq2019',
  #     run = ceq_inputs$run,
  #     inps = ceq_inputs$key,
  #     presim = presim_dta,
  #     all_inps = reactive(inps_all)
  #   )

  # postsim19_results <- mod_ceq2019_postsim_server(id = 'ceq2019', sim19_results)
  observe({
    ceq_inputs$run
    ceq_inputs$key
    # req(postsim19_results())# %>%
    #   # readr::write_rds("data-dev/postsim19_results.rds", compress = "gz")
    # req(sim19_results()) %>%
    #   readr::write_rds("data-dev/sim19_results.rds", compress = "gz")
    # browser()

    })

  # Results display ==================================================
  # mod_ceq2019_results_server(
  #   id = "ceq2019",
  #   sim_res = sim19_results,
  #   postsim_res = postsim19_results
  # )

  # callModule(profvis_server, "profiler")

  observe({
    # ceq_inputs$key()
    # ceq_results$out()
    # browser()
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
#' @noRd
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
           misspolicy_fn = devCEQ::add_missing_inp_generic ) {

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
