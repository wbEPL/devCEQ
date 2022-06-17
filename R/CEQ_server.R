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
                       presim_dta = leactive(list(NULL)),
                       inp_str_fn = gen_inp_str,
                       ui_gen_fn = gen_inp_ui,
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
