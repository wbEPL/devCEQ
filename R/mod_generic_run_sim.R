#' @describeIn make_run_sim_server Generic CEQ runner server that uses
#'   CEQ functions to run the analysis based on the `persim` and `inp` args.
#'
#' @param run reactive invalidation variable that signals to run the simulation.
#' @param inps reactive list#'
#' @param presim reactive with pre-simulation data
#' @param all_inps reactive with a complete list of inputs that is passed to
#'     `fn_add_missing_inp`.
#' @param fn_add_missing_inp a function that takes two vectors with pol. ch. in
#'   (`inps` and `all_inps`) and adds missing elements to the first pol. ch.
#'   by name.#'
#' @param fn_ceq_sim a function with two inputs: `inps` and `presim`. It must
#'   return a single data frame with the simulation results.
#' @param ceq_progress is reactive tha contains progress function created
#'   with the `fct_make_ceq_progress`
#'
#' @param fn_ceq_pre_postsim a function that performs some of the post-simulation
#'   calculations that is relevant to perform at this stage. This, for
#'   example could be aggregating deciles or creating a bulk of default plots.
#'
#' @description It makes sure that if the inputs to the policy choices did not
#'   change or the policy choice are identical to the baseline, the simulation
#'   is not being executed again and again.
#'
#' @return a `reactiveVal` object that contains a list of data sets with
#'   simulation results, pre-simulations and post simulation data. It is not
#'   meant to save space but rather hold all the information in one object.
#'   Such results object is a list that consists of: `run`, `policy_sim_raw`,
#'   `run_timestamp` and `policy_sim_agg`. All of it is passed to post-sim
#'   module.
#'
#' @importFrom shinyWidgets show_alert
#' @importFrom purrr imap
#'
#' @export
mod_generic_run_sim_server <-
  function(id,
           run = reactive(NULL),
           presim = reactive(NULL),
           inps = reactive(NULL),
           all_inps = reactive(NULL),
           ceq_progress = reactive(NULL),
           fn_add_missing_inp = add_missing_inp_generic,
           fn_ceq_sim = ceq_sim_generic,
           fn_ceq_pre_postsim = ceq_pre_postsim_generic,
           ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      sim_res <- reactiveVal(NULL)
      last_res <- reactiveVal(list(NULL))

      # Add missing policy choices =========================================
      inps_local <-
        reactive({
          inps() %>%
            purrr::map( ~ {
              .x$policy_choices <-
                fn_add_missing_inp(.x$policy_choices, all_inps())
              .x
            })
        },
        label = "ceq-add-missing-inputs")

      # Run simulations =====================================================
      observeEvent( #
        run(),
        {
          req(run())
          req(inps_local())
          req(length(inps_local()) > 0)

          # Simulation progress --- --- --- --- --- --- --- --- ---
          ceq_progress()$set(value = 0)
          fct_big_step_ceq_progress(
            ceq_progress(),
            message = "Step 1/3: Pre-simulation",
            detail = "Loading pre-simulation data"
          )

          prog_upd <- function(detail) {
            fct_samll_step_ceq_progress(
              ceq_progress(),
              n_small = length(inps_local()) + 1,
              message = "Step 2/3: CEQ simulaitons:",
              detail = detail
            )
          }

          # Loading presim --- --- --- --- --- --- --- --- ---
          req(presim())
          prog_upd("")

          # Running the simulation --- --- --- --- --- --- ---
          inps_local() %>%
            purrr::imap(~ {
              last <- last_res()[[.y]]
              .x$run <- TRUE

              if (isTruthy(last)) {
                same_policy <-
                  all.equal(.x$policy_choices,
                    last$policy_choices,
                    check.attributes = FALSE
                  ) %>%
                  isTRUE() %>%
                  all()

                if (same_policy) {
                  .x$run <- FALSE
                  .x$policy_sim_raw <- last$policy_sim_raw
                  .x$run_timestamp <- last$run_timestamp
                  .x$policy_sim_agg <- last$policy_sim_agg
                }
              }

              .x
            }) %>%
            purrr::map(~ {

              # Update sim progress --- --- --- --- --- --- --- --- ---
              prog_upd(.x$policy_name)

              # # Run simulation --- --- --- --- --- --- --- --- --- ---

              # # Skip running if it is like base.
              if (.x$run && .x$policy_as_base) {
                .x$policy_sim_raw <- presim()$bl_res
                .x$policy_sim_agg <- .x$policy_sim_raw %>% fn_ceq_pre_postsim()
                .x$run_timestamp <- Sys.time()
              }

              # Run if it is not like base
              if (.x$run && !.x$policy_as_base) {
                .x$policy_sim_raw <- # "run_2019_sim"
                  fn_ceq_sim(inp = .x$policy_choices, presim = presim())
                .x$policy_sim_agg <- .x$policy_sim_raw %>% fn_ceq_pre_postsim()
                .x$run_timestamp <- Sys.time()
              } else {
                # not applicable
              }

              .x
            }) %>%
            sim_res()
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE,
        label = "ceq-runing the sim and postsim"
      )

      observeEvent(sim_res(),
        {
          sim_res() %>% last_res()
        },
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

      sim_res
    })
  }

#' User-side functional for customizing the run-sim server with own functions
#'
#' @inheritParams mod_generic_run_sim_server
#' @returns a function which need to be supplied with arguments of the
#'   `mod_generic_run_sim_server` function such as `id`, `run`, `presim`,
#'   `inps`, `all_inps`, `ceq_progress`.
#' @importFrom rlang dots_list
#' @export
make_run_sim_server <-
  function(
    fn_add_missing_inp = add_missing_inp_generic,
    fn_ceq_sim = ceq_sim_generic,
    fn_ceq_pre_postsim = ceq_pre_postsim_generic) {
    function(id,
             run,
             presim,
             inps,
             all_inps,
             ceq_progress,
             ...)
      mod_generic_run_sim_server(
        id,
        run = run,
        presim = presim,
        inps = inps,
        all_inps = all_inps,
        ceq_progress = ceq_progress,
        fn_add_missing_inp = fn_add_missing_inp,
        fn_ceq_sim = fn_ceq_sim,
        fn_ceq_pre_postsim = fn_ceq_pre_postsim,
        ...
      )
  }



# Generic functions ---------------------------------------------------------


#' @describeIn make_run_sim_server inputs that are missing to the CEQ inputs list.
#'
#' @export
add_missing_inp_generic <-
  function(inps, all_inps, ...) {
    all_inps %>%
      `[`(!names(.) %in% names(inps)) %>%
      prepend(inps)
  }


#' @describeIn make_run_sim_server generic function for CEQ simulation
#'
#' @importFrom rlang dots_list
ceq_sim_generic <- function(inp, presim, ...) {
  list(inp = inp,
       presim = presim,
       dots = rlang::dots_list(...))
}


#' @describeIn make_run_sim_server generic function for CEQ pre-postsim
#'
#' @importFrom rlang dots_list
ceq_pre_postsim_generic <- function(x, ...) {
  list(x = x,
       dots = rlang::dots_list(...))
}


# Testers -----------------------------------------------------------------

#' Simpole app for testing module `mod_generic_run_sim_server`
#'   with the user-defined logic
#'
#' @export
test_mod_run_sim <- function(
    id = NULL,
    fn_sim_srvr = make_run_sim_server(),
    inps = reactive(list(
      policy0 = list(policy_choices = list(a = 1, b = 2),
                     policy_as_base = FALSE,
                     policy_name = "Test 1"),
      policy1 = list(policy_choices = list(a = 3, b = 4),
                     policy_as_base = FALSE,
                     policy_name = "Test 2")
    )),
    all_inps = reactive(list(
      a = 1,
      b = 2,
      c = 3,
      d = 3
    )),
    presim = reactive(tibble(presim = c(5:9)))
    ) {

  ui <- fluidPage(
    actionButton("run", "Run simulation"),
    actionButton("save", "Save simulation results to data-test"),
    verbatimTextOutput("inpus_structure"),
    verbatimTextOutput("results_structure"),
    mod_dev_res_ui("dev-res")
  )

  srv <- function(input, output, session) {

    # inps = reactive(list(
    #   policy0 = list(policy_choices = list(a = 1, b = 2),
    #                  policy_as_base = FALSE,
    #                  policy_name = "Test 1"),
    #   policy1 = list(policy_choices = list(a = 3, b = 4),
    #                  policy_as_base = FALSE,
    #                  policy_name = "Test 2")
    # ))
    #
    # all_inps = reactive(list(
    #   a = 1,
    #   b = 2,
    #   c = 3,
    #   d = 3
    # ))
    run <- reactive(input$run)
    # presim <- reactive(tibble(presim = c(5:9)))
    ceq_progress <- mod_ceq_progress(NULL, run = run, 2)

    output$inpus_structure <- renderPrint({
      list(
        run = run(),
        inps = inps(),
        all_inps = all_inps(),
        presim = presim(),
        ceq_progress = ceq_progress()
      ) %>%
        str(max.level = 2)
    })

    sim_results <-
      fn_sim_srvr(
        id = NULL,
        run = run,
        inps = inps,
        presim = presim,
        all_inps = all_inps,
        ceq_progress = ceq_progress
      )

    mod_dev_res_server("dev-res", sim_results)

    observe({
      req(sim_results())
      isolate({
        fct_close_ceq_progress(ceq_progress())
      })
    })

    observeEvent(
      input$save,
      {
        req(sim_results())
        dir.create("data-test", showWarnings = FALSE)
        write_rds(sim_results(), "data-test/full_sim_results.rds")
      }
    )

    output$results_structure <- renderPrint({
      sim_results() %>% str(max.level = 2)
    })

  }

  shinyApp(ui, server = srv)
}

