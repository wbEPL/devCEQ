#' Generic post-simulation runner server
#'
#' @param sim_res reactive containing results of the `mod_generic_run_sim_server`
#' @inheritParams mod_generic_run_sim_server
#' @return a `reactiveValues` object that contains a list of data sets with
#'   post simulation results that are usually can be directly plotted.
#'
#' @param fn_ceq_postsim a function that performs post-simulation
#'   calculations that is relevant to perform at this stage. This, for
#'   example could be aggregating deciles or creating a bulk of default plots.
#'   it accepts two arguments `sim_res()` and `progress_tick`, which is the
#'   function that ticks progress.
#'
#' @importFrom shinyWidgets show_alert
#' @importFrom purrr imap iwalk
#'
#' @export
mod_generic_run_postsim_server <-
  function(
    id,
    sim_res = reactive(NULL),
    ceq_progress = NULL,
    fn_ceq_postsim = function(x) {x},
    ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      postsim <- reactiveValues()

      # Run POST-simulations =====================================================
      observeEvent(#
        sim_res(), {
          req(sim_res())

          # Post-Simulation progress --- --- --- --- --- --- --- --- ---
          prog_upd <- function(detail) {
            fct_samll_step_ceq_progress(
              ceq_progress(),
              n_small = 4,
              message = 'Step 3/3: Post-simulation',
              detail = detail
            )
          }

          prog_upd(" ")

          ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
          # Here is a potential window for optimization.
          # We may selectively update post-simulation data only for some
          # simulation and not for those that did not change.
          ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
          sim_res() %>%
            fn_ceq_postsim(progress_tick = prog_upd) %>%
            iwalk(~{
              postsim[[.y]] <<- .x
            })

          # Close progress      --- --- --- --- --- --- --- --- ---
          fct_close_ceq_progress(ceq_progress())
        },
        ignoreInit = FALSE,
        ignoreNULL = TRUE,
        label = "ceq19_post-sim_run")

      postsim
    })
  }
