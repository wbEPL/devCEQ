
#' CEQ progress module that initializes the progress object
#' @export
mod_ceq_progress <-
  function(id,
           run = reactive(NULL),
           prog_length = 4) {
    moduleServer(id, function(input, output, session) {
      eventReactive(run(), {
        fct_make_ceq_progress(session = session, prog_length = prog_length)
      })
    })

  }

#' @describeIn mod_ceq_progress Initialize progress.
fct_make_ceq_progress <- function(session, prog_length = 4) {
  shiny::Progress$new(session = session,
                      min = 0,
                      max = prog_length + 1)
}

#' @describeIn mod_ceq_progress big step progress
#' @export
fct_big_step_ceq_progress <- function(prog,
                                      message = NULL,
                                      detail =  NULL, ...) {
  fct_samll_step_ceq_progress(prog,
                              n_small = 1,
                              message = message,
                              detail =  detail, ...)
}


#' @describeIn mod_ceq_progress small step progress
#' @export
fct_samll_step_ceq_progress <- function(prog,
                                        n_small = 4,
                                        message = NULL,
                                        detail =  NULL, ...) {
  prog$set(message = message,
           detail = detail,
           value = prog$getValue() + 1 / n_small)
}



#' @describeIn mod_ceq_progress small step progress
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
