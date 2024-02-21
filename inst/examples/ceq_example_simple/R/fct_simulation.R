#' Functions with the simulation logic divided into steps
#' 
step_1_income_tax <-
  function(sim, inps, presim, ...) {
    sim$s1_inc_tx <- presim$inc_tax_raw
    sim
  }

#' Another function
#' 
step_2_vat <-
  function(sim, inps, presim, ...) {
    
    # This is safe inputs wrapper that helps to return NA if the input is not
    # present in the shiny app.
    safe_inp <- devCEQ::make_get_inp_fn(actual_inps = inps)
    
    sim$s2_vat <- presim$inc_tax_raw
    sim
  }

#' final function that combines all simulation components into one.
#' 
step_99_agg_results <- function(sim, inps, presim, ...) {
  
  output <- mtcars
  
  output
}

#' run complete CEQ function
#' @export
#' @noRd
#' @import dplyr purrr
full_ceq <- function(inps, presim, ...) {
  list() |> 
    step_1_income_tax(inps, presim = presim, ...)  |> 
    step_2_vat(inps, presim = presim, ...) |> 
    step_99_agg_results(inps, presim = presim, ...)
}