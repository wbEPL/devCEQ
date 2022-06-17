##' @noRd
##' @export
testrun_input_ui_generation <- 
  function(str_path = NULL) {
    if (is.null(str_path)) {
      inp_raw_str <-inp_str_test_dta
    } else {
      inp_raw_str <- 
        str_path %>%
        load_input_xlsx() 
    }
    options(ceq_dev = TRUE)
    options(scipen = 16)
    
    out_ui <-
      gen_inp_str(inp_raw_str, 2) %>%
      gen_inp_ui(type = "fluid")
    
    server <- function(input, output, session) {}
    
    fluidPage(column(2, wellPanel(numericInput("111", "exmpl", 1))),
              column(10, out_ui$ui)) %>%
      shinyApp(., server)
  }


##' @noRd
##' @export
testrun_input_ui_page <-
  function(str_path = NULL,
           mod_name = NULL) {
    if (is.null(str_path)) {
      inp_raw_str <- inp_str_test_dta
    } else {
      inp_raw_str <-
        str_path %>%
        load_input_xlsx()
    }
    options(ceq_dev = FALSE)
    options(scipen = 16)
    
    server <- function(input, output, session) {
      run_inputs <-
        mod_inputs_server(mod_name,
                          inp_raw_str,
                          inp_str_fn = gen_inp_str,
                          ui_gen_fn = gen_inp_ui)
    }
    
    fluidPage(
      shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
      mod_inputs_ui_wrapper(mod_name)
    ) %>%
      shinyApp(., server)
  }

