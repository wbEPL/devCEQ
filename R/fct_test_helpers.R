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
    options(golem.app.prod = FALSE)
    options(scipen = 16)

    out_ui <-
      gen_inp_str(inp_raw_str, 2) %>%
      gen_tabinp_ui(type = "fluid")

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
    options(golem.app.prod = TRUE)
    options(scipen = 16)

    server <- function(input, output, session) {
      run_inputs <-
        mod_inputs_server(mod_name,
                          inp_raw_str,
                          inp_str_fn = gen_inp_str,
                          ui_gen_fn = gen_tabinp_ui)
    }

    fluidPage(
      shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
      mod_inputs_ui_wrapper(mod_name)
    ) %>%
      shinyApp(., server)
  }


#' diffdf for local comparisons
#'
#' @import diffdf diffdf
#' @export
#' @noRd
#'
mydiff <- function(dta, dta_compare, key = c("numind", "hhid"), tollerance = 0.01) {
  dta %>%
    diffdf::diffdf(dta_compare %>% select(any_of(names(dta))),
                   tolerance = tollerance,
                   keys = key)
}


#' diffdf for local comparisons
#'
#' @export
#'
#' @importFrom purrr imap_dfr
#' @noRd
#'
get_diffdf_sum <- function(dta) {
  dta %>%
    purrr::imap_dfr( ~ {
      if (str_detect(.y, "VarDiff")) {
        .x %>% get_MAD()
      }
    })
}


#' diffdf for local comparisons
#'
#' @export
#' @noRd
#'
get_MAD <- function(dta) {
  dta %>%
    dplyr::group_by(VARIABLE) %>%
    # filter()
    dplyr::summarise(
      `n diff` = n(),
      `NA B` = sum(is.na(BASE)),
      `NA C` = sum(is.na(COMPARE)),
      `NA B 0 C` = sum(is.na(BASE) & COMPARE == 0),
      `NA C 0 B` = sum(is.na(COMPARE) & BASE == 0),
      `NA non 0` = sum((is.na(COMPARE) & round(BASE) != 0) | (is.na(BASE) & round(COMPARE) != 0)),
      `Mean ABS deviation` = mean(abs(BASE - COMPARE), na.rm = TRUE),
      `mean(abs(BASE-COMPARE)/BASE)*100` = mean(abs(BASE - COMPARE) / BASE, na.rm = TRUE)*100)
}
