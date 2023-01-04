
#' Return a function that extracts inputs frmo the inputs list do not returning NAs
#'
#' @param actual_inps is the inputs list with the inputs
#' @export
make_get_inp_fn <-
  function(actual_inps) {
    # browser()
    function(inp_nm) {
      if (!is.null(actual_inps[[inp_nm]]))
        return(actual_inps[[inp_nm]])
      else
        return(NA_real_)
    }
  }


#' Create a data frame with the tax brakes
#'
#' @description Requires two vectors of equal length. One with the upper tax thresholds and
#' another one with the the tax rates
#'
#' @export
#' @importFrom tidyr replace_na drop_na nest
#' @importFrom dplyr mutate ungroup group_by filter row_number lead lag
make_tax_brackets <- function(thresholds, rates) {
  thresholds[is.null(thresholds)] <- NA_real_
  rates[is.null(rates)] <- NA_real_

  if (length(thresholds) != length(rates))
    stop("Vectors of income thresholds and tax rates have different length")

  tibble(lower_th = thresholds, rate = rates) %>%
    # We drop all NA provided thresholds and rates and duplicating lower thresholds
    tidyr::drop_na() %>%
    dplyr::group_by(lower_th) %>%
    dplyr::filter(dplyr::row_number() == max(dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      upper_th = dplyr::lead(lower_th),
      upper_th = ifelse(is.na(upper_th), Inf, upper_th),
      tax_lag = (upper_th - lower_th) * rate,
      tax_lag = dplyr::lag(cumsum(tax_lag))
    ) %>%
    tidyr::replace_na(list(tax_lag = 0))
}



#' Apply tax brackets to the data and impute new tax variable
#'
#' @param dta original data set to compute tax levels
#' @param base_var string variable name that should be used used as income
#' @param rules data frame with tax brackets rules returned with `make_tax_brackets`
#' @param new_var string of the new variable to create based on the formula
#'     cum_tax + (base_var - lower) * rate
#' @param calc_drop logical TRUE as default to compute the tax level and drop
#'     all auxiliary variables
#' @param low_thld_rule rule for comparing income to the lower threshold.
#'     usually `>` (default) or `>`.
#' @param up_thld_rule rule for comparing income to the upper threshold.
#'     usually `<=`  (default) or `<`.
#'
#' @export
#' @importFrom purrr reduce
#' @importFrom dplyr mutate if_else
#'
match_tax_brackets <-
  function(dta,
           base_var,
           rules,
           new_var,
           calc_drop = TRUE,
           low_thld_rule = `>`,
           up_thld_rule = `<=`) {
    rules <-
      rules %>% group_by(row_number()) %>% tidyr::nest() %>% pull(data)
    base_var <- sym(base_var)
    out <-
      dta %>%
      dplyr::mutate(lower = NA_real_,
                    rate = NA_real_,
                    cum_tax = NA_real_) %>%
      list() %>%
      append(rules) %>%
      purrr::reduce( ~ {
        .x %>%
          dplyr::mutate(
            is_threshold = FALSE,
            is_threshold = low_thld_rule({
              {
                base_var
              }
            }, .y$lower_th) & up_thld_rule({
              {
                base_var
              }
            }, .y$upper_th),
            lower = dplyr::if_else(is_threshold, .y$lower_th, lower, lower),
            rate = dplyr::if_else(is_threshold, .y$rate, rate, rate),
            cum_tax = dplyr::if_else(is_threshold, .y$tax_lag, cum_tax, cum_tax)
          )
      }) %>%
      select(-is_threshold)

    if (calc_drop) {
      new_var <- sym(new_var)
      out <- out %>%
        mutate(!!new_var := cum_tax + (!!base_var - lower) * rate) %>%
        replace_na(list(new_var = 0)) %>%
        drop_tax_brackets_var()
    }

    out
  }

#' @describeIn match_tax_brackets drop auxiliary variables after `match_tax_brackets`
#' @noRd
#' @importFrom dplyr select any_of
drop_tax_brackets_var <- function(dta) {
  dta %>% select(-any_of(c(
    "is_threshold", "lower", "rate", "cum_tax"
  )))
}
