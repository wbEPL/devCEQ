
#' Calculate N quantiles of a variable
#' 
#' @name deciles
#' 
NULL 

#' @describeIn deciles Calculate deciles/quantiles for specified variables
#' 
#' @param dta A data frame containing the variables to calculate quantiles for
#' @param dec_var Character vector of variable names to calculate quantiles for
#' @param wt_var Character name of the weight variable. If NULL or not found in data, equal weights are used
#' @param n_dec Number of quantiles to create (default: 10 for deciles)
#' 
#' @return The input data frame with additional columns for each variable's quantiles,
#'   named as `{var}_decile_{n_dec}`. Existing decile variables are not recalculated.
#' 
#' @export
f_calc_deciles <- function(
  dta,
  dec_var = NULL,
  wt_var = NULL,
  n_dec = 10
) {
  # Validate inputs
  if (is.null(dec_var) || length(dec_var) == 0) {
    cli::cli_warn("{.arg dec_var} must be a non-empty character vector")
    return(dta)
  }
  
  missing_vars <- dec_var[!dec_var %in% names(dta)]
  if (length(missing_vars) == length(dec_var)) {
    cli::cli_abort("None of {.arg dec_var} variables found in data: {.var {missing_vars}}")
  }
  if (length(missing_vars) > 0) {
    cli::cli_warn("Skipping missing variables: {.var {missing_vars}}")
    dec_var <- setdiff(dec_var, missing_vars)
  }
  
  new_dec_var <- paste0(dec_var, "_decile_", n_dec)
  
  # Check NULL first, then membership
  if (is.null(wt_var) || !wt_var %in% names(dta)) {
    dta <- dta |> mutate(wt_temp__ = 1)
  } else {
    dta <- dta |> mutate(wt_temp__ = !!sym(wt_var))
  }
  
  # Identify which deciles need to be created
  on_dec_var <- dec_var[!(new_dec_var %in% names(dta))]
  
  if (length(on_dec_var) == 0) {
    cli::cli_inform("All requested decile variables already exist")
    return(dta)
  }
  
  dta <-
    dta |>
    calc_deciles(
      dec_var = on_dec_var,
      wt_var = "wt_temp__",
      n_dec = n_dec,
      dec_var_name = str_c("{.col}_decile_", n_dec)
    ) |>
    select(-wt_temp__)
  
  return(dta)
}


#' @describeIn deciles Calculate deciles/quantiles across multiple policy simulations
#' 
#' @param dta_sim A list of simulation results, where each element contains a 
#'   `policy_sim_raw` data frame with microdata
#' @param dec_var Character vector of variable names to calculate quantiles for
#' @param wt_var Character name of the weight variable. If NULL or not found in data, 
#'   equal weights are used
#' @param n_dec Number of quantiles to create (default: 10 for deciles)
#' @param ... Additional arguments (currently unused)
#' 
#' @return A list with the same structure as `dta_sim`, where each simulation's 
#'   `policy_sim_raw` data frame has been augmented with decile columns
#' 
#' @export
f_calc_deciles_by_sim <- function(dta_sim, dec_var, wt_var = NULL, n_dec = 10, ...) {
  dta_sim |>
    purrr::map(
      ~ {
        .x$policy_sim_raw <-
          .x$policy_sim_raw |>
          f_calc_deciles(
            dec_var = dec_var,
            wt_var = wt_var,
            n_dec = n_dec
          )
        .x
      }
    )
}

#' @describeIn deciles Calculate N qualtiles of a variables from the CEQ results
#'
#' @inheritParams get_dta_gini
#' @param var character of a single variable name or an unnamed
#'     character vector of variables created with `c(...)`. These
#'     variables will be used for creating N quantiles. Default
#'     `get_inc_nm()$var` that represents all income concepts.
#' @param n_dec number of quantiles/deciles to create.
#'     Default is 10.
#'
#' @returns the same data frame with addition column (s). One column
#'     per each `var` used for computing deciles. If we provide
#'     `var = "ypf_SA2"` the resulting data frame will contain
#'     additional variable `ypf_SA2_decile`. If we specify a list of
#'     variables, as many additional variables will be created
#'     with extension `_decile` in their name.
#'
#' @export
#' @importFrom forcats as_factor fct_drop
#' @importFrom glue glue
calc_deciles <-
  function(dta,
           dec_var, #get_inc_nm()$var,
           n_dec = 10,
           wt_var = NULL, #get_wt_nm(),
           dec_var_name = "{.col}_decile",
           ...) {

    # Check if any of the dec_var are missing in the data
    missing_vars <- dec_var[!dec_var %in% names(dta)]
    if (length(missing_vars) > 0) {
      cli::cli_warn(
        "Variable(s) {.var {missing_vars}} not found in the data. Skipping these variable(s)."
      )
    }

    if (length(missing_vars) == length(dec_var)) {
      cli::cli_abort(
        "None of the specified {.var {missing_vars}} variables are present in the data."
      )
    }

    if (is.null(wt_var)) {
      warning("`wt_var` was not specified. ",
              "Non-weighted statistics is computed.")
      dta <- mutate(dta, dummy_weighting_variable = 1)
      wt_var <- "dummy_weighting_variable"
    }

    if (!is.null(wt_var) && !wt_var %in% names(dta)) {
      warning("Weighting variable '", wt_var, "' is not present in the `dta`. ",
              "Non-weighted statistics is computed."
      )
      dta <- mutate(dta, dummy_weighting_variable = 1)
      wt_var <- "dummy_weighting_variable"
    }

    wt_var_sym <- sym(wt_var)
    dta %>%
      mutate(
        across(
          any_of(dec_var),
          ~ get_quantiles_stata(., n = n_dec, wt = !!wt_var_sym),
          .names = dec_var_name
        )
      ) %>%
      select(-any_of("dummy_weighting_variable")) %>%
      select(contains("decile"), everything())
  }

#' @describeIn deciles Helper function to get quantiles using statar::xtile
#' @importFrom statar xtile
get_quantiles_stata <- function(x, n = 10, wt = NULL) {
  statar::xtile(x, n = n, wt = wt) |> factor()
}


#' @describeIn deciles Helper function to get quantiles using collapse::fquantile
#' 
#' @importFrom collapse fquantile
get_quantiles <- function(x, n = 10, wt = NULL, labels = NULL, type = 7) {
  probs <- seq(0, 1, length.out = n + 1)
  q <- collapse::fquantile(x, probs = probs, type = type, w = wt)
  cut(x, breaks = q, include.lowest = TRUE, labels = FALSE) |> factor()
}

#' @describeIn deciles Aggregates all relevant CEQ results by variable.
#'
#' @inheritParams calc_deciles
#' @param by_var character name of the grouping variable
#'
#' @export
calc_agg_by <-
  function(dta,
           vars,
           by_var,
           wt_var = get_wt_nm()) {
    
    vars_1 <- vars[vars %in% names(dta)]
    non_numeric_vars <- vars_1[!sapply(dta[vars_1], is.numeric)]
    if (length(non_numeric_vars) > 0) {
      warning("The following non-numeric variables will be excluded from aggregation: ",
              paste(non_numeric_vars, collapse = ", "))
      vars_1 <- setdiff(vars_1, non_numeric_vars)
      vars_1 <- setdiff(vars_1, wt_var)
    }

    if (is.null(wt_var)) {
      warning("`wt_var` was not specified. ",
              "Non-weighted statistics is computed.")
      dta <- mutate(dta, dummy_weighting_variable = 1)
      wt_var <- "dummy_weighting_variable"
    }

    if (!is.null(wt_var) && !wt_var %in% names(dta)) {
      warning("Weighting variable '", wt_var, "' is not present in the `dta`. ",
              "Non-weighted statistics is computed."
      )
      dta <- mutate(dta, dummy_weighting_variable = 1)
      wt_var <- "dummy_weighting_variable"
    }

    by_var_sym <- sym(by_var)
    wt_var_sym <- sym(wt_var)

    dta %>%
      group_by(!!by_var_sym) %>%
      summarise(
        across(any_of(vars_1), ~ sum(. * {{ wt_var_sym }}, na.rm = TRUE))
      ) %>%
      ungroup()
  }



#' @describeIn deciles Aggregate cariables of a single simulaiton by decile
#'
#' @importFrom forcats fct_relevel fct_drop
#' @export
agg_by_deciles <-
  function(dta,
           policy_name = NA_character_,
           dec_by,
           dec_vars,
           n_dec = 10,
           wt_var = NULL,
           get_var_fn = get_var_nm, ...) {
    # policy_name = NA_character_
    # dec_by = "yp_pc"
    # dec_vars =  get_inc_nm()$var
    # wt_var = "pcweight"
    # browser()
    dta %>%
      calc_deciles(dec_var = dec_by,
                   dec_var_name = "Decile",
                   n_dec = n_dec,
                   wt_var = wt_var, ...) %>%
      calc_agg_by(by_var = "Decile",
                  vars = c(dec_by, dec_vars) %>% unique(),
                  wt_var = wt_var) %>%
      pivot_longer(cols = c(everything(), -Decile, -any_of(dec_by)), names_to = "var") %>%
      left_join(get_var_fn(), by = "var") %>%
      mutate(
        var_title2 =
          if_else(is.na(var_title), as.character(var), as.character(var_title)),
        var_title2 =
          factor(var_title2, levels = unique(var_title2)) %>%
          forcats::fct_relevel(levels(get_var_fn(dec_vars)$var_title)),
        var_title = var_title2
        ) %>%
      select(- var_title2) %>%
      mutate(
        Income = get_var_fn(dec_by)$var_title[[1]],
        Simulation = policy_name %>% forcats::fct_drop()) %>%
      select(
        Decile,
        Income_value = {{dec_by}},
        Income,
        Simulation,
        Source = var_title,
        value
      )
  }

#' @describeIn deciles aggregates specific variables from the simulation resutls.
#' @noRd
#' @export
#' @importFrom purrr map
#' @export
agg_sims_by_deciles <-
  function(sim_res,
           dec_by,
           dec_vars,
           get_var_fn = get_var_nm,
           n_dec = 10,
           wt_var = NULL,
           ...)  {
    sim_res %>%
      purrr::map( ~ {
        agg_by_deciles(
          dta = .x$policy_sim_raw,
          policy_name = .x$policy_name,
          dec_by = dec_by,
          dec_vars = dec_vars,
          n_dec = n_dec,
          wt_var = wt_var,
          get_var_fn = get_var_fn,
          ...
        )
      })
  }

