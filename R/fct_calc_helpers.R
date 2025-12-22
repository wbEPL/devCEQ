#' Calculate N quantiles of a variable
#'
#' @name deciles
#'
NULL

#' @describeIn deciles Helper function to get unique values of a variable
#' @export
f_get_var_uniq_vals <- function(dta, var_nm, ...) {
  dta |>
    select(any_of(f_get_colname(var_nm))) |>
    pull(1) |>
    unique() |>
    as.character()
}

#' Format incidence data for presentation with labels and proper ordering
#'
#' @param dta A data frame containing incidence data with columns:
#'   \describe{
#'     \item{measure}{Measure type (character or factor)}
#'     \item{decile_var}{Variable used for decile grouping (character or factor)}
#'     \item{var}{Variable name (character or factor)}
#'   }
#' @param ... Additional arguments (currently unused)
#'
#' @return A formatted tibble with:
#'   \itemize{
#'     \item Measure and variable labels applied from dictionaries
#'     \item All character/factor columns converted to ordered factors
#'     \item Glue-based interpolation applied to measure labels
#'     \item Column names renamed according to the column names dictionary
#'   }
#'
#' @details
#' This function performs a complete formatting pipeline for incidence data:
#' \enumerate{
#'   \item Adds measure labels using \code{f_add_measure_labels()}
#'   \item Adds variable labels using \code{f_add_var_labels()}
#'   \item Adds decile variable labels
#'   \item Groups and nests by measure, decile_var, and var
#'   \item Preserves factor ordering through temporary numeric columns
#'   \item Applies glue interpolation to measure labels (allows dynamic text like "{var}")
#'   \item Restores factor ordering and unnests data
#'   \item Renames columns using \code{f_rename_cols()}
#' }
#'
#' The function is particularly useful for preparing incidence analysis results
#' for tables and visualizations with human-readable labels.
#'
#' @export
f_format_incidence <- function(dta, ...) {
  dta |>
    f_add_measure_labels() |>
    f_add_var_labels() |>
    f_add_var_labels(to_var = "decile_var") |>
    f_add_var_labels(to_var = "group_var") |>
    group_by(measure, decile_var, var) |>
    nest() |>
    ungroup() |>
    # Preserve original factor order
    mutate(
      across(where(is.factor), ~ as.numeric(.), .names = "order_{.col}"),
      across(where(is.factor), as.character)
    ) |>
    mutate(
      measure = purrr::pmap_chr(
        list(.data[["measure"]], .data[["decile_var"]], .data[["var"]]),
        ~ {
          glue(..1, var = str_to_lower(..3), decile_var = str_to_lower(..2))
        }
      )
    ) |>
    mutate(
      across(
        where(is.character),
        ~ as_factor(.x) |> fct_reorder(get(paste0("order_", cur_column())))
      )
    ) |>
    select(-starts_with("order_")) |>
    unnest(cols = c(data)) |>
    arrange(across(any_of(c(
      "sim",
      "var",
      "group_var",
      "decile_var",
      "measure",
      "decile"
    )))) |>
    f_rename_cols()
}

#' Calculate relative and absolute incidence measures
#'
#' @param dta A data frame containing aggregated data with columns:
#'   \describe{
#'     \item{var}{Variable name to calculate incidence for}
#'     \item{level}{Aggregated level/value of the variable}
#'     \item{decile_val}{Sum of the decile variable values (for relative incidence)}
#'     \item{decile_var}{Optional - variable used for decile grouping}
#'     \item{sim}{Optional - simulation identifier}
#'   }
#' @param ... Additional arguments (currently unused)
#'
#' @return A long-format tibble with incidence measures, containing:
#'   \describe{
#'     \item{decile_var}{Decile grouping variable (if present in input)}
#'     \item{sim}{Simulation identifier (if present in input)}
#'     \item{var}{Variable name}
#'     \item{measure}{Type of incidence measure: "relative", "absolute", or "level"}
#'     \item{value}{Calculated incidence value}
#'     \item{decile_val}{Sum of decile values}
#'     \item{n, pop}{Number of observations and population (if present)}
#'   }
#'
#' @details
#' This function calculates three types of incidence measures:
#' \itemize{
#'   \item \strong{Relative incidence}: The level value as a proportion of the decile sum
#'         (\code{level / decile_val})
#'   \item \strong{Absolute incidence}: The level value as a proportion of the variable's
#'         total across all groups (\code{level / total})
#'   \item \strong{Level}: The raw level value multiplied by a factor from the variable
#'         dictionary (typically 1 or -1 for benefits vs. taxes)
#' }
#'
#' The function performs the following steps:
#' \enumerate{
#'   \item Joins with the variable dictionary to get factor values (sign adjustments)
#'   \item Applies the factor to level values (e.g., -1 for taxes)
#'   \item Calculates total values by grouping variables
#'   \item Computes relative and absolute incidence ratios
#'   \item Transforms to long format with measure types as rows
#' }
#'
#' @examples
#' \dontrun{
#' # After aggregating by deciles
#' dec_agg |>
#'   f_calc_incidence()
#' }
#'
#' @export
f_calc_incidence <- function(dta, force_abs = FALSE, ...) {
  dta <-
    dta |>
    left_join(
      get_var_nm() |>
        select(-any_of("var_title")),
      by = c("var" = "var")
    )

  if (!"factor" %in% colnames(dta)) {
    dta <- dta %>% mutate(factor = 1)
  } else {
    dta <- dta |>
      mutate(
        factor = as.numeric(factor),
        factor = ifelse(is.na(factor), NA_real, factor)
      )
  }

  if (!isTRUE(force_abs)) {
    dta <- dta |> mutate(level = level * factor)
  }

  dta |>
    group_by(across(any_of(c("decile_var", "sim", "var")))) |>
    mutate(total = sum(level, na.rm = TRUE)) |>
    ungroup() |>
    mutate(relative = level / decile_val, absolute = level / total) |>
    mutate(
      across(
        c(relative, absolute),
        ~ ifelse(is.nan(.) | is.infinite(.), 0, .)
      )
    ) |>
    select(-total, -factor) |>
    pivot_longer(
      cols = any_of(c("relative", "absolute", "level")),
      names_to = "measure",
      values_to = "value"
    )
}

#' @describeIn deciles Aggregate variables by decile across multiple policy simulations
#'
#' @param dta_sim A list of simulation results, where each element contains:
#'   \describe{
#'     \item{policy_sim_raw}{A data frame with simulation microdata}
#'     \item{policy_name}{Character name of the policy simulation}
#'   }
#' @param var_decile Character vector of decile variable names to aggregate by,
#'   formatted as `{var}___decile`
#' @param var_agg Character vector of variable names to aggregate by weighted sum
#' @param var_group Character vector of grouping variable names for disaggregation (optional)
#' @param wt_var Character name of the weight variable. If NULL or not found,
#'   equal weights of 1 are used
#' @param ... Additional arguments passed to `f_agg_by_decile()`
#'
#' @return A tibble combining aggregations across all simulations, with an
#'   additional `sim` column identifying the policy simulation name. Each row
#'   represents aggregated statistics for one decile group in one simulation.
#'
#' @details
#' This function applies `f_agg_by_decile()` to each simulation in the list and
#' combines results with simulation identifiers. Useful for comparing decile
#' distributions across different policy scenarios.
#'
#' @export
f_agg_by_decile_by_sim <- function(
  dta_sim,
  var_decile,
  var_agg,
  var_group = NULL,
  wt_var = NULL,
  ...
) {
  dta_sim |>
    purrr::map(
      ~ {
        f_agg_by_decile(
          dta = .x$policy_sim_raw,
          var_decile = var_decile,
          var_agg = var_agg,
          wt_var = wt_var,
          var_group = var_group,
          ...
        ) |>
          mutate(sim = .x$policy_name)
      }
    ) |>
    bind_rows()
}

#' @describeIn deciles Aggregate variables by decile
#'
#' @param dta A data frame containing decile variables and variables to aggregate
#' @param var_decile Character string of the decile variable name, formatted as
#'   `{var}___decile___{n}` where `{var}` is the income variable and `{n}` is
#'   the number of deciles/quantiles
#' @param var_agg Character vector of variable names to aggregate by weighted sum
#' @param wt_var Character name of the weight variable. If NULL or not found,
#'   equal weights of 1 are used
#' @param ... Additional arguments (currently unused)
#'
#' @return A tibble with one row per decile group containing:
#'   \describe{
#'     \item{[decile_var]}{The decile grouping variable (factor)}
#'     \item{n}{Number of observations in each decile}
#'     \item{pop}{Weighted population in each decile}
#'     \item{[var_agg columns]}{Weighted sums for each variable in `var_agg`}
#'     \item{var_by}{The base income variable name extracted from `var_decile`}
#'     \item{deciles_n}{The number of deciles/quantiles used}
#'   }
#'
#' @details
#' This function performs weighted aggregation of variables by decile groups.
#' The decile variable name is parsed to extract metadata about which income
#' variable was used to create the deciles and how many quantile groups were formed.
#'
#' @examples
#' \dontrun{
#' f_agg_by_decile(
#'   dta = microdata,
#'   var_decile = "ym___decile",
#'   var_agg = c("ym", "dtx_prog1", "dtx_prog2"),
#'   wt_var = "hhwt"
#' )
#' }
f_agg_by_decile_one <- function(
  dta,
  var_decile,
  var_agg,
  var_group = NULL,
  wt_var = NULL,
  ...
) {
  # Check if var_decile exists in dta
  if (!var_decile %in% names(dta)) {
    cli::cli_abort(
      "Decile variable {.var {var_decile}} not found in the data. ",
      "Please ensure that deciles are calculated before aggregation."
    )
  }

  # Parse decile variable name to extract income variable and number of deciles
  dec_income <- var_decile |> stringr::str_split_1("___") |> pluck(1)

  # Check if dec_income is in data
  if (!dec_income %in% names(dta)) {
    cli::cli_warn(
      "Variable {.var {dec_income}} used for decile calculation ",
      "should be present in the data to avoid confusion."
    )
    dta <- dta |> mutate(decile_val = NA_real_)
  } else {
    dta <- dta |> mutate(decile_val = .data[[dec_income]])
  }

  # Check if wt_var exists in dta and impute weight 1 if not
  if (is.null(wt_var) || !wt_var %in% names(dta)) {
    dta <- dta |> mutate(wt_local__ = 1)
  } else {
    dta <- dta |> mutate(wt_local__ = .data[[wt_var]])
  }

  # Check if var_group is not NULL and all var_group from the vectpr do not exist
  if (!is.null(var_group)) {
    if ("all" %in% var_group) {
      dta <- dta |>
        mutate(group_var = "No groupping", group_val = "All observations")
    } else {
      missing_var_group <- setdiff(var_group, names(dta))
      if (length(missing_var_group) > 0) {
        cli::cli_warn(
          "Grouping variable(s) {.var {missing_var_group}} not found in the data. ",
          "These variable(s) will be ignored."
        )
      } else {
        var_group <- intersect(var_group, names(dta))
        dta <- dta |>
          mutate(
            group_var = var_group,
            group_val = as.character(!!sym(var_group))
          )
      }
    }
  } else {
    dta <- dta |>
      mutate(group_var = "No groupping", group_val = "All observations")
  }

  # Aggregate by decile
  dta <-
    dta |>
    select(
      c(
        all_of(c(
          var_decile,
          "group_var",
          "group_val",
          "wt_local__",
          "decile_val"
        )),
        any_of(var_agg) & (where(~ is.numeric(.) | is.integer(.)))
      )
    ) |>
    summarise(
      n = n(),
      pop = sum(wt_local__, na.rm = TRUE),
      decile_val = first(decile_val),
      across(
        c(
          any_of("decile_val"),
          any_of(var_agg) & (where(~ is.numeric(.) | is.integer(.)))
        ),
        ~ sum(. * wt_local__, na.rm = TRUE)
      ),
      .by = any_of(c(var_decile, "group_var", "group_val"))
    )

  n_dec <- length(levels(dta[[var_decile]]))
  dta |>
    mutate(
      decile_var = dec_income,
      decile_n = n_dec,
      decile = .data[[var_decile]]
    ) |>
    select(
      decile_var,
      decile,
      decile_n,
      decile_val,
      group_var,
      group_val,
      n,
      pop,
      any_of(var_agg)
    ) |>
    mutate(
      group_var = as_factor(group_var),
      group_val = as_factor(group_val)
    ) |>
    arrange(across(any_of(c("group_var", "group_val", "decile")))) |>
    pivot_longer(
      cols = any_of(var_agg),
      names_to = "var",
      values_to = "level"
    )
}


#' @describeIn deciles Aggregate variables by multiple decile groupings
#'
#' @param dta A data frame containing decile variables and variables to aggregate
#' @param var_deciles Character vector of decile variable names, each formatted as
#'   `{var}___decile` where `{var}` is the income variable and `{n}` is
#'   the number of deciles/quantiles
#' @param var_agg Character vector of variable names to aggregate by weighted sum
#' @param wt_var Character name of the weight variable. If NULL or not found,
#'   equal weights of 1 are used
#' @param ... Additional arguments passed to `f_agg_by_decile_one()`
#'
#' @return A tibble combining aggregations for all decile groupings, with one
#'   row per decile group per decile variable. Contains the same columns as
#'   `f_agg_by_decile_one()` output, stacked for all decile variables.
#'
#' @details
#' This function is a wrapper around `f_agg_by_decile_one()` that applies it
#' to multiple decile variables and combines the results. Useful when you want
#' to aggregate by deciles of different income concepts (e.g., market income,
#' net income, disposable income) in a single operation.
#'
#' @examples
#' \dontrun{
#' f_agg_by_decile(
#'   dta = microdata,
#'   var_decile = c("ym___decile", "yn___decile", "yp___decile"),
#'   var_agg = c("ym", "yn", "yp", "dtx_prog1", "dtx_prog2"),
#'   wt_var = "hhwt"
#' )
#' }
f_agg_by_decile <- function(
  dta,
  var_decile,
  var_agg,
  wt_var = NULL,
  var_group = NULL,
  ...
) {
  # Check if var_deciles exist in dta
  missing_vars <- var_decile[!var_decile %in% names(dta)]
  # Skip the one missing if at leat one exists
  if (length(missing_vars) == length(var_decile)) {
    cli::cli_abort(
      "None of the specified decile variables {.var {missing_vars}} are present in the data."
    )
  }
  if (length(missing_vars) > 0) {
    cli::cli_warn(
      "Variable(s) {.var {missing_vars}} not found in the data. Skipping these variable(s)."
    )
    var_decile <- setdiff(var_decile, missing_vars)
  }

  # Check if var_group is a character vector and warn if not assignint var_group  to null
  if (!is.null(var_group) && !is.character(var_group)) {
    cli::cli_warn(
      "{.arg var_group} should be a character vector of variable names. Ignoring {.arg var_group}."
    )
    var_group <- NULL
  }

  var_decile |>
    purrr::map(
      ~ {
        var_decile_local <- .x
        var_group |>
          map(
            ~ {
              var_group_local <- .x
              f_agg_by_decile_one(
                dta = dta,
                var_decile = var_decile_local,
                var_agg = var_agg,
                var_group = var_group_local,
                wt_var = wt_var,
                ...
              )
            }
          ) |>
          bind_rows()
      }
    ) |>
    bind_rows()
}

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
    cli::cli_abort(
      "None of {.arg dec_var} variables found in data: {.var {missing_vars}}"
    )
  }
  if (length(missing_vars) > 0) {
    cli::cli_warn("Skipping missing variables: {.var {missing_vars}}")
    dec_var <- setdiff(dec_var, missing_vars)
  }

  new_dec_var <- paste0(dec_var, "___decile")

  # Check NULL first, then membership
  if (is.null(wt_var) || !wt_var %in% names(dta)) {
    dta <- dta |> mutate(wt_temp__ = 1)
  } else {
    dta <- dta |> mutate(wt_temp__ = !!sym(wt_var))
  }

  # Identify which deciles need to be created
  on_dec_var <- dec_var#[!(new_dec_var %in% names(dta))]

  # if (length(on_dec_var) == 0) {
  #   cli::cli_inform("All requested decile variables already exist")
  #   return(dta)
  # }

  dta <-
    dta |>
    select(-any_of(new_dec_var)) |>
    calc_deciles(
      dec_var = on_dec_var,
      wt_var = "wt_temp__",
      n_dec = n_dec,
      dec_var_name = str_c("{.col}___decile")
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
f_calc_deciles_by_sim <- function(
  dta_sim,
  dec_var,
  wt_var = NULL,
  n_dec = 10,
  ...
) {
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
  function(
    dta,
    dec_var, #get_inc_nm()$var,
    n_dec = 10,
    wt_var = NULL, #get_wt_nm(),
    dec_var_name = "{.col}_decile",
    ...
  ) {
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
      warning(
        "`wt_var` was not specified. ",
        "Non-weighted statistics is computed."
      )
      dta <- mutate(dta, dummy_weighting_variable = 1)
      wt_var <- "dummy_weighting_variable"
    }

    if (!is.null(wt_var) && !wt_var %in% names(dta)) {
      warning(
        "Weighting variable '",
        wt_var,
        "' is not present in the `dta`. ",
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
  function(dta, vars, by_var, wt_var = get_wt_nm()) {
    vars_1 <- vars[vars %in% names(dta)]
    non_numeric_vars <- vars_1[!sapply(dta[vars_1], is.numeric)]
    if (length(non_numeric_vars) > 0) {
      warning(
        "The following non-numeric variables will be excluded from aggregation: ",
        paste(non_numeric_vars, collapse = ", ")
      )
      vars_1 <- setdiff(vars_1, non_numeric_vars)
      vars_1 <- setdiff(vars_1, wt_var)
    }

    if (is.null(wt_var)) {
      warning(
        "`wt_var` was not specified. ",
        "Non-weighted statistics is computed."
      )
      dta <- mutate(dta, dummy_weighting_variable = 1)
      wt_var <- "dummy_weighting_variable"
    }

    if (!is.null(wt_var) && !wt_var %in% names(dta)) {
      warning(
        "Weighting variable '",
        wt_var,
        "' is not present in the `dta`. ",
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
  function(
    dta,
    policy_name = NA_character_,
    dec_by,
    dec_vars,
    n_dec = 10,
    wt_var = NULL,
    get_var_fn = get_var_nm,
    ...
  ) {
    # policy_name = NA_character_
    # dec_by = "yp_pc"
    # dec_vars =  get_inc_nm()$var
    # wt_var = "pcweight"
    # browser()
    dta %>%
      calc_deciles(
        dec_var = dec_by,
        dec_var_name = "Decile",
        n_dec = n_dec,
        wt_var = wt_var,
        ...
      ) %>%
      calc_agg_by(
        by_var = "Decile",
        vars = c(dec_by, dec_vars) %>% unique(),
        wt_var = wt_var
      ) %>%
      pivot_longer(
        cols = c(everything(), -Decile, -any_of(dec_by)),
        names_to = "var"
      ) %>%
      left_join(get_var_fn(), by = "var") %>%
      mutate(
        var_title2 = if_else(
          is.na(var_title),
          as.character(var),
          as.character(var_title)
        ),
        var_title2 = factor(var_title2, levels = unique(var_title2)) %>%
          forcats::fct_relevel(levels(get_var_fn(dec_vars)$var_title)),
        var_title = var_title2
      ) %>%
      select(-var_title2) %>%
      mutate(
        Income = get_var_fn(dec_by)$var_title[[1]],
        Simulation = policy_name %>% forcats::fct_drop()
      ) %>%
      select(
        Decile,
        Income_value = {{ dec_by }},
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
  function(
    sim_res,
    dec_by,
    dec_vars,
    get_var_fn = get_var_nm,
    n_dec = 10,
    wt_var = NULL,
    ...
  ) {
    sim_res %>%
      purrr::map(
        ~ {
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
        }
      )
  }
