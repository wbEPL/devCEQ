#' Calculate poverty and inequality measures for multiple income variables
#' @named f_povineq
#' 
NULL

#' @describeIn f_povineq Calculate poverty and inequality measures for multiple income variables
#' @export
#'
f_calc_povineq <- function(
  dta,
  var_inc,
  var_wt = NULL,
  pl_var = NULL,
  pl_val = NULL,
  group_var = "total",
  ...
) {
  # Handling missing income variabels
  var_inc_not_found <- setdiff(var_inc, names(dta))
  var_inc_found <- intersect(var_inc, names(dta))
  if (length(var_inc_not_found) == length(var_inc)) {
    cli::cli_abort(
      "None of the income variables provided in 'var_inc' are found in the data."
    )
  }

  # Warn if any variables are not in the data
  if (length(var_inc_not_found) > 0) {
    cli::cli_warn(
      "The following income variables are not found in the data and will be ignored: {var_inc_missing}"
    )
  }

  dta <- dta |> mutate(default_inc = !!sym(var_inc_found[[1]]))

  # Handing missing weight variable
  if (is.null(var_wt) || !var_wt %in% names(dta)) {
    cli::cli_warn(
      "Weight variable '{var_wt}' not found in the data. Using equal weights."
    )
    var_wt <- "default_wt"
  }

  dta <- dta |> mutate(default_wt = 1, wt = !!sym(var_wt))

  # Handeling missing poverty line variable
  if (is.null(pl_var) && is.null(pl_val)) {
    cli::cli_warn(
      "'pl_var' and 'pl_val' are not provided: using default poverty line as 40% of median income."
    )
    # pl_val <- "pl_default"
    dta <- dta |> mutate(pl = weighted.mean(default_inc, w = wt) * 0.4)
  }

  if (is.null(pl_var) && !is.null(pl_val)) {
    # cli::cli_info("Using provided poverty line value.")
    dta <- dta |> mutate(pl = pl_val)
    # var_pl <- "pl_val"
  }

  if (!is.null(pl_var) && !pl_var %in% names(dta) && !is.null(pl_val)) {
    cli::cli_warn(
      "Poverty line variable '{pl_var}' not found in the data. Using provided poverty line value."
    )
    dta <- dta |> mutate(pl = pl_val)
  }

  if (!is.null(pl_var) && !pl_var %in% names(dta) && is.null(pl_val)) {
    cli::cli_warn(
      "Poverty line variable '{pl_var}' not found in the data and 'pl_val' is not provided. Using default poverty line as 40% of median income."
    )
    dta <- dta |> mutate(pl = weighted.mean(default_inc, w = wt) * 0.4)
  }

  if (!is.null(pl_var) && pl_var %in% names(dta)) {
    dta <- dta |> mutate(pl = !!sym(pl_var))
  }

  # Grpoup variable
  if (
    is.null(group_var) ||
      !all(group_var %in% names(dta)) ||
      group_var == "total"
  ) {
    dta <- dta |>
      mutate(group_var = "No groupping", group_val = "All observations")
  }

  if (!is.null(group_var) && all(group_var %in% names(dta))) {
    dta <- dta |>
      mutate(
        group_var = group_var,
        group_val = as.character(!!sym(group_var))
      )
  }

  dta |>
    select(
      any_of(c("group_var", "group_val", var_inc_found, "pl", "wt"))
    ) |>
    mutate(id = row_number()) |>
    pivot_longer(
      any_of(var_inc_found),
      names_to = "var",
      values_to = "inc_val"
    ) |>
    group_by(across(c("group_var", "group_val", "var"))) |>
    summarise(
      hc = sum((inc_val < pl) * wt, na.rm = TRUE),
      fgt0 = calc_pov_fgt(
        x = inc_val,
        pl = pl,
        alpha = 0,
        w = wt,
        na.rm = TRUE
      ),
      fgt1 = calc_pov_fgt(
        x = inc_val,
        pl = pl,
        alpha = 1,
        w = wt,
        na.rm = TRUE
      ),
      fgt2 = calc_pov_fgt(
        x = inc_val,
        pl = pl,
        alpha = 2,
        w = wt,
        na.rm = TRUE
      ),
      gini = calc_gini(x = inc_val, w = wt, na.rm = TRUE),
      theil = calc_theil(x = inc_val, w = wt, na.rm = TRUE),
      n = n(),
      pop = sum(wt, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = c(hc, fgt0, fgt1, fgt2, gini, theil, n, pop),
      names_to = "measure",
      values_to = "value"
    )
}


#' @describeIn f_povineq Calculate poverty and inequality measures for multiple income variables by multiple grouping variables
#' @importFrom dplyr bind_rows
#' @importFrom purrr map
#' @export
f_calc_povineq_by <-
  function(
    dta,
    var_inc,
    var_wt = NULL,
    pl_var = NULL,
    pl_val = NULL,
    group_vars = c("total"),
    ...
  ) {
    # Handle case when group_vars is NULL
    if (is.null(group_vars) || all(group_vars == "total")) {
      group_vars <- "total"
    }

    # If group vars are not total, check that they exist in the data
    group_vars_other <- setdiff(group_vars, "total")
    group_vars_other_missing <- setdiff(group_vars_other, names(dta))
    group_vars_other_present <- intersect(group_vars_other, names(dta))
    group_vars_used <- c("total", group_vars_other_present)
    if (length(group_vars_other_missing) > 0) {
      cli::cli_warn(
        "The following grouping variables are not found in the data and will be ignored: {group_vars_other_missing}"
      )
    }

    cli::cli_inform("Using grouping variables: {group_vars_used}")

    group_vars_used |>
      purrr::map(
        ~ {
          f_calc_povineq(
            dta = dta,
            var_inc = var_inc,
            var_wt = var_wt,
            pl_var = pl_var,
            pl_val = pl_val,
            group_var = .x,
            ...
          )
        }
      ) |>
      bind_rows()
  }



#' @describeIn f_povineq Calculate poverty and inequality measures across multiple policy simulations
#' 
#' @param dta_sim A list of simulation results, where each element contains:
#'   \describe{
#'     \item{policy_sim_raw}{A data frame with simulation microdata}
#'     \item{policy_name}{Character name of the policy (optional)}
#'   }
#' @param var_inc Character vector of income variable names to analyze
#' @param pl_var Character name of poverty line variable in the data (optional)
#' @param group_vars Character vector of grouping variables for disaggregation (optional)
#' @param var_wt Character name of weight variable (optional)
#' @param ... Additional arguments passed to \code{f_calc_povineq_by}
#' 
#' @return A tibble with poverty and inequality measures for all simulations,
#'   including columns for simulation name (\code{sim}), grouping variables,
#'   income variables (\code{var}), measures, and values.
#'   
#' @details 
#' The function automatically handles missing or duplicate policy names by:
#' \itemize{
#'   \item Replacing NULL, NA, or empty policy names with "Policy 1", "Policy 2", etc.
#'   \item Detecting duplicate names and renaming subsequent duplicates sequentially
#' }
#' 
#' If \code{pl_var} is not provided, a default poverty line is calculated as 
#' 40\% of the median income for each simulation.
#' 
#' @export
f_calc_povineq_by_sims <- function(
  dta_sim,
  var_inc,
  pl_var = NULL,
  group_vars = NULL,
  var_wt = NULL,
  ...
) {

  # Extract policy names
  policy_names <- vapply(dta_sim, function(x) {
    if (is.null(x$policy_name) || is.na(x$policy_name) || x$policy_name == "") {
      NA_character_
    } else {
      as.character(x$policy_name)
    }
  }, character(1))
  
  # Find duplicates and missing names
  for (i in seq_along(policy_names)) {
    if (is.na(policy_names[i]) || policy_names[i] %in% policy_names[seq_len(i - 1)]) {
      policy_names[i] <- paste0("Policy ", i)
      dta_sim[[i]]$policy_name <- policy_names[i]
    }
  }
  
  # Process all simulations
  dta_sim |> 
    map(
      ~ .x$policy_sim_raw |> 
        # mutate(pl_nat = median(ym) * 0.4) |> 
        f_calc_povineq_by(
          var_inc = var_inc,
          var_wt = var_wt,
          pl_var = pl_var,
          group_var = group_vars
        ) |> 
        mutate(sim = .x$policy_name)
    ) |> 
    bind_rows()
}


#' @describeIn f_povineq Calculate poverty and inequality measures across multiple policy simulations with default variable names
#' @param dta A list of simulation results, where each element contains:
#' @export
#' 
f_calc_pov_stats <- function(
  dta,
  var_inc = get_inc_nm()$var,
  var_wt = get_wt_nm(),
  group_vars = get_group_nm()$var,
  pl_var = NULL,
  pl_val = NULL,
  ...
) {

  # pl_var <- f_make_reactive(pl_var)
  # pl_val <- f_make_reactive(pl_val)
browser()
  f_calc_povineq_by_sims(
    dta_sim = dta,
    var_inc = var_inc,
    var_wt = var_wt,
    pl_var = pl_var,
    pl_val = pl_val,
    group_vars = group_vars
  ) |>
    f_add_measure_labels() |>
    f_add_var_labels() |>
    f_add_var_labels(to_var = "group_var") |>
    f_rename_cols()
}
