#' Variables relabelling and manipulation helpers
#' @name f_var_helpers
#' 
NULL


#' @describeIn f_var_helpers Get dictionary data frame from a function name
#' 
#' @param dic_nm Character string naming a function that returns a dictionary data frame
#' @param ... Additional arguments passed to the dictionary function
#' 
#' @return A data frame dictionary if found and valid, NULL otherwise with warning
#' 
#' @details
#' This function checks if a dictionary function exists, calls it, and validates
#' the result is a non-empty data frame. Returns NULL with warnings if validation fails.
#' 
#' @keywords internal
f_get_dic <- function(dic_nm, ...) {
  # Validate input
  if (is.null(dic_nm) || !is.character(dic_nm) || length(dic_nm) != 1) {
    cli::cli_warn("{.arg dic_nm} must be a single character string.")
    return(NULL)
  }
  
  if (!exists(dic_nm, mode = "function")) {
    cli::cli_warn("Dictionary function {.fn {dic_nm}} not found.")
    return(NULL)
  }
  
  # Call dictionary function safely
  dic <- tryCatch(
    do.call(dic_nm, list(...)),
    error = function(e) {
      cli::cli_warn("Error calling {.fn {dic_nm}}: {e$message}")
      return(NULL)
    }
  )
  
  # Validate result
  if (!is.null(dic)) {
    if (!is.data.frame(dic)) {
      cli::cli_warn("Dictionary {.fn {dic_nm}} must return a data frame.")
      return(NULL)
    }

    if (nrow(dic) == 0) {
      cli::cli_warn("Dictionary {.fn {dic_nm}} is empty.")
      return(NULL)
    }
  }

  return(dic)
}

#' @describeIn f_var_helpers Get updated dictionary by merging custom and default dictionaries
#' 
#' @param dic_nm Character string naming a custom dictionary function
#' @param dic_default_name Character string naming a default dictionary function
#' @param ... Additional arguments passed to the dictionary functions
#' 
#' @return A data frame combining default dictionary with custom overrides
#' 
#' @details
#' This function retrieves both custom and default dictionaries, then updates
#' the default with custom values using \code{rows_update()}. If only one
#' dictionary is valid, it returns that one. Aborts if both are invalid.
#' 
#' @keywords internal
f_get_upd_dic <- function(dic_nm, dic_default_name, ...) {
  # Validate inputs
  if (is.null(dic_nm) || !is.character(dic_nm) || length(dic_nm) != 1) {
    cli::cli_warn("{.arg dic_nm} must be a single character string.")
    dic_nm <- NULL
  }
  
  if (is.null(dic_default_name) || !is.character(dic_default_name) || length(dic_default_name) != 1) {
    cli::cli_abort("{.arg dic_default_name} must be a single character string.")
  }
  
  # Get dictionaries
  dic <- if (!is.null(dic_nm)) f_get_dic(dic_nm, ...) else NULL
  dic_default <- f_get_dic(dic_default_name, ...)
  
  # Check if both are NULL
  if (is.null(dic) && is.null(dic_default)) {
    cli::cli_abort(
      "Both dictionaries are invalid: {.fn {dic_nm}} and {.fn {dic_default_name}}"
    )
  }
  
  # Merge dictionaries if both exist
  if (!is.null(dic) && !is.null(dic_default)) {
    common_cols <- intersect(names(dic_default), names(dic))
    
    if (length(common_cols) == 0) {
      cli::cli_warn(
        "No common columns between {.fn {dic_nm}} and {.fn {dic_default_name}}. Using default."
      )
      return(dic_default)
    }
    
    dic <- tryCatch(
      dic_default |> rows_update(dic, by = common_cols, unmatched = "ignore"),
      error = function(e) {
        cli::cli_warn("Error merging dictionaries: {e$message}. Using default.")
        dic_default
      }
    )
  } else if (is.null(dic)) {
    dic <- dic_default
  }

  return(dic)
}

#' @describeIn f_var_helpers Add measure labels to a data frame
#' 
#' @param dta A data frame containing a \code{measure} column with measure codes
#' @param measure_nm_tbl A data frame with measure names/labels (default: \code{get_measure_nm()})
#' @param ... Additional arguments (currently unused)
#' 
#' @return A data frame with the \code{measure} column replaced by labeled factors
#' 
#' @export
f_add_measure_labels <- function(dta, measure_nm_tbl = get_measure_nm(), ...) {
  dta |>
    left_join(measure_nm_tbl, by = c("measure" = "measure")) |>
    mutate(
      measure = if_else(is.na(measure_title), measure, measure_title) |> as_factor()
    ) |>
    select(-measure_title)
}


#' @describeIn f_var_helpers Add variable labels to a data frame
#' 
#' @param dta A data frame containing a \code{var} column with variable codes
#' @param var_nm_tbl A data frame with variable names/labels (default: \code{get_var_nm()})
#' @param ... Additional arguments (currently unused)
#' 
#' @return A data frame with the \code{var} column replaced by labeled factors
#' 
#' @export
f_add_var_labels <- function(dta, var_nm_tbl = get_var_nm(), to_var = "var", ...) {
  dta |>
    left_join(var_nm_tbl, by = setNames("var", to_var)) |>
    mutate(
      !!sym(to_var) := if_else(is.na(var_title), !!sym(to_var), var_title) |>
        as_factor()
    ) |>
    select(-var_title, -any_of("factor") )
}


#' @describeIn f_var_helpers Get column names dictionary, using custom if available
#' @return A named character vector mapping original column names to new names
#' @export
f_get_colnames_dic <- function() {
  if (exists("f_colnames_dic", mode = "function")) {
    dic <- f_colnames_dic()
  } else {
    dic <- f_colnames_dic_default()
  }
  dic
}

#' @describeIn f_var_helpers Get new column name for a given original name
#' @param x Original column name
#' @return The new column name if found in the dictionary, otherwise returns \code{x} unchanged
#' @export
#' 
f_get_colname <- function(x) {
  dic <- f_get_colnames_dic()
  if (any(x %in% names(dic))) {
    unname(dic[x])
  } else {
    x
  }
}

#' @describeIn f_var_helpers Rename data frame columns using a dictionary
#' @param dta A data frame to rename columns
#' @param dic A named character vector mapping new names to original column names
#' (default: output of \code{f_get_colnames_dic()})
#' @param ... Additional arguments (currently unused)
#' @return A data frame with renamed columns
#' @export
f_rename_cols <- function(dta, dic = f_get_colnames_dic(), ...) {
  dta |> 
    rename_with(
      .fn = ~ dic[.x],
      .cols = intersect(names(dic), colnames(dta))
    )
}