#' Variables relabelling and manipulation helpers
#' @name f_var_helpers
#' 
NULL

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