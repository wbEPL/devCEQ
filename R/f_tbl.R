#' Tables formatting helper functions
#' @name f_tbl_helpers
NULL


f_label_percent <- function(x, ...) {
  x_max <- max(x, na.rm = TRUE)
  x_min <- min(x, na.rm = TRUE)
  if (x_max > 0.5) {
    return(scales::label_percent(accuracy = .1, scale = 100))
  } else if (x_max > 0.1) {
    return(scales::label_percent(accuracy = .01, scale = 100))
  } else {
    return(scales::label_percent(accuracy = .001, scale = 100))
  }
}

f_label_number <- function(x, ...) {
  x_max <- max(x, na.rm = TRUE)
  x_min <- min(x, na.rm = TRUE)
  if (x_max < 2 & x_min > -2) {
    scale_fn <- scales::label_number(accuracy = .0001, ...)
  } else if (x_max < 10 & x_min > -10) {
    scale_fn <- scales::label_number(accuracy = .001, ...)
  } else if (x_max < 100 & x_min > -100) {
    scale_fn <- scales::label_number(accuracy = .01, ...)
  } else if (x_max < 9000 & x_min > -9000) {
    scale_fn <- scales::label_number(accuracy = .1, big.mark = "", ...)
  } else {
    scale_fn <- scales::label_number(
      accuracy = .1,
      scale_cut = scales::cut_short_scale() , #c(0, "K" = 5 * 10^3, "M" = 10^6, "B" = 10^9),
      big.mark = "",
      ...
    )
  }
  return(scale_fn)
}

#' @describeIn f_tbl_helpers Format number by title and value
#' @importFrom scales label_percent label_number cut_short_scale
#' @param x Numeric vector to format
#' @param title Character string of measure title to determine formatting
#' @param ... Additional arguments (currently unused)
#' @return A character vector of formatted numbers
#' @export
f_num_by_title <- function(x, title = NULL, ...) {
  
  # Check if title indicates percentage formatting
  is_percent <- !is.null(title) && 
                length(title) > 0 && 
                all(!is.na(title)) && 
                all(str_detect(as.character(title), "%"))
  
  if (is_percent) {
    format_fn <- f_label_percent(x)
  } else if (any(!is.null(x) || !is.na(x))) {
    format_fn <- f_label_number(x)
  } else {
    format_fn <- scales::label_number(accuracy = .001, big.mark = "", ...)
  }
  
  format_fn
}



#' @describeIn f_tbl_helpers Format a data frame for table output
#' @param dta A data frame to format
#' @param pivot_names_from Character vector of column names to pivot into new column names (default: c("group_var", "group_val"))
#' @param pivot_values_from Character vector of column names to pivot into new column values (default: c("value"))
#' @param ... Additional arguments (currently unused)
#' @return A formatted data frame ready for table output
#' @importFrom dplyr group_by mutate across case_when
#' @importFrom tidyr pivot_wider unnest
#' @importFrom stringr str_detect
#' @importFrom scales number percent
#' @export
f_format_tbl <- function(
  dta,
  pivot_names_from = c("group_var", "group_val"),
  pivot_values_from = c("value"),
  ...
) {
  col_measure <- f_get_colname("measure")
  col_val <- f_get_colname("value")
  # browser()
  dta |>
    group_by(across(any_of(col_measure))) |>
    nest() |>
    mutate(
      data = purrr::map2(
        data,
        .data[[col_measure]],
        ~ {
          format_fn <- f_num_by_title(.x[[col_val]], as.character(.y))
          .x |> mutate(across(any_of(col_val), ~ format_fn(.)))
        }
    )) |> 
    tidyr::unnest(cols = c(data)) |> 
    ungroup() |>
    pivot_wider(
      names_from = any_of(unname(f_get_colname(pivot_names_from))),
      values_from = any_of(unname(f_get_colname(pivot_values_from))),
      names_sep = "__",
      values_fill = "",
      values_fn = as.character
    )
}

#' @describeIn f_tbl_helpers Format a decile table for output
#' @export
f_format_decile_tbl <- function(
  dta,
  pivot_names_from = c("group_var", "group_val"),
  pivot_values_from = c("value"),
  ...
) {
  dta |>
    select(
      -any_of(c(
        f_get_colname("n"),
        f_get_colname("decile_n"),
        f_get_colname("pop"),
        f_get_colname("decile_val")
      ))
    ) |>
    f_format_tbl(
      pivot_names_from = pivot_names_from,
      pivot_values_from = pivot_values_from,
      ...
    )
}