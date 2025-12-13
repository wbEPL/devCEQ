#' Tables formatting helper functions
#' @name f_tbl_helpers
NULL

#' @describeIn f_tbl_helpers Format number by title and value
#' @importFrom scales label_percent label_number cut_short_scale
#' @param x Numeric vector to format
#' @param title Character string of measure title to determine formatting
#' @param ... Additional arguments (currently unused)
#' @return A character vector of formatted numbers
#' @export
f_num_by_title <- function(x, title = NULL, ...) {
  out <- case_when(
    # !is.null(title) && str_detect(title, "%") ~ scales::number(
    #   x,
    #   accuracy = .01,
    #   scale = 100
    # ),
    max(x, na.rm = TRUE) < 2 & min(x, na.rm = TRUE) > -2 ~ scales::number(
      x,
      accuracy = .0001
    ),
    max(x, na.rm = TRUE) < 10 & min(x, na.rm = TRUE) > -10 ~ scales::number(
      x,
      accuracy = .001
    ),
    max(x, na.rm = TRUE) < 100 & min(x, na.rm = TRUE) > -100 ~ scales::number(
      x,
      accuracy = .01
    ),
    max(x, na.rm = TRUE) < 9000 &
      min(x, na.rm = TRUE) > -9000 ~ scales::number(
      x,
      accuracy = .1,
      big.mark = ""
    ),
    .default = scales::number(
      x,
      accuracy = .1,
      scale_cut = c(" K" = 5 * 10^3, " M" = 10^6, " B" = 10^9),
      big.mark = ""
    )
  )
  title <- as.character(title)
  if (all(!is.null(title)) && all(str_detect(title, "%"))) {
    out <- scales::number(
      x,
      accuracy = .01,
      scale = 100
    )
  }
  return(out)
}


#' @describeIn f_tbl_helpers Format a data frame for table output
#' @param dta A data frame to format
#' @param pivot_names_from Character vector of column names to pivot into new column names (default: c("group_var", "group_val"))
#' @param pivot_values_from Character vector of column names to pivot into new column values (default: c("value"))
#' @param ... Additional arguments (currently unused)
#' @return A formatted data frame ready for table output
#' @importFrom dplyr group_by mutate across case_when
#' @importFrom tidyr pivot_wider
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
  dta |>
    group_by(across(any_of(col_measure))) |>
    mutate(
      across(
        any_of(col_val),
        ~ f_num_by_title(., .data[[col_measure]], "%")
      )
    ) |>
    ungroup() |>
    pivot_wider(
      names_from = any_of(unname(f_get_colname(pivot_names_from))),
      values_from = any_of(unname(f_get_colname(pivot_values_from))),
      names_sep = "__",
      values_fill = "",
      values_fn = as.character
    )
}