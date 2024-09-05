
#' Summarise the `diffdf::diffdf` results in a compact table
#'
#' @param dta is the results of the diffdf::diffdf() functional call.
#'
#' @returns
#'
#' `get_diffdf_sum()` returns A data frame that summaries the differences between `B` - Base data and
#' `C` - Compare data frame by variable present in the data. The columns in
#' The summary data frame consists of:
#'
#' * `VARIABLE` is the name of the variable compared;
#'
#' * `Diff ` is the number of rows different between Base and Compare for
#'   each variable given provided tolerance;
#'   Diff excl NA` is the number of rows where both values in Base and
#'   Compare are non-NA and the difference between them is above the tolerance;
#'
#' * `NA-!0 B*C|C*B[Total]` is the number of rows that have **NA (missing)
#'   value in base and a non-Zero value in Compare**, after "|" the comparison
#'   of Compare versus Base is made and the total is reported in brackets;
#'
#' * `NA-0 B*C|C*B[Total]` is the number of rows that have **NA (missing) value
#'   in base and a Zero value in Compare**, after "|" the comparison of
#'   Compare versus Base is made and the total is reported in brackets;
#'
#' * `MAD` is the average of the absolute deviation between base and compare
#'   (not rounded), i.e. $\\sum_i|\\text{Base} - \\text{Compare}| / N$, where
#'   $i$ is the index of the row that is different between Base and Compare
#'   given the tolerance (any missing/NA values are converted to zero);
#'
#' * `Mean ABS dev/Base` is the mean absolute deviation between the base
#'   and compare divided by base value and multiplied by 100% (rounded to 4
#'   digits), i.e. $\\sum_i|\\text{Base} - \\text{Compare}| / N$, where $i$ is
#'   the index of the row that is different between Base and Compare given
#'   the tolerance;
#'
#' @export
#' @importFrom purrr imap_dfr
get_diffdf_sum <- function(dta) {
  dta %>%
    purrr::imap_dfr( ~ {
      if (str_detect(.y, "VarDiff")) {
        .x %>% get_MAD()
      }
    })
}


#' @describeIn get_diffdf_sum Compare base and compage data using diffdf::diffdf() by matching columns only.
#' @param base_dta base data frame
#' @param compare_dta data frame to compare with base
#' @param tolerance numerical tolerance for each comparison (0.01 by default).
#' @param key is the key columns in both data frames that could be used for row-by-row
#'        comparison. If NULL, data frames are compared by row order.
#' @import diffdf diffdf
#' @export
mydiff <- function(base_dta, compare_dta, key = c("numind", "hhid"), tollerance = 0.01) {
  base_dta %>%
    diffdf::diffdf(compare_dta %>% select(any_of(names(base_dta))),
                   tolerance = tollerance,
                   keys = key)
}

#' @describeIn get_diffdf_sum Computed summary statistics about the differences in a single value
#' @param dta is the element of the list resulting from the diffdf::diffdf()
#'            function. Usually, such elements of a list start from `VarDiff`.
#'
#' @export
get_MAD <- function(dta) {
  dta %>%
    group_by(VARIABLE) %>%
    # filter()
    summarise(
      `Diff` = n(),
      `Diff excl NA` =
        n() - sum((is.na(BASE) & !is.na(COMPARE)) | (!is.na(BASE) & is.na(COMPARE))),
      `NA-!0 BxC|CxB [Total]` =
        str_c(
          sum(is.na(BASE) & (!is.na(COMPARE) & COMPARE != 0)), "|",
          sum(is.na(COMPARE) & (!is.na(BASE) & BASE != 0)), "[",
          sum((is.na(BASE) & (!is.na(COMPARE) & COMPARE != 0)) |
                (is.na(COMPARE) & (!is.na(BASE) & BASE != 0))),
          "]"),

      `NA-0 BxC|CxB [Total]` =
        str_c(
          sum(is.na(BASE) & (COMPARE == 0 & !is.na(COMPARE))), "|",
          sum(is.na(COMPARE) & (BASE == 0 & !is.na(BASE))), "[",
          sum((is.na(BASE) & (COMPARE == 0 & !is.na(COMPARE))) |
                (is.na(COMPARE) & (BASE == 0 & !is.na(BASE)))),
          "]"),
      MAD = mean(abs(ifelse(is.na(BASE), 0, BASE) - ifelse(is.na(COMPARE),0,COMPARE)), na.rm = TRUE),
      `abs diff % B` = round(mean(abs(BASE - COMPARE) / BASE, na.rm = TRUE) * 100, 4))
}

#' @describeIn get_diffdf_sum Compares two data sets and prints knittable summary
#'
#' @export
compare_two <-
  function(dta_base,
           dta_compare,
           tol = 0.01,
           keys = c("hhid")) {
    bl_diff <-
      diffdf(dta_base,
             dta_compare,
             tolerance = tol,
             keys = keys)
    bl_diff |> get_diffdf_sum() |> kable() |> print()

    if (any(str_detect(names(bl_diff), "VarDiff"))) {
      cat("There are differences between Base and compare at ",
          tol,
          " level of tolerance. \n")
      cat(
        "Tables below show up to 10 cases, when a certain variables is difference between Base and Compare. \n"
      )
      cat(
        "Observations are sorted in a descending order of the absolute value of the difference between Base and Compare. \n"
      )

      bl_diff |>
        names() |>
        str_subset("VarDiff") |>
        walk(~ {
          cat("\n")
          bl_diff[[.x]] |>
            mutate(`|DIFF|` = abs(BASE - COMPARE),
                   `|DIFF| % of BASE` = `|DIFF|` / BASE * 100) |>
            arrange(desc(`|DIFF|`)) |>
            slice(seq(1, min(10, nrow(bl_diff[[.x]])))) |>
            kable(digits = 3) |> print()
          cat("\n")
        })
    } else {
      cat("There are no differences at the ", tol, " level of tolerance. \n")
    }
  }
