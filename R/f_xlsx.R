#' Tools to manipulate Excel files in PEA dashboards
#' 
#' @name peaxlsx
#' 
NULL


#' @describeIn peaxlsx Write multiple PEA-style sheets to an Excel file
#'
#' @param all_dta A list of lists, where each sublist contains:
#'
#' - `sheet_name`: The name of the sheet to be created.
#' - `meta_tbl`: A data frame containing metadata information for the sheet.
#' - `tbl`: A data frame containing the main data for the sheet.
#' - `ggs`: A ggplot object or a list of ggplot objects to be added to the sheet.
#'
#' @param ... Additional arguments passed to the `wb_save` function for saving the Excel file.
#'
#' @return The path to the created Excel file.
#' @importFrom openxlsx2 wb_workbook wb_save
#' @importFrom purrr reduce
#'
#'
#'
pea_write_xlsx <- function(all_dta, progres_fn = function() {}, ...) {
  temp_file <- tempfile(fileext = ".xlsx")

  # Add any overview sheets if neede here
  sheet_1 <- wb_workbook(creator = 'WB PEA', theme = "Office Theme")

  all_dta |>
    reduce(
      ~ {
        progres_fn()

        if (is.null(.y$sheet_name)) {
          cli::cli_warn(c(
            x = "`sheet_name` is NULL for one of the sheets."
          ))
          return(.x)
        }

        if (!is.null(.y$ft)) {
          if (is.list(.y$ft)) {
            .y$tbl <- .y$ft
          } else {
            .y$tbl <- list(.y$ft)
          }
        }

        peawb_add_sheet(
          .x,
          sheet_name = .y$sheet_name,
          meta_tbl = .y$meta_tbl,
          tbl = .y$tbl,
          ggs = .y$ggs
        )
      },
      .init = sheet_1
    ) |>
    wb_save(file = temp_file, overwrite = T)
  return(temp_file)
}


#' @describeIn peaxlsx Excel file name
#' 
fct_default_excel_filename <- function(suffix = "Fiscal-Sim results ", extension = ".xlsx") {
  file.path(tempdir(),
    paste0(suffix, format(Sys.time(), "%Y-%m-%d %H-%M-%S"), extension))
}


#' @describeIn peaxlsx Add a PEA-style sheet to an Excel workbook
#'
#' This function adds a new sheet to an existing Excel workbook with a specified name,
#' metadata table, data table, and optional ggplot figures. The metadata table is placed
#' at the top of the sheet, followed by the data table, and then the ggplot figures are added
#' below the tables.
#'
#' @param wb An existing Excel workbook object (of class `wbWorkbook`).
#' @param sheet_name A character string specifying the name of the new sheet.
#' @param meta_tbl A data frame containing metadata information to be placed at the top of the sheet.
#' @param tbl A data frame containing the main data to be placed below the metadata table.
#' @param ggs An optional list of ggplot objects to be added below the data table.
#' @param ... Additional arguments passed to the `wb_add_image` function for customizing image placement.
#'
#' @return The modified Excel workbook object with the new sheet added.
#'
#' @importFrom openxlsx2 wb_add_worksheet
#' @importFrom cli cli_abort
#'
#'
#'
peawb_add_sheet <- function(wb, sheet_name, meta_tbl = tibble(0), tbl = tibble(0), ggs = NULL, ...) {
  if (!inherits(wb, "wbWorkbook")) {
    cli_abort("`wb` must be a <wbWorkbook> object.")
  }
  if (!is.character(sheet_name) || length(sheet_name) != 1) {
    cli_abort("`sheet_name` must be a single character string.")
  }

  wb <- wb |> wb_add_worksheet(sheet_name) 

  if (inherits(meta_tbl, "data.frame")) {
    meta_tbl <- list(meta_tbl)
  }
  
  if (inherits(meta_tbl, "flextable")) {
    meta_tbl <- list(meta_tbl)
  }

  # browser()
  if (inherits(tbl, "data.frame")) {
    tbl <- list(tbl) 
  }
  
  if (inherits(tbl, "flextable")) {
    tbl <- list(tbl)
  }

  all_tbls <- meta_tbl |> append(tbl)
  all_lengths <- all_tbls |> map(~ get_nrows(.x) + 5) |> cumsum()
  all_width <- all_tbls |> map(~ get_ncols(.x)) |> unlist() |> max()
  start_point <- c(1, all_lengths)
  start_point <- start_point[-length(start_point)] 
  
  reduce2(
    all_tbls,
    start_point,
    ~ {
      peawb_add_table(
        ..1,
        sheet = sheet_name,
        dta = ..2,
        start_row = ..3,
        with_filter = FALSE
      )
    },
    .init = wb
  ) |>
    peawb_add_ggs(
      sheet_name = sheet_name,
      offset_x = all_width + 2,
      offset_y = 12,
      ggs = ggs,
      ...
    )
}

#' @describeIn peaxlsx Get the number of rows or columns in a data frame or flextable object
#' 
get_nrows <- function(dta) {
  if (inherits(dta, "flextable")) {
    nrow(dta$body$dataset)
  } else if (inherits(dta, "data.frame")) {
    nrow(dta)
  } else {
    cli_abort("`dta` must be a data frame or flextable object.")
  }
}

#' @describeIn peaxlsx Get the number of rows or columns in a data frame or flextable object
#' 
get_ncols <- function(dta) {
  if (inherits(dta, "flextable")) {
    ncol(dta$body$dataset)
  } else if (inherits(dta, "data.frame")) {
    ncol(dta)
  } else {
    cli_abort("`dta` must be a data frame or flextable object.")
  }
}

#' @describeIn peaxlsx Add a data frame or flextable to an existing Excel workbook sheet
#' 
peawb_add_table <- function(
  wb,
  sheet_name,
  dta,
  start_row = 1,
  start_col = 1,
  ...
) {

  # Switch function based on class of dta
  fn_dta <- if (inherits(dta, "flextable")) {
    peawb_add_flextable
  } else if (inherits(dta, "data.frame")) {
    peawb_add_df
  } else {
    cli_abort("`dta` must be a data frame or flextable object.")
  }

  wb |>
    fn_dta(
      sheet = sheet_name,
      dta = dta,
      start_row = start_row,
      start_col = start_col,
      ...
    ) 
}

#' @describeIn peaxlsx Add a flextable to an existing Excel workbook sheet
#' 
#' @importFrom flexlsx wb_add_flextable
#' 
peawb_add_flextable <- function(
  wb,
  sheet_name,
  dta,
  start_row = 1,
  start_col = 1,
  ...
) {
  wb |>
    flexlsx::wb_add_flextable(
      sheet = sheet_name,
      ft = dta,
      start_row = start_row,
      start_col = start_col
    )
}

#' @describeIn peaxlsx Add a data frame to an existing Excel workbook sheet
#' 
#' @importFrom openxlsx2 wb_add_data
#' 
peawb_add_df <- function(
  wb,
  sheet_name,
  dta,
  start_row = 1,
  start_col = 1,
  ...
) {
  wb |>
    wb_add_data(
      sheet = sheet_name,
      x = dta,
      na.strings = "",
      start_row = start_row,
      start_col = start_col,
      ...
    )
}


#' @describeIn peaxlsx add ggplot figures to an existing Excel workbook sheet
#' 
#' This function adds ggplot figures to a specified sheet in an existing Excel workbook.
#' The figures are saved as PNG files and inserted into the sheet at specified positions.
#' 
#' @param wb An existing Excel workbook object (of class `wbWorkbook`).
#' @param sheet_name A character string specifying the name of the sheet where the figures will be added.
#' @param offset_x An integer specifying the column offset for placing the figures. Default is 0.
#' @param ggs A list of ggplot objects to be added to the sheet.
#' @param ... Additional arguments passed to the `wb_add_image` function for customizing image placement.
#' 
#' @return The modified Excel workbook object with the ggplot figures added to the specified sheet.
#' 
#' @importFrom openxlsx2 wb_add_image wb_dims
#' @importFrom purrr reduce2 map
#' @importFrom ggplot2 ggsave
#' 
#' 
peawb_add_ggs <- function(wb, sheet_name, offset_x = 0, ggs = NULL, ...) {

  # Check the length of the ggplots. If more than one loop over them
  if (!is.null(ggs)) {
    if (inherits(ggs, "gg")) {
      ggs <- list(ggs)
    } 
    ggs <- map(ggs, ~ {
      to_file <- tempfile(fileext = ".png")
      ggsave(
        to_file,
        plot = .x,
        width = 6,
        height = 3.5,
        units = "in",
        dpi = 300,
        scale = 1.25
      )
      to_file
    })
  } else {
    return(wb)
  }

  if (length(ggs) > 0) {
    wb <-
      reduce2(
        .x = ggs,
        .y = seq_along(ggs),
        .f = ~  {
          aa <- ..1 |>
            wb_add_image(
              sheet = sheet_name,
              dims = wb_dims(
                from_col = offset_x + ..3,
                from_row = 10 + 17 * (..3 - 1)
              ),
              file = ..2,
              width = 6,
              height = 3.5
            )
        },
        .init = wb
      )
    return(wb)
  }
}


#' @describeIn peaxlsx Get columns with a single unique observation
#' 
#' 
fct_get_singleobs_cols <- function(dta) {
  dta <- dta |> select(where(~ n_distinct(.) == 1))
  if (ncol(dta) == 0) {
    return(tibble())
  } else {
    dta |>
      distinct() |>
      pivot_longer(
        everything(),
        names_to = " ",
        values_to = "  "
      )
  }
}









