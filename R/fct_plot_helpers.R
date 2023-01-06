#' Configurate a flextable
#'
#' @importFrom flextable flextable add_header_row merge_at colformat_num autofit theme_booktabs
#' @importFrom scales number_format
#'
#' @export
flextable_config <- function(dta, title = NULL, digits = 3, merge = TRUE) {
  form_num <- scales::number_format(1 / 10 ^ (digits), big.mark = ",")
  out <-
    dta %>%
    mutate(across(where(is.numeric), ~ form_num(.))) %>%
    flextable::flextable(col_keys = names(.))

  if (!is.null(title)) {
    out <-
      out %>%
      flextable::add_header_row(top = T, values = title, colwidths = ncol(dta))
  }
  if (merge) {
    out <-
      out %>%
      flextable::merge_at(i = 1, j = 1:length(dta), part = "header")
  }
  out %>%
    flextable::colformat_num(j = 2:length(dta), digits = digits) %>%
    flextable::autofit() %>%
    flextable::theme_booktabs()

}


#' Configurate a plotly chart
#'
#' @param plt plotly object
#'
#' @noRd
#' @importFrom plotly config
#' @export
plotly_config <- function(plt) {
  plt %>%
    plotly::config(
      displaylogo = FALSE,
      showAxisDragHandles = FALSE,
      modeBarButtonsToRemove = c(
        'pan2d',
        'select2d',
        'lasso2d',
        "toggleSpikelines",
        "resetScale2d"
      )
    )
}



#' helper for validating outputs
#' @export
validate_result <- function(dta, err_msg = "Error") {
  if ("try-error" %in% class(dta)) cat(dta[[1]])
  validate(need(!"try-error" %in% class(dta),
                str_c(attr(dta, err_msg))))
  dta
}

#' Configuration DT with data for printing in the app
#'
#' @description Format a DT table for the app
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @export
#' @importFrom DT datatable JS

fct_config_export_dt <- function(.data,
                                 file_title = "Table",
                                 digits_number = 3,
                                 pageLength = 10,
                                 scroll_y = TRUE) {

  pageLength = min(nrow(.data), pageLength)
  if (scroll_y)
    scroll_y_height = round(pageLength * 250 / 6, 0) %>% str_c(., "px")
  else {
    scroll_y_height = FALSE
  }
  req(.data)
  .data %>%
    DT::datatable(
      .,
      rownames = FALSE,
      extensions = c("Buttons", "Scroller"),
      options = list(
        pageLength = pageLength,
        dom = c("Bfrtip"),
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = scroll_y_height,
        scroller = TRUE,
        buttons = list(
          list(extend = "copy", text = "Copy"),
          list(
            extend = "excel",
            text = "Save in Excel",
            title = file_title
          )
        )
      )
    ) #%>%
  # DT::formatRound(columns = names(.data)[sapply(.data, is.numeric)],
  #                 digits = digits_number)
  # %>%
  #   DT::formatStyle(columns = names(dta), lineHeight='80%')
}


