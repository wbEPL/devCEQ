#' fct_config_gen_dt
#'
#' @description Formats generif DT table for the app
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#'
#' @importFrom DT datatable JS
fct_config_gen_dt <-
  function(dta, file_name = "Table", group_row = NULL,
           precision = nchar(1 / (.Machine$double.eps)^0.5)) {
    dta %>%
      mutate(
        row_id = row_number(),
        changed = pmap_lgl(., ~{
          dta <- rlang::dots_list(...)
          lll <-
            dta[3:length(dta)] %>%
            unlist() %>%
            round(., digits = precision) %>%
            unique() %>%
            length()
          lll != 1
          })
      ) %>%
      arrange(desc(changed)) %>%
      DT::datatable(#
        rownames = FALSE,
        extensions = c('Buttons', 'Scroller', 'RowGroup'),
        options = list(
          width = "450px",
          autoWidth = T,
          rowGroup =
            if (!is.null(group_row)) {
              list(dataSrc = group_row)
            } else{
              NULL
            },
          #
          dom = 'Bfrtip',
          buttons =
            list(
              list(extend = 'copy'),
              list(
                extend = 'excel',
                filename = file_name,
                text = "Download in Excel"
              )
            ),
          deferRender = TRUE,
          scrollY = 550,
          scroller = TRUE,
          searching = FALSE,
          columnDefs =
            list(

              # Text overflow in the groups title column
              if (!is.null(group_row)) {
                list(list(
                  targets = group_row,
                  render = DT::JS(
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data.length > 25 ?",
                    "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
                    "}"
                  )
                ))
              } else {
                NULL
              }
              ,
              list(list(
                targets = 1,
                width = "150px"
              ))

              ,
              list(visible=FALSE, targets=c(1, ncol(dta), ncol(dta) + 1))
            )
        ),
        # callback = DT::JS('table.page(3).draw(false);'),
        selection = 'none') %>%
      DT::formatStyle(
        columns = "changed",
        target = "row",
        backgroundColor = DT::styleEqual(TRUE, c('yellow'))
        )
  }
#
# fct_config_export_dt <- function(.data,
#                                  file_title = "Table",
#                                  digits_number = 3,
#                                  pageLength = 10,
#                                  scroll_y = TRUE) {
#
#   pageLength = min(nrow(.data), pageLength)
#   if (scroll_y)
#     scroll_y_height = round(pageLength * 250 / 6, 0) %>% str_c(., "px")
#   else {
#     scroll_y_height = FALSE
#   }
#   .data %>%
#     DT::datatable(
#       .,
#       rownames = FALSE,
#       extensions = c("Buttons", "Scroller"),
#       options = list(
#         pageLength = pageLength,
#         dom = c("Bfrtip"),
#         scrollX = TRUE,
#         deferRender = TRUE,
#         scrollY = scroll_y_height,
#         scroller = TRUE,
#         buttons = list(
#           list(extend = "copy", text = "Copy"),
#           list(
#             extend = "excel",
#             text = "Save in Excel",
#             title = file_title
#           )
#         )
#       )
#     ) %>%
#     DT::formatRound(columns = names(.data)[sapply(.data, is.numeric)],
#                     digits = digits_number)
#   # %>%
#   #   DT::formatStyle(columns = names(dta), lineHeight='80%')
# }
