#' Module for dowanloding excel files with all the data and figures
#' 
#' @name m_download
#' 
NULL

# @describeIn m_download  Workaround for data download
#  
# @import shiny
# downloadButton <- function(...) {
#   tag <- shiny::downloadButton(...)
#   # tag$attribs$download <- NULL
#   tag
# }

#' @describeIn m_download  UI for the download module
#' 
m_download_ui <- function(id, label = "Export all data to Excel", ui_fn = downloadButton, ...) {
  ns <- NS(id)
  tagList(ui_fn(ns("dl_data"), label = label, ...))
}

#' @describeIn m_download  Server for the download module
#'
#' @importFrom stringr str_trunc
#' 
m_download_srv <- function(
  id,
  all_figs,
  excel_file = paste0("FiscalSim-Results-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".xlsx"),
  ...
) {
  moduleServer(id, function(input, output, session) {
    output$dl_data <- downloadHandler(
      filename = function() {
        return(basename(excel_file))
      },
      content = function(file) {
        
        # Check if file from all_figs$xlsx exists and return it
        if (file.exists(excel_file)) {    
          # browser()      
          # file.copy(file, excel_file, overwrite = T)
          file.copy(excel_file, file, overwrite = T)
          # return(file)
        } else
        {
        # browser()
          
        if (is.reactive(all_figs)) {
          all_figs <- all_figs()
        }
        
        withProgress(
          message = 'Preparing Excel file',
          detail = 'Saving figures and tables...',
          value = 0,
          {
            temp_file <-
              all_figs |>
              imap(
                ~ {
                  # Making clean sheet name
                  if (is.null(.x$sheet_name)) {
                    .x$sheet_name <- paste0("Sheet_", sample(letters, 20, replace = TRUE) |> paste0(collapse = ""))
                  }
                  .x$sheet_name <- stringr::str_trunc(.x$sheet_name, 30)

                  # Clean data table
                  if (is.null(.x$tbl)) {
                    .x$tbl <- .tibble(0)
                  }

                  # Clean metadata
                  if (is.null(.x$meta_tbl)) {
                    .x$meta_tbl <- tibble(o)
                  }

                  .x
                }
              ) |>
              pea_write_xlsx(
                progres_fn = function() {
                  incProgress(1 / (length(all_figs) + 1))
                }
              )
            file.copy(temp_file, excel_file, overwrite = T)
            file.copy(excel_file, file, overwrite = T)
          }
        )
        }
        }
    )
  })
}

#' @describeIn m_download  Test downlod app
#' 
test_download_app <- function(n = 3, ...) {

  all_args_0 <- yaml::yaml.load_file(here::here("data-app", "meta-fig-v2.yml"))

  ui <- shiny::fluidPage(m_download_ui("dl"))

  server <- function(input, output, session) {
    figures <-
      reactive({
        out <- all_args_0[1:n] |>
          imap(~ fct_load_test_figure(dta_test, .y, all_args = all_args_0))
        list(
          excel_file = file.path(tempdir(), paste0("FiscalSim-Results-", as.numeric(Sys.time()), ".xlsx")),
          dta = out
        )
      })
    m_download_srv("dl", all_figs = figures()$dta, excel_file = figures()$excel_file, ...)
  }

  shinyApp(ui, server)
}