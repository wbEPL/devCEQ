#' dev_res displays and allows to download development results of the app.
#'
#' @description only active if `options(ceq_results_dev = TRUE)`.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom haven write_dta
#'
#' @export
mod_dev_res_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(
      4,
      h2("Inputs"),
      shiny::verbatimTextOutput(ns("inps"))
    ),
    column(
      8,
      shiny::uiOutput(ns("download_btns")),
      h2("Results"),
      shiny::verbatimTextOutput(ns("outps"))
    )
  ) %>%
    fluidPage()
}


#' @describeIn mod_dev_res_ui Server side of the module
#'
#' @noRd
#' @export
mod_dev_res_server <- function(id, sim_res = reactive(NULL)){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # observe({
    #   req(sim_res())
    #   sim_res() %>% map("policy_sim_raw") %>% str() %>% cat()
    #   browser()
    # })

    # "Inputs structure"
    output$inps <- renderPrint({
      validate(need(sim_res(),
                    message = "To see development results, Run the simulation first"))
      base_policy <- sim_res()$policy0$policy_choices %>% unlist()
      sim_res() %>%
        imap( ~ {
          list(
            policy_id = .y,
            policy_name = .x$policy_name,
            policy_choices_number = length(unlist(.x$policy_choices)),
            policy_diff_base = unlist(.x$policy_choices)[unlist(.x$policy_choices) != base_policy]
          )
        }) %>%
        str()
      })

    # "outps structure"
    output$outps <- renderPrint({
      validate(need(sim_res(),
                    message = "To see development results, Run the simulation first"))
      sim_res() %>%
        imap( ~ {
          list(
            policy_id = .y,
            policy_sim_raw = .x$policy_sim_raw
          )
        }) %>%
        str()
      })

    # Buttons
    output$download_btns <-
      renderUI({
        req(sim_res())
        sim_res() %>%
          imap(~{
            downloadButton(
              outputId = ns(str_c(.y, "dwnload_dta")),
              label = str_c("'",  .x$policy_name,"' in .dta")
            )
          }) %>%
          tagList()
      })

    observe({
      req(sim_res())
      sim_res() %>%
        iwalk(~{
          output[[str_c(.y, "dwnload_dta")]] <<-
            downloadHandler(
              filename = function() {str_c(.y, "_sim_res.dta")},
              content = function(file) {haven::write_dta(.x$policy_sim_raw, path = file)}
            )
        })
    })

  })
}
