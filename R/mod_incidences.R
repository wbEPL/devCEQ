#' Insidences module
#' 
#' @name mod_incidences
#' 
NULL

#' @describeIn mod_incidences New incidences UI placeholder
#' 
#' @importFrom shiny NS uiOutput
#' @importFrom shinycssloaders withSpinner
#' 
mod_incidences_ui <- 
  function(id) {
    ns <- NS(id)
    uiOutput(ns("incidences_ui")) |> shinycssloaders::withSpinner() 
    # tags$div(style = "overflow-y: auto; height: calc(100vh - 8rem); overflow-x: hidden;") %>%
    # tagList()
  }


#' insidences Server Functions OLD
#'
#' @noRd
#' @importFrom shinyWidgets radioGroupButtons
#' @export
mod_incidences_server <-
  function(id,
           sim_res,
           dec_vars = get_var_nm()$var,
           make_bar_fn = make_bar_dta,
           n_dec_label = "Number of deciles",
           dec_by_label = "Deciles by:",
           tab_title = NULL,
           ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      output$incidence_ui <- renderUI({
        req(sim_res())

        tagList(
          uiOutput(ns("title")),
          fluidRow(
            #
            column(
              2,
              numericInput(
                ns("n_dec"),
                label = n_dec_label,
                value = 10,
                min = 5,
                max = 50
              )
            ),

            column(
              3,
              selectInput(
                ns("by_income"),
                label = dec_by_label,
                choices = set_names(get_inc_nm()$var, get_inc_nm()$var_title),
                selected = NULL,
                width = "350px"
              )
            ),

            column(#
              7,
              uiOutput(#
                ns(
                  "radioGroupButtons"
                )))
          ),

          tags$div(
            tags$div(
              shinyWidgets::dropMenu(
                shinyWidgets::actionBttn(
                  inputId = ns("ddmenue"),
                  style = "simple",
                  size = "xs",
                  color = "primary",
                  icon = icon("bars")
                ),
                actionButton(
                  inputId = ns("more"),
                  label = "Save plot",
                  icon = icon("download"),
                  #ph("download-simple"),
                  class = "btn-sm"
                )
              ),
              style = htmltools::css(
                position = "absolute",
                top = 0,
                left = "5px",
                zIndex = 30
              )
            ),

            shinycssloaders::withSpinner(plotly::plotlyOutput(ns(
              "incidence_ly"
            ))),

            style = htmltools::css(
              position = "relative",
              width = htmltools::validateCssUnit("100%"),
              height = htmltools::validateCssUnit("400px")
            )
          ),
          tags$hr(),
          column(12, shinycssloaders::withSpinner(DT::DTOutput(
            ns("incidence_dt")
          ))),
          tags$hr()
        )
      })

      # Incidences table --------------------------------------------------------
      n_dec_debounced <-
        reactive(req(input$n_dec)) %>% debounce(1000)

      incidence_esimates <-
        reactive({
          req(sim_res())
          req(n_dec_debounced())
          req(input$by_income)
          out <- try({
            sim_res() %>%
              agg_sims_by_deciles(
                dec_by = input$by_income,
                dec_vars = dec_vars,
                wt_var = get_wt_nm(),
                n_dec = n_dec_debounced(),
                get_var_fn = get_var_nm,
                ...
              ) %>%
              make_bar_fn()
          }, silent = T)
          out %>%
            validate_result(str_c("Erro occured when estimating the incidencesin in ",
                                  ns("")))
        })

      output$title <-
        renderUI({
          req(incidence_esimates())
          if (is.null(tab_title)) {
            incidence_esimates()$title %>% h3()
          } else {
            tab_title |> h3()
          }
        })


      # Updating plots selector
      plt_indx_upd <- reactiveVal(NULL)
      radioGroupButtons_ui <- reactiveVal(NULL)
      observe({
        req(incidence_esimates()$plt_indx)
        isolate({
          if (!isTruthy(plt_indx_upd())) {
            shinyWidgets::radioGroupButtons(
              inputId = ns("plot_var"),
              label = NULL,
              choices = incidence_esimates()$plt_indx,
              selected = incidence_esimates()$plt_indx[[1]],
              status = "primary",
              size = "sm",
              individual  = TRUE,
              direction = "horizontal",
            ) %>% radioGroupButtons_ui()
            plt_indx_upd(incidence_esimates()$plt_indx)
          }
        })
      })

      output$radioGroupButtons <-
        renderUI({
          req(radioGroupButtons_ui())
        })

      cur_plot_var <- reactive({
        if (!isTruthy(input$plot_var)) {
          return(dec_vars[[1]])
        } else {
          return(input$plot_var)
        }
      })
      output$incidence_ly <- renderPlotly({
        incidence_esimates()$ly[[cur_plot_var()]] %>% validate_result() %>% plotly_config()
      })

      output$incidence_dt <- DT::renderDT({
        dta <- incidence_esimates()$tbl_dt
        dta %>%
          validate_result() %>%
          fct_config_export_dt("Incidences") #%>%
        # DT::formatRound(columns = names(dta)[sapply(dta, is.numeric)],
        #                 digits = 2)
      },
      server = FALSE)

      # Plot export modal ----------------------------------------------------
      rv <- reactiveValues(plot = NULL)
      observe({
        req(input$plot_var)
        req(incidence_esimates()$gg[[input$plot_var]])
        rv$plot <- incidence_esimates()$gg[[input$plot_var]]
      })

      observeEvent(input$more, {
        esquisse::save_ggplot_modal(id = session$ns("export"),
                                    title = "Export chart")
      })
      save_ggplot_server2("export",
                          plot_rv = rv,
                          dpi = 450,
                          scale = 2)

      incidence_esimates
    })

  }

## To be copied in the UI
# mod_insidences_ui("insidences_1")

## To be copied in the server
# mod_insidences_server("insidences_1")
