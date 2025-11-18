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


#' @describeIn mod_incidences New incidence server logic
#'
#' @param ndec_type Type of input for number of deciles: "numericInput" or "selectInput"
#' 
m_incidences_srv <-
  function(
    id,
    sim_res,
    page_ui = function(id) {
      ns <- NS(id)
      tagList(
        # m_title_ui(id),
        m_input_ui(ns("title")),
        layout_columns(
          m_input_ui(ns("ndec")),
          m_input_ui(ns("decby")),
          m_input_ui(ns("grpby"))
        ),
        layout_columns(
          m_input_ui(ns("pltby"))
        ),
        uiOutput(ns("page_card")),
        uiOutput(ns("dev_info"))
      )
    },
    # dec_vars = get_var_nm()$var,
    # make_bar_fn = make_bar_dta,

    page_title = "Incidences",

    ndec_type = "selectInput", #"numericInput"
    ndec_title = "Number of deciles:",
    ndec_choices = c(5, 10, 25, 50, 100) |> as.integer(),
    
    decby_type = "selectInput",
    decby_title = "Deciles by:",
    decby_choices = f_var_names_vector(get_inc_nm()),

    grpby_type = "selectizeInput", #"numericInput"
    grpby_title = "Compare by:",
    grpby_choices = c("Simulation" = "sim_id", f_var_names_vector(get_var_nm(c("group_1", "group_2", "group_3")))) , # c("Simulation" = "sim_id"),

    pltby_type = "radioGroupButtons",
    pltby_title = NULL,
    ...
  ) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # Step 1. Page structure -------------------------------------------------------
      output$incidences_ui <- renderUI(page_ui(id))

      # Step 2.a Title
      ptitle <- m_input_srv("title", "title", title = reactive(page_title), choices = reactive(page_title))

      # Step 2.b Number of deciles
      ndec <- m_input_srv("ndec", ndec_type, ndec_title, ndec_choices)
      decby <- m_input_srv("decby", "selectInput", decby_title, decby_choices)
      grpby <- m_input_srv("grpby", grpby_type, grpby_title, grpby_choices)
      pltby <- m_input_srv("pltby", pltby_type, pltby_title, reactive(fig()$title))

      # Step 3 Generating plots ------------------------------------------------
      #TODO

      fig <- reactiveVal(
        list(
          id = c("Fig 1", "Fig 2", "Fig 3"),
          title = c("Figure 1", "Figure 2", "Figure 3"),
          ly = list(),
          gg = list(fig_gg_random(), fig_gg_random(), fig_gg_random()) |> 
            set_names(c("Fig 1", "Fig 2", "Fig 3")),
          tbl_dt = tibble(1:10)
        )
      )

      # Step 10. Card with plot and data table --------------------------------
      #TODO

      # Step 20. Results export modal ----------------------------------------
      #TODO

      # Step 80. Development context info ----------------------------------------
      output$dev_info <- renderUI({
        if (in_devmode()) {
          verbatimTextOutput(ns("page_dev"))
        }
      })

      output$page_dev <- renderPrint({
        if (in_devmode()) {
          str(
            list(
              inputs = list(
                ptitle = ptitle(),
                ndec = ndec(),
                decby = decby(),
                grpby = grpby(),
                pltby = pltby()
              ),
              ggs = list(),
              fts = list(),
              dta = list()
            )
          )
        }
      })

      # Step 90. Return reactive values ----------------------------------------
      list(
        ndec = ndec,
        decby = decby,
        grpby = grpby,
        pltby = pltby
      )

    })
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
