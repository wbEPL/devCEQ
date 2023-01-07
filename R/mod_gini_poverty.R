#' gini UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @export
#' @importFrom shiny NS tagList
mod_gini_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(
      uiOutput(ns("gini_ui"))
    )
  )
}

#' #' @noRd
#' #' @export
#' mod_poverty_ui <- function(id){
#'   ns <- NS(id)
#'   tagList(
#'     shinycssloaders::withSpinner(
#'       uiOutput(ns("poverty_ui"))
#'     )
#'   )
#' }
#'
#'
#' #' @noRd
#' #' @export
#' mod_gini_poverty_ui <- function(id) {
#'   ns <- NS(id)
#'   # ns_gini <- NS("gini")
#'   # ns_pov <- NS("pov")
#'   fluidRow(column(6, mod_gini_ui(id = ns("gini"))),
#'            column(6, mod_poverty_ui(id = ns("pov"))))
#' }

# mod_gini_poverty_server <-
#   function(id, sim_res = reactive(NULL), ...) {
#     moduleServer(id, function(input, output, session) {
#       mod_gini_server("gini", sim_res, ...)
#       mod_poverty_server("pov", sim_res, ...)
#     })
#   }

#' gini Server Functions
#'
#' @noRd
#' @importFrom plotly renderPlotly plotlyOutput
#' @importFrom DT DTOutput renderDT
#' @importFrom htmltools css validateCssUnit
#' @import esquisse
#' @noRd
#' @export
mod_gini_pov_gen_server <-
  function(id,
           sim_res = reactive(NULL),
           title = "Inequality",
           export_btn_title = "Save plot",
           pl_choices = NULL,
           # c(
           #   "Seuil pauvrete national" = "zref",
           #   "International poverty line 5.5 USD (2011 PPP)" = "line_55",
           #   "International poverty line 3.2 USD (2011 PPP)" = "line_32",
           #   "International poverty line 1.9 USD (2011 PPP)" = "line_19"
           # ),
           pl_title = "Poverty line",
           get_dta_fn = function(dta, policy_name, ...) {
             get_dta_gini(
               dta = dta,
               policy_name = policy_name,
               income_vars_tbl = get_inc_nm(),
               wt_var = get_wt_nm(),
               para_names =
                 tibble(
                   parameter = c("Gini", "Theil"),
                   label = c("Gini index", "Theil index")
                 ),
               ...
             )
           },
           make_plot_fn = function(dta, ...) {
             make_gini_pov(
               dta = dta,
               y = NULL,
               x = Income,
               color = Simulation,
               title = "Inequality",
               ylab = NULL,
               xlab = "Income",
               ...
             )
           },

           ...) {

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$gini_ui <- renderUI({
      validate(
        need(sim_res(), "Click `Run` on the policy choices tab to see the results.")
        )
      req(sim_res())

      if (isTruthy(pl_choices)) {
        btn_ui <-
          fluidRow(#
            column(
              4,
              shiny::selectInput(
                inputId = ns("povLine"),
                label = pl_title,
                choices = pl_choices
              )
            ),
            column(8, uiOutput(ns("radioGroupButtons")))
          )
      } else {
        btn_ui <- fluidRow(column(12, uiOutput(ns("radioGroupButtons"))))
      }

      tagList(
        tags$h4(title),

        btn_ui,

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
                icon = icon("download"), #ph("download-simple"),
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
          shinycssloaders::withSpinner(plotly::plotlyOutput(ns("ineq_ly"))),
          style = htmltools::css(
            position = "relative",
            width = htmltools::validateCssUnit("100%"),
            height = htmltools::validateCssUnit("400px")
          )
        ),
        tags$hr(),
        # tags$h4("Inequality, Gini index"),
        column(12, shinycssloaders::withSpinner(DT::DTOutput(ns("ineq_dt")))),
        tags$hr()
      )
    })

    # Poverty line choice if available ----------------------------------------
    pl_choice <- reactive({
      if (isTruthy(pl_choices) & !isTruthy(input$povLine)) {
        return(pl_choices[[1]])
      } else if (isTruthy(pl_choices) & isTruthy(input$povLine)) {
        return(input$povLine)
      } else {
        return(NULL)
      }
    }) %>% debounce(500)

    # Statistics estimates ----------------------------------------------------
    stat_esimates <-
      reactive({
        req(sim_res())
        sim_res() %>%
          purrr::map( ~ {
            get_dta_fn(
              dta = .x$policy_sim_raw,
              policy_name = .x$policy_name,
              poverty_line_var = pl_choice()
            )
          }) %>%
          make_plot_fn()
      })

    # Plot selector Button ----------------------------------------------------
    plt_indx_upd <- reactiveVal(NULL)
    radioGroupButtons_ui <- reactiveVal(NULL)
    observe({
      req(stat_esimates()$plt_indx)
      isolate({
        if (!isTruthy(plt_indx_upd())) {
          shinyWidgets::radioGroupButtons(
            inputId = ns("plot_var"),
            label = NULL,
            choices = stat_esimates()$plt_indx,
            selected = stat_esimates()$plt_indx[[1]],
            size = "sm",
            status = "primary",
            individual  = TRUE,
            direction = "horizontal",
          ) %>%
            radioGroupButtons_ui()
          plt_indx_upd(stat_esimates()$plt_indx)
        }
      })
    })

    output$radioGroupButtons <- renderUI({req(radioGroupButtons_ui())})

    # Currently selected plot --------------------------------------------------
    cur_plot_var <- reactive({
      if (!isTruthy(input$plot_var)) {
        return(1)
      } else {
        return(input$plot_var)
      }
    })

    # Plotly plotting ---------------------------------------------------------
    output$ineq_ly <- renderPlotly({
      stat_esimates()$ly[[cur_plot_var()]] %>%
        validate_result() %>%
        plotly_config()
    })

    # DT print ---------------------------------------------------------
    output$ineq_dt <- DT::renderDT({
      dta <- stat_esimates()$tbl_dt
      dta %>%
        validate_result() %>%
        fct_config_export_dt(file_title = title) %>%
        DT::formatRound(columns = names(dta)[sapply(dta, is.numeric)],
                        digits = 4)
    })

    # GGPlot export ----------------------------------------------------
    rv <- reactiveValues(plot = NULL)
    observe({
      req(cur_plot_var())
      req(stat_esimates()$gg[[cur_plot_var()]])
      rv$plot <- stat_esimates()$gg[[cur_plot_var()]]
    })
    observeEvent(input$more, {
      esquisse::save_ggplot_modal(id = session$ns("export"), title = export_btn_title)
    })
    save_ggplot_server2("export", plot_rv = rv, dpi = 450, scale = 2)

    stat_esimates
  })
}


#' #' poverty Server Functions
#' #'
#' #' @noRd
#' #' @importFrom shinyWidgets dropMenu actionBttn
#' mod_poverty_server <- function(id, sim_res = reactive(NULL), ...){
#'   moduleServer( id, function(input, output, session){
#'     ns <- session$ns
#'
#'
#'     # Poverty ---------------------------------------------------------------
#'     output$poverty_ui <- renderUI({
#'       req(sim_res())
#'       tagList(
#'         tags$h4("Poverty"),
#'         tags$div(
#'           tags$div(
#'             shinyWidgets::dropMenu(
#'               shinyWidgets::actionBttn(
#'                 inputId = ns("ddmenue"),
#'                 style = "simple",
#'                 size = "xs",
#'                 color = "primary",
#'                 icon = icon("bars")
#'               ),
#'               shiny::selectInput(
#'                 inputId = ns("povLine"),
#'                 label = "Poverty line",
#'                 choices = c("National poverty line" = "pl_nat",
#'                             "3.2 PPP$ per day" = "pl_320")
#'               ),
#'               actionButton(
#'                 inputId = ns("more"),
#'                 label = "Save plot",
#'                 icon = icon("download")
#'               )
#'             ),
#'             style = htmltools::css(
#'               position = "absolute",
#'               top = 0,
#'               left = "5px",
#'               zIndex = 30
#'             )
#'           ),
#'           shinycssloaders::withSpinner(plotly::plotlyOutput(ns("pov_ly"))),
#'           style = htmltools::css(
#'             position = "relative",
#'             width = htmltools::validateCssUnit("100%"),
#'             height = htmltools::validateCssUnit("400px")
#'           )
#'         ),
#'         tags$hr(),
#'         tags$h4("Povert rate, %"),
#'         column(12, shinycssloaders::withSpinner(DT::DTOutput(ns("pov_dt")))),
#'         tags$hr()
#'       )
#'     })
#'
#'     pov_esimates <-
#'       reactive({
#'         req(sim_res())
#'
#'         if (!isTruthy(input$povLine) |
#'             isTruthy(input$povLine) && !input$povLine %in% c("pl_nat", "pl_320")) {
#'           pov_line_var <- "pl_nat"
#'         } else {
#'           pov_line_var <- input$povLine
#'         }
#'
#'         sim_res() %>%
#'           purrr::map( ~ {
#'             get_dta_pov(
#'               .x$policy_sim_raw,
#'               .x$policy_name,
#'               poverty_line_var = pov_line_var,
#'               wt_var = "pcweight",
#'               income_vars_tbl = c("yp_pc", "yn_pc", "yg_pc", "yc_pc") %>% get_var_nm()
#'             )
#'           }) %>%
#'           make_gini_pov(
#'             y = Poverty,
#'             x = Income,
#'             color = Simulation,
#'             title = "Poverty",
#'             ylab = "Poverty rate, %",
#'             xlab = "Income"
#'           )
#'       })
#'
#'     output$pov_ly <- renderPlotly({
#'       pov_esimates()$ly %>% validate_result() %>% plotly_config()
#'     })
#'     output$pov_dt <- DT::renderDT({
#'       dta <- pov_esimates()$tbl_dt
#'       dta %>%
#'         validate_result() %>%
#'         fct_config_export_dt("Poverty") %>%
#'         DT::formatRound(columns = names(dta)[sapply(dta, is.numeric)],
#'                         digits = 2)
#'     })
#'
#'
#'     # Plot export modal ----------------------------------------------------
#'     rv <- reactiveValues(plot = NULL)
#'     observe({rv$plot <- pov_esimates()$gg})
#'     observeEvent(input$more, {
#'       esquisse::save_ggplot_modal(id = session$ns("export"),
#'                                   title = "Export chart")
#'     })
#'     save_ggplot_server2("export", plot_rv = rv, dpi = 450, scale = 3)
#'
#'     # Retun
#'     pov_esimates
#'   })
#' }

