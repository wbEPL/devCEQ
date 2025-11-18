testthat::skip()

# library(bslib)
# library(shiny)

# bslib::page_sidebar(
#   id = "main_sidebar_panel",
#   sidebar = bslib::sidebar(
#     title = NULL,
#     width = 350,
#     open = "always",
#     sb_content
#   ),
#   fig_content
# )

pkgload::load_all()
library(shiny)
library(shinyWidgets)
library(bslib)

# Inputs module --------------------------------------------
# test_m_input()

# Diagnostic module --------------------------------------------
# test_m_diagnostics()

# Figure module --------------------------------------------
# test_m_figure()

# Incidences module --------------------------------------------
test_m_incid(page_ui = f_incid_ui_linear)
test_m_incid(page_ui = f_incid_ui_card)

# Page switching --------------------------------------------
# test_m_res_switches()

# Insidences results module with switches and all results collection ----------
test_m_incid_switches()


# page_fluid(


  #   )
  #  tagList(
  #         uiOutput(ns("title")),
  #         fluidRow(
  #           #
  #           column(
  #             2,
  #             numericInput(
  #               ns("n_dec"),
  #               label = n_dec_label,
  #               value = 10,
  #               min = 5,
  #               max = 50
  #             )
  #           ),

  #           column(
  #             3,
  #             selectInput(
  #               ns("by_income"),
  #               label = dec_by_label,
  #               choices = set_names(get_inc_nm()$var, get_inc_nm()$var_title),
  #               selected = NULL,
  #               width = "350px"
  #             )
  #           ),

  #           column(#
  #             7,
  #             uiOutput(#
  #               ns(
  #                 "radioGroupButtons"
  #               )))
  #         ),

  #         tags$div(
  #           tags$div(
  #             shinyWidgets::dropMenu(
  #               shinyWidgets::actionBttn(
  #                 inputId = ns("ddmenue"),
  #                 style = "simple",
  #                 size = "xs",
  #                 color = "primary",
  #                 icon = icon("bars")
  #               ),
  #               actionButton(
  #                 inputId = ns("more"),
  #                 label = "Save plot",
  #                 icon = icon("download"),
  #                 #ph("download-simple"),
  #                 class = "btn-sm"
  #               )
  #             ),
  #             style = htmltools::css(
  #               position = "absolute",
  #               top = 0,
  #               left = "5px",
  #               zIndex = 30
  #             )
  #           ),

  #           shinycssloaders::withSpinner(plotly::plotlyOutput(ns(
  #             "incidence_ly"
  #           ))),

  #           style = htmltools::css(
  #             position = "relative",
  #             width = htmltools::validateCssUnit("100%"),
  #             height = htmltools::validateCssUnit("400px")
  #           )
  #         ),
  #         tags$hr(),
  #         column(12, shinycssloaders::withSpinner(DT::DTOutput(
  #           ns("incidence_dt")
  #         ))),
  #         tags$hr()
  #       )





  

mod_res_ui <- function(id) {
    ns <- NS(id)
    res_content <-
      tagList(
        shiny::actionButton(ns("browse"), label = "Browse the visualisation"),
        shiny::actionButton(ns("save"), label = "Save simulation raw data")
      )

    left_col <-
      list(
        shinyWidgets::radioGroupButtons(
          inputId = ns("results_tab_choice"),
          label = NULL,
          choices = c(
            "DEV/TEST" = "dev_test",
            "Poverty" = "tab_pov",
            "Inequality" = "tab_ineq",
            "Net Cash Position" = "tab_ncp",
            "Marginal contributions" = "tab_margins",

            "<hr class=\"hr-small-line\"/>" = "hr1",
            "Incidences:" = "ins",
            "Direct transfers" = "tab_dtr",
            "Subsidies  " = "tab_sub",
            "In-kind transfers" = "tab_ink",
            "<hr class=\"hr-small-line\"/>" = "hr2",
            "Direct taxes" = "tab_dtx",
            "Indirect taxes" = "tab_itx",
            "Contributions" = "tab_con" #,
          ),
          direction = "vertical",
          justified = TRUE,
          width = "100%"
        ) #,
        # mod_results_btns_ui(id)
      ) %>%
      wellPanel() %>%
      column(width = 3)

    right_col <-
      column(
        9,
        tabsetPanel(
          # tabPanelBody(value = "dev_test", developer_ui(ns("dev-srv"))),
          tabPanelBody(
            value = "tab_pov",
            local_gini_pov_ui1(ns("gin-pov-srv"))
          ),
          tabPanelBody(
            value = "tab_ineq",
            local_gini_pov_ui2(ns("gin-pov-srv"))
          ),
          tabPanelBody(
            value = "tab_ncp",
            devCEQ::mod_incidences_ui(ns("ncp-srv"))
          ),

          tabPanelBody(value = "tab_dtr", mod_incidences_ui(ns("tab_dtr"))),
          tabPanelBody(value = "tab_sub", mod_incidences_ui(ns("tab_sub"))),
          tabPanelBody(value = "tab_ink", mod_incidences_ui(ns("tab_ink"))),

          tabPanelBody(value = "tab_dtx", mod_incidences_ui(ns("tab_dtx"))),
          tabPanelBody(value = "tab_itx", mod_incidences_ui(ns("tab_itx"))),
          tabPanelBody(value = "tab_con", mod_incidences_ui(ns("tab_con"))),

          tabPanelBody(value = "tab_margins", mod_magins_ui(ns("tab_margins"))),

          id = ns("results_tab"),
          selected = "results_panel1",
          type = c("hidden"),
          header = NULL,
          footer = NULL
        )
      )

    tabPanel(
      title = "Results",
      if (golem::app_dev()) {
        res_content
      },
      extra_css,
      fluidRow(left_col, right_col)
    )
  }
