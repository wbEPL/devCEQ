#' Results module building blocks
#' 
#' @name m_res
NULL

#' @describeIn m_res Sidebar switcehd UI Function
#' 
#' @importFrom shiny NS
#' @importFrom shinyWidgets radioGroupButtons
#' 
#' 
m_res_switches_ui <- function(
  id = NULL,
  panels_choices = f_res_switches_val(),
  ...
) {
  ns <- NS(id)
  shinyWidgets::radioGroupButtons(
    inputId = ns("results_tab_choice"),
    label = NULL,
    choices = panels_choices,
    direction = "vertical",
    justified = TRUE,
    width = "100%",
    ...
  )
}


#' @describeIn m_res Sidebar switches server
#' 
#' @importFrom shiny moduleServer observe req updateTabsetPanel
#' @importFrom shinyWidgets updateRadioGroupButtons
#' 
m_res_switches_srv <- function(id, active_tab = reactive(NULL), ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # Switching tabs ---------------------------------------------------
      observe({
        # req(active_tab())
        shinyWidgets::updateRadioGroupButtons(
          session,
          "results_tab_choice",
          disabledChoices = c("ins", "hr1", "hr2")
        )
      })

      ### TAB switch logic ----------------------------------------------
      observeEvent(
        input$results_tab_choice,
        {
          bslib::nav_select(
            "results_tab",
            input$results_tab_choice,
            session = session
          )
        },
        ignoreInit = TRUE
      )


    })
}


#' @describeIn m_res Content container UI Function
#' 
#' @importFrom shiny NS
#' @importFrom bslib navset_hidden
#' 
m_res_content_ui <- function(id, panle_ui, ...) {
  ns <- NS(id)
  bslib::navset_hidden(
    id = ns("results_tab"),
    !!!panle_ui,
    ...
  )
}


#' @describeIn m_res Helper function returning the sidebar choices
#' 
f_res_switches_val <- function() {
  c(
    "DEV/TEST" = "dev_test",
    "Poverty" = "tab_pov",
    "Inequality" = "tab_ineq",
    "Net Cash Position" = "tab_ncp",
    "<hr class=\"hr-small-line\"/>" = "hr1"
  )
}

#' @describeIn m_res Helper function returning the content panels
#' 
f_res_switches_cont <- function() {
  f_res_switches_val () |>
  purrr::imap(
    ~ bslib::nav_panel_hidden(value = .x, bslib::card(h2(.y)))
  ) |> 
  unname()
}

#' @describeIn m_res Test function for the results module with switches
#' @importFrom bslib bs_theme page_fluid
#' 
test_m_res_switches <- function(
  panels_choices = f_res_switches_val(),
  panle_ui = f_res_switches_cont()

) {
  page_ui <-
    page_fluid(
      theme = bs_theme(),
      layout_columns(
        widths = c(3, 9),
        m_res_switches_ui("res-srv", panels_choices = panels_choices),
        m_res_content_ui("res-srv", panle_ui = panle_ui)
      )
    )

  page_srv <-
    function(input, output, session) {
      m_res_switches_srv("res-srv")
    }

  shiny::runApp(shinyApp(page_ui, page_srv))
}

#' results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @export
#' @importFrom shiny NS tagList 
mod_res_ui <- function(id) {
  ns <- NS(id)
  res_content <-
    tagList(
      shiny::actionButton(ns("browse"), label = "Browse the visualisation"),
      shiny::actionButton(ns("save"), label = "Save simulation raw data"))
  
  left_col <-
    list(
      shinyWidgets::radioGroupButtons(
        inputId = ns("results_tab_choice"),
        label = NULL,
        choices = c(
          "DEV/TEST" = "dev_test",
          "Poverty"                        = "tab_pov",
          "Inequality"                     = "tab_ineq",
          "Net Cash Position"              = "tab_ncp",
          "Marginal contributions"         = "tab_margins",
          
          "<hr class=\"hr-small-line\"/>"  = "hr1",
          "Incidences:"                    = "ins",
          "Direct transfers"               = "tab_dtr",
          "Subsidies  "                    = "tab_sub",
          "In-kind transfers"              = "tab_ink",
          "<hr class=\"hr-small-line\"/>"  = "hr2",
          "Direct taxes"                   = "tab_dtx",
          "Indirect taxes"                 = "tab_itx",
          "Contributions"                 = "tab_con"#,
        ),
        direction = "vertical",
        justified = TRUE, 
        width = "100%"
      )#,
      # mod_results_btns_ui(id)
    ) %>% 
    wellPanel() %>%
    column(width = 3)
  
  right_col <- 
    column(
      9,
      tabsetPanel(
        # tabPanelBody(value = "dev_test", developer_ui(ns("dev-srv"))),
        tabPanelBody(value = "tab_pov", local_gini_pov_ui1(ns("gin-pov-srv"))),
        tabPanelBody(value = "tab_ineq", local_gini_pov_ui2(ns("gin-pov-srv"))),
        tabPanelBody(value = "tab_ncp", devCEQ::mod_incidences_ui(ns("ncp-srv"))),
        
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
      ))
  
  extra_css <- 
    
    tags$head(
      tags$style(HTML("
      
.tooltip-disabled {
  position: relative;
  display: inline-block;
}

.tooltip-enabled {
  position: relative;
  display: inline-block;
}

.tooltiptext {visibility: hidden;}
.tooltip-disabled .tooltiptext {
  visibility: hidden;
  width: 120px;
  background-color: black;
  color: #fff;
  text-align: center;
  border-radius: 6px;
  padding: 5px 0;

  /* Position the tooltip */
  position: absolute;
  z-index: 1;
}

.tooltip-disabled:hover .tooltiptext {
  visibility: visible;
}

    "))
    )
  
  tabPanel(title = "Results",
           if (golem::app_dev()) {res_content},
           extra_css,
           fluidRow(left_col, right_col))
  
}


#' results Server Functions
#'
#' @noRd
#' @export
mod_res_test_server <- function(
  id,
  sim_res = reactive(list()),
  postsim_res = reactive(list()),
  active_tab = reactive(NULL),
  presim = reactive(NULL),
  ...
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # observeEvent(
    #   sim_res(),
    #   {
    #     if (!isTruthy(sim_res())) {
    #       shinyjs::addClass(
    #         selector = "#main_sidebar li:nth-child(3)",
    #         class = "tooltip-disabled"
    #       )
    #       shinyjs::html(
    #         selector = "#main_sidebar li:nth-child(3)",
    #         html = '<span class="tooltiptext" id="ToolTipContent">Press Run to generate results.</span>',
    #         add = TRUE,
    #         asis = TRUE
    #       )
    #       shinyjs::disable(selector = "#main_sidebar li:nth-child(3)")
    #     } else {
    #       removeUI(selector = "#ToolTipContent")
    #       shinyjs::removeClass(
    #         selector = "#main_sidebar li:nth-child(3)",
    #         class = "tooltip-disabled"
    #       )
    #       shinyjs::addClass(
    #         selector = "#main_sidebar li:nth-child(3)",
    #         class = "tooltip-enabled"
    #       )
    #       shinyjs::enable(selector = "#main_sidebar li:nth-child(3)")
    #     }
    #   },
    #   ignoreInit = FALSE,
    #   ignoreNULL = FALSE
    # )

    # Switching tabs ---------------------------------------------------
    observe({
      req(active_tab())
      shinyWidgets::updateRadioGroupButtons(
        session,
        "results_tab_choice",
        disabledChoices = c("ins", "hr1", "hr2")
      )
    })

    ### TAB switch logic ----------------------------------------------
    observe({
      req(input$results_tab_choice)
      shiny::updateTabsetPanel(
        session,
        "results_tab",
        selected = input$results_tab_choice
      )
    })

    # Error message -------------------------------------------
    sim_res_local <- reactive({
      validate(need(
        sim_res(),
        "Click `Run` on the policy choices tab to see the results."
      ))
      # browser()
      # readr::write_rds(sim_res(), "data-temp/sim_res_2.rds")
      sim_res()
    })

    # DEBUGGING - save policy results locally -------------------------------
    observeEvent(
      input$save,
      {
        req(sim_res())
        dir.create("data-temp", showWarnings = F)
        readr::write_rds(sim_res(), "data-temp/sim_res.rds")
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$browse,
      {
        req(sim_res())
        # golem::browser_dev({
        #   sim_res()
        #
        # })
        # browser()
        # dir.create("data-temp", showWarnings = F)
        # readr::write_rds(sim_res(), "data-temp/sim_res.rds")
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Development module ----------------------------------------------------
    # mod_developer_server("dev-srv", sim_res = sim_res_local, ...)

    # Poverty and Inequality ------------------------------------------------
    local_gini_pov_server("gin-pov-srv", sim_res = sim_res_local, ...)

    # # # Net Cash Position -----------------------------------------------------
    local_mod_ncp_ser(
      id = "ncp-srv",
      sim_res = sim_res_local,
      dec_vars = c(
        "dtr_pc",
        "itx_pc",
        "dtx_pc",
        "sub_pc",
        "con_pc",
        "hlt_pc",
        "edu_pc"
      )
    )

    # # # incidences ------------------------------------------------------------
    local_mod_inc_ser(
      id = "tab_dtr",
      sim_res = sim_res_local,
      dec_vars = c(
        "dtr_pc",
        "dtr_cash_pc",
        "dtr_work_pc",
        "dtr_hous_pc",
        "dtr_empf_pc"
      )
    )

    local_mod_inc_ser(
      id = "tab_sub",
      sim_res = sim_res_local,
      dec_vars = c(
        "sub_pc",
        "sub_elec_pc",
        "sub_delec_pc",
        "sub_ielec_pc",
        "sub_fuel_pc",
        "sub_ifuel_pc",
        "sub_dfuel_pc"
      )
    )

    local_mod_inc_ser(
      id = "tab_ink",
      sim_res = sim_res_local,
      dec_vars = c(
        "hlt_pc",
        "edu_pc",
        "hlt_prim_pc",
        "hlt_hosp_pc",
        "edu_prim_pc",
        "edu_seco_pc",
        "edu_tert_pc"
      )
    )

    local_mod_inc_ser(
      id = "tab_dtx",
      sim_res = sim_res_local,
      dec_vars = c(
        "dtx_pc",
        "dtx_pitx_pc",
        "dtx_wage_pc",
        "dtx_mova_pc",
        "dtx_rent_pc",
        "dtx_avtx_pc"
      )
    )

    local_mod_inc_ser(
      id = "tab_itx",
      sim_res = sim_res_local,
      dec_vars = c(
        "itx_pc",
        "itx_vatx_pc",
        "itx_dvat_pc",
        "itx_ivat_pc",
        "itx_excs_pc",
        "itx_exca_pc",
        "itx_exct_pc",
        "itx_exco_pc",
        "itx_cust_pc",
        "itx_dcus_pc",
        "itx_icus_pc"
      )
    )

    local_mod_inc_ser(
      id = "tab_con",
      sim_res = sim_res_local,
      dec_vars = c("con_pc", "con_hous_pc", "con_empf_pc")
    )

    # ## Fiscal impact --------------------------------------------------------
    # mod_fiscal_server(id = "fiscal_mod", sim_res = sim_res_local, presim = presim, ...)

    # ## Marginal contributions --------------------------------------------------------
    mod_magins_server(
      id = "tab_margins",
      sim_res = sim_res_local,
      presim = presim,
      ...
    )
  })
}


## To be copied in the UI
# mod_results_ui("results_1")

## To be copied in the server
# mod_results_server("results_1")

#' Tests a simple results page module providing it with the sim_res
#'
#' @export
test_results_mod <-
  function(sim_res = NULL,
           ui_side,
           server_side,
           id = "testid", 
           ...) {
    ui <- fluidPage(ui_side(id = id))
    srv <-
      function(input, output, session) {
        server_side(id = id, sim_res = reactive(sim_res), ...)
      }
    shinyApp(ui, srv)
    
  }