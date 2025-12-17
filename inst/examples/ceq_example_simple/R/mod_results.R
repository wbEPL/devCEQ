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
mod_results_ui <- function(id) {
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
          "Pauvreté"     = "results_panel1",
          "Inégalité"     = "results_panel1a",
          "<hr class=\"hr-small-line\"/>" = "hr1",
          "Situation de trésorerie nette" = "ncp_panel",

          "Incidences:" = "ins",

          "Transfert direct"           = "am_tot1_panel",
          # "Programmes CMU"        = "am_tot2_panel",
          "Subventions"   = "subs_tot_panel",
          "Transferts en nature"            = "inkind_panel",
          # "Éducation en nature"        = "education_inKind_panel",

          "<hr class=\"hr-small-line\"/>" = "hr2",

          "Impôts directs"               = "income_tax_panel",
          # "Droits d'accises"             = "excise_taxes_panel",
          "Taxes Indirectes"   = "Tax_TVA_panel",
          "Cotisations"                  = "cs_total_panel"
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
        tabPanelBody(value = "results_panel1", local_gini_pov_ui1(ns("gin-pov-srv"))),
        tabPanelBody(value = "results_panel1a", local_gini_pov_ui2(ns("gin-pov-srv"))),

        tabPanelBody(value = "am_tot1_panel", mod_incidences_ui(ns("am_tot1"))),
        # tabPanelBody(value = "am_tot2_panel", mod_incidences_ui(ns("am_tot2"))),
        tabPanelBody(value = "subs_tot_panel", mod_incidences_ui(ns("subs_tot"))),
        tabPanelBody(value = "inkind_panel", mod_incidences_ui(ns("inkind"))),
        # tabPanelBody(value = "education_inKind_panel", mod_incidences_ui(ns("education_inKind"))),
        tabPanelBody(value = "income_tax_panel", mod_incidences_ui(ns("income_tax"))),
        # tabPanelBody(value = "excise_taxes_panel", mod_incidences_ui(ns("excise_taxes"))),
        tabPanelBody(value = "Tax_TVA_panel", mod_incidences_ui(ns("Tax_TVA"))),
        tabPanelBody(value = "cs_total_panel", mod_incidences_ui(ns("cs_total"))),

        tabPanelBody(value = "ncp_panel", mod_incidences_ui(ns("ncp"))),

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
mod_results_server <- function(id,
                                   sim_res = reactive(list()),
                                   postsim_res = reactive(list()),
                                   active_tab = reactive(NULL),
                                   ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Switching tabs ---------------------------------------------------
    observe({
      req(active_tab())
      shinyWidgets::updateRadioGroupButtons(
        session,
        "results_tab_choice",
        disabledChoices = c("ins", "hr1", "hr2"))
    })

    ### TAB switch logic ----------------------------------------------
    observe({
      req(input$results_tab_choice)
      shiny::updateTabsetPanel(session, "results_tab",
                               selected = input$results_tab_choice)
    })

    # Error message -------------------------------------------
    sim_res_local <- reactive({
      validate(need(sim_res(),
                    "Click `Run` on the policy choices tab to see the results."))
      # browser()
      # readr::write_rds(sim_res(), "data-temp/sim_res_2.rds")
      sim_res()
    })

    # DEBUGGING - save policy results locally -------------------------------
    observeEvent(input$save, {
      req(sim_res())
      dir.create("data-temp", showWarnings = F)
      readr::write_rds(sim_res(), "data-temp/sim_res.rds")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input$browse, {
      req(sim_res())
      # golem::browser_dev({
      #   sim_res()
      #
      # })
      # browser()
      # dir.create("data-temp", showWarnings = F)
      # readr::write_rds(sim_res(), "data-temp/sim_res.rds")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Poverty and Inequality ------------------------------------------------
    local_gini_pov_server("gin-pov-srv", sim_res = sim_res_local, ...)

    # Net Cash Position -----------------------------------------------------
    local_mod_ncp_ser(
      id = "ncp",
      sim_res = sim_res_local,
      dec_vars = c(
        "dtx_total",
        "itx_total",
        "pen"
        )
      )

    # # incidences ------------------------------------------------------------
    # local_mod_inc_ser(id = "am_tot1", sim_res = sim_res_local,
    #                       dec_vars = c("dirtransf_total_pc", "am_BNSF_pc", "am_Cantine_pc",
    #                                    "am_bourse_pc", "am_subCMU_pc", "am_sesame_pc", "am_moin5_pc", "am_cesarienne_pc"))

    # local_mod_inc_ser(id = "am_tot2", sim_res = sim_res_local,
    #                       dec_vars = c("am_CMU_progs_pc", "am_sesame_pc", "am_moin5_pc", "am_cesarienne_pc"))

    # local_mod_inc_ser(id = "subs_tot", sim_res = sim_res_local,
    #                       dec_vars = c(
    #                         "subsidy_total_pc", "subsidy_elec_pc",  "subsidy_elec_direct_pc",
    #                         "subsidy_elec_indirect_pc",  "subsidy_fuel_pc", "subsidy_fuel_direct_pc",
    #                         "subsidy_fuel_indirect_pc", "subsidy_eau_pc", "subsidy_eau_direct_pc",
    #                         "subsidy_eau_indirect_pc"))

    # local_mod_inc_ser(id = "inkind", sim_res = sim_res_local,
    #                       dec_vars = c("inktransf_total_pc", "Sante_inKind_pc", "education_inKind_pc"))

    # local_mod_inc_ser(id = "education_inKind", sim_res = sim_res_local,
    #                       dec_vars = c("education_inKind_pc", "am_pre_school_pub_pc", "am_primary_pub_pc", "am_secondary_pub_pc", "am_tertiary_pub_pc"))

    local_mod_inc_ser(id = "income_tax", sim_res = sim_res_local,
                          dec_vars = c("dtx_total", "dtx_inc", "dtx_con"))

    local_mod_inc_ser(id = "Tax_TVA", sim_res = sim_res_local,
                          dec_vars = c("itx_total"))

    # local_mod_inc_ser(id = "cs_total", sim_res = sim_res_local,
    #                       dec_vars = c("sscontribs_total_pc", "csh_css_pc", "csh_ipm_pc"))

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
           id = "testid") {
    ui <- fluidPage(ui_side(id = id))
    srv <-
      function(input, output, session) {
        server_side(id = id, sim_res = reactive(sim_res))
      }
    shinyApp(ui, srv)

  }
