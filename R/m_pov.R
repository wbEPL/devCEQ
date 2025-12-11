#' Module: Poverty Incidence Analysis
#' @name m_pov
NULL

#' @describeIn m_pov New incidence server logic
#'
#' @param ndec_type Type of input for number of deciles: "numericInput" or "selectInput"
#' 
m_pov_srv <-
  function(
    id,
    sim_res,
    page_ui = f_incid_ui_linear,
    # dec_vars = get_var_nm()$var,
    # make_bar_fn = make_bar_dta,

    page_title = "Poverty",

    pl_type = "selectInput", #"numericInput"
    pl_title = "Poverty line:",
    pl_choices = c("National PL" = "pl_nat", "PPP 1.90" = "pl_190", "PPP 5.00" = "pl_500"),
    
    decby_type = "selectInput",
    decby_title = "Deciles by:",
    decby_choices = f_var_names_vector(get_inc_nm()),

    grpby_type = "selectizeInput", #"numericInput"
    grpby_title = "Compare by:",
    grpby_choices = c("Simulation" = "sim_id", f_var_names_vector(get_var_nm(c("group_1", "group_2", "group_3")))) , # c("Simulation" = "sim_id"),

    pltby_type = "radioGroupButtons",
    pltby_title = NULL,

    fig_by = "measure",
    ...
  ) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # Step 1. Page structure -------------------------------------------------------
      output$incidences_ui <- renderUI(page_ui(id))

      # Step 2.a Title
      ptitle <- m_input_srv("title", "title", title = reactive(page_title), choices = reactive(page_title))

      # Step 2.b Number of deciles
      pl_choice <- m_input_srv("ndec", pl_type, pl_title, pl_choices)
      decby <- m_input_srv("decby", "selectInput", decby_title, decby_choices)
      grpby <- m_input_srv("grpby", grpby_type, grpby_title, grpby_choices)

      # Step 2.c Plot selection
      pltby <- m_input_srv("pltby", pltby_type, pltby_title, reactive(fig$id))

      # Step 3.A Data/Plots preparation -------------------------------------------
      observeEvent(
        {
          sim_res()
          pl_choice()
        },
        {
          req(sim_res())
          req(pl_choice())
          fig$dta <-
            f_calc_pov_stats(
              dta = sim_res(),
              var_inc = get_inc_nm()$var,
              var_wt = get_wt_nm(),
              group_vars = get_group_nm()$var,
              pl_var = pl_choice(),
              pl_val = NULL
            )
        }
      )

      observeEvent(fig$dta, {
        req(fig$dta)
        fig$ggs <- f_plot_pov_by(fig$dta , fig_by = fig_by, x_lab = "Income concepts")
        req(fig$ggs)
        fig$id <- names(fig$ggs)
        fig$title <- names(fig$ggs)
      })
      
      observeEvent(fig$dta, {
        req(fig$dta)
        fig$dta_out <- fig$dta |> f_format_tbl() 
      })
      
      observeEvent(fig$dta_out, {
        req(fig$dta_out)
        fig$rt <- fig$dta_out |> f_format_rt(col_min_groups = 1)
      })
   
      # Step 3 Generating plots ------------------------------------------------
      fig <- reactiveValues(
          id = c("Fig 1", "Fig 2", "Fig 3"),
          title = c("Figure 1", "Figure 2", "Figure 3"),
          lys = list(),
          ggs = NULL,
          fts = NULL,
          rt = NULL,
          dta_out = NULL,
          dta = NULL
      )
      
      # Step 10. Card with plot and data table --------------------------------
      m_figure_server("fig1", figures = reactive({fig$ggs}), selected = pltby, force_ly = T)
      m_figure_server("tbl1", figures = reactive({fig$rt}), selected = pltby)

      # Step 20. Results export modal ----------------------------------------
      #TODO

      # Step 50. Results of the module ----------------------------------------
      mod_out <- 
        reactive({
          list(
            inputs = list(
              ptitle = ptitle(),
              pl_choice = pl_choice(),
              decby = decby(),
              grpby = grpby(),
              pltby = pltby()
            ),
            id = fig$id,
            title = fig$title,
            ggs = fig$ggs,
            fts = fig$fts,
            dta = fig$dta,
            dta_out = fig$dta_out
          )
        })

      # Step 80. Development context info ----------------------------------------
      m_diagnostics_srv("dev_info", out = mod_out)

      # Step 90. Return reactive values ----------------------------------------
      mod_out

    })
  }


#' @describeIn m_pov Test app for m_incid module
#' @export
#'
test_m_pov <- function(
  page_ui = f_incid_ui_card,
  sim_res = reactive(dta_sim),
  ...
) {
  library(shiny)
  library(shinyWidgets)
  library(bslib)

  ui <- page_fluid(
    m_incid_ui("pov1"),
    m_incid_ui("pov2")
  )

  server <- function(input, output, session) {
    m_pov_srv("pov1", sim_res = sim_res, page_ui = page_ui, ...)
    m_pov_srv("pov2", sim_res = sim_res, page_ui = page_ui, ...)
  }

  shinyApp(ui, server)
}