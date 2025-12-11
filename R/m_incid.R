#' Insidences and other vis template module
#' 
#' @name m_incid
#' 
NULL

#' @describeIn m_incid New incidences UI placeholder
#' 
#' @importFrom shiny NS uiOutput
#' @importFrom shinycssloaders withSpinner
#' 
m_incid_ui <- 
  function(id) {
    ns <- NS(id)
    uiOutput(ns("incidences_ui")) |> shinycssloaders::withSpinner() 
    # tags$div(style = "overflow-y: auto; height: calc(100vh - 8rem); overflow-x: hidden;") %>%
    # tagList()
  }


#' @describeIn m_incid New incidence server logic
#'
#' @param ndec_type Type of input for number of deciles: "numericInput" or "selectInput"
#' 
m_incid_srv <-
  function(
    id,
    sim_res,
    page_ui = f_incid_ui_linear,
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
      ndec <- m_input_srv("ndec", ndec_type, ndec_title, ndec_choices)
      decby <- m_input_srv("decby", "selectInput", decby_title, decby_choices)
      grpby <- m_input_srv("grpby", grpby_type, grpby_title, grpby_choices)

      # Step 2.c Plot selection
      pltby <- m_input_srv("pltby", pltby_type, pltby_title, reactive(fig$id))

      # Step 3.A Data/Plots preparation -------------------------------------------      
      observeEvent(sim_res(), {
        fig$dta <-
          f_calc_pov_stats(
            dta = sim_res(),
            var_inc = get_inc_nm()$var,
            var_wt = get_wt_nm(),
            group_vars = get_group_nm()$var,
            pl_var = NULL,
            pl_val = NULL
          ) 
      })

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
              ndec = ndec(),
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

#' @describeIn m_incid Incidence page template for output in a results layout
#' @export
#'
f_incid_ui_linear <- function(id) {
  ns <- NS(id)
  tagList(
    m_input_ui(ns("title")),
    layout_columns(
      m_input_ui(ns("ndec")),
      m_input_ui(ns("decby")),
      m_input_ui(ns("grpby"))
    ),
    layout_columns(
      m_input_ui(ns("pltby"))
    ),
    # Figure
    m_figure_ui(ns("fig1")),

    # Table
    m_figure_ui(ns("tbl1")),

    # Diagnostics
    m_diagnostics_ui(ns("dev_info"))
  )
}


#' @describeIn m_incid Incidence card template with multiple tabs (Figures and Data) and a sidebar with key input components
#' @export
#'
f_incid_ui_card <- function(id, ...) {
  ns <- NS(id)
  navset_card_tab(
    full_screen = TRUE,
    title = m_input_ui(ns("title")),

    sidebar = sidebar(
      m_input_ui(ns("ndec")),
      m_input_ui(ns("decby")),
      m_input_ui(ns("grpby")),
      m_input_ui(ns("pltby"))
    ),

    nav_panel(
      "Plot",
      card_body(m_figure_ui(ns("fig1")), min_height = "350px", max_height = "500px"),
      card_footer("Footer placeholder")
    ),

    nav_panel(
      "Data",
      card_body(m_figure_ui(ns("tbl1")), min_height = "350px", max_height = "450px")
    ),

    if (in_devmode()) {
      nav_panel(
        "Dignostics",
        m_diagnostics_ui(ns("dev_info"))
      )
    },

    nav_spacer(),

    # nav_item(
    #   actionButton(ns("download_excel"), "Download Excel")
    #   # m_download_ui(
    #   #   id,
    #   #   "Save Excel",
    #   #   ui_fn = downloadButton,
    #   #   icon = icon("file-excel"),
    #   #   class = "btn btn-light btn-sm"
    #   # )
    # )
  )
}



#' @describeIn m_incid Test app for m_incid module
#' @export
#' 
test_m_incid <- function(
  page_ui = f_incid_ui_card,
  sim_res = reactive(dta_sim),
  ...
) {
  library(shiny)
  library(shinyWidgets)
  library(bslib)

  ui <- page_fluid(
    m_incid_ui("incid1")
  )

  server <- function(input, output, session) {
    m_incid_srv("incid1", sim_res = sim_res, page_ui = page_ui, ...)
  }

  shinyApp(ui, server)
}


#' @describeIn m_incid Test app for m_incid module within a results switching layout
#' @export
#' 
test_m_incid_switches <- function() {
  library(shiny)
  library(shinyWidgets)
  library(bslib)

  figures <- c(
    "pov" = "Poverty",
    "gini" = "Inquality",
    "ncp" = "Net Cash Position",
    "other" = "Other"
  )

  pages <-
    bslib::nav_panel_hidden(
      value = "tab_incid",
      map(names(figures), m_incid_ui)
      # m_incid_ui("pov"),
      # m_incid_ui("gini"),
      # m_incid_ui("ncp")
    ) |>
    list() |>
    append(
      list(bslib::nav_panel_hidden(
        value = "tab_incid2",
        m_incid_ui("tab_incid2")
      ))
    ) |>
    append(
      list(bslib::nav_panel_hidden(
        value = "Diagnostics",
        m_diagnostics_ui("diag1")
      ))
    )

  page_ui <-
    page_fluid(
      theme = bs_theme(),
      layout_columns(
        col_widths = c(3, 9),
        m_res_switches_ui(
          "res-srv",
          panels_choices = c(
            "Poverty" = "tab_incid",
            "Incidences2" = "tab_incid2",
            "Diagnostics" = "Diagnostics"
          )
        ),
        m_res_content_ui("res-srv", panle_ui = pages)
      )
    )

  page_srv <-
    function(input, output, session) {
      m_res_switches_srv("res-srv")


      all_figs <-
        figures |> 
        imap(~{
          m_incid_srv(
            .y,
            page_ui = f_incid_ui_card,
            page_title = .x
          )
        })
      # m_incid_srv("pov", page_ui = f_incid_ui_card, page_title = "Poverty")
      # m_incid_srv("gini", page_ui = f_incid_ui_card, page_title = "Inquality")
      # m_incid_srv("ncp", page_ui = f_incid_ui_card, page_title = "Net Cash Position")
      all_figs$tab_incid2 <- m_incid_srv("tab_incid2", title = "Incidence 2")

      all_figs_r <- reactive({
        all_figs |> map(~ .x())
      })

      m_diagnostics_srv("diag1", out = all_figs_r)
    }

  # devmode(F, F)
  # devmode()

  shiny::runApp(shinyApp(page_ui, page_srv))
}