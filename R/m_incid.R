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
      pltby <- m_input_srv("pltby", pltby_type, pltby_title, reactive(names(fig()$ggs)))

      # Step 3 Generating plots ------------------------------------------------
      #TODO

      fig <- reactiveVal(
        list(
          id = c("Fig 1", "Fig 2", "Fig 3"),
          title = c("Figure 1", "Figure 2", "Figure 3"),
          lys = list(),
          ggs = list(fig_gg_random(), fig_gg_random(), fig_gg_random()) |> 
            set_names(c("Fig 1", "Fig 2", "Fig 3")),
          fts = map(1:3, ~ sample_n(mtcars, 5) |> flextable()) |> 
            set_names(c("Fig 1", "Fig 2", "Fig 3")),
          dta = tibble(1:10)
        )
      )

      # Step 10. Card with plot and data table --------------------------------
      m_figure_server("fig1", figures = reactive({fig()$ggs}), selected = pltby)
      m_figure_server("tbl1", figures = reactive({fig()$fts}), selected = pltby)

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
            ggs = fig()$ggs,
            fts = fig()$fts,
            dta = fig()$dta
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
      m_figure_ui(ns("fig1")),
      card_footer("Footer placeholder")
    ),

    nav_panel(
      "Data",
      m_figure_ui(ns("tbl1"))
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
#'
test_m_incid <- function(
  page_ui = f_incid_ui_linear,
  ...
) {
  library(shiny)
  library(shinyWidgets)
  library(bslib)

  ui <- page_fluid(
    m_incid_ui("incid1")
  )

  server <- function(input, output, session) {
    sim_res <- reactive({
      NULL
    })
    m_incid_srv("incid1", sim_res = sim_res, page_ui = page_ui, ...)
  }

  shinyApp(ui, server)
}
