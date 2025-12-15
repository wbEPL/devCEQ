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
    
    var_inc = get_inc_nm()$var,
    var_wt = get_wt_nm(),
    var_group = get_group_nm()$var,

    page_title = f_get_app_text("m_incidence"),

    ndec_type = "selectInput", 
    ndec_title = f_get_app_text("ndec_title"),
    ndec_choices = c(10, 5, 15, 25, 50),

    decby_type = "selectizeInput",
    decby_title = f_get_app_text("decby_title"),
    decby_choices = f_var_names_vector(get_inc_nm()),

    incid_type = "selectizeInput",
    incid_title = f_get_app_text("incid_title"),
    incid_choices = c("relative", "absolute", "level"),

    grpby_type = "selectizeInput",
    grpby_title = f_get_app_text("title_compare"),
    grpby_choices = f_var_names_vector(get_var_nm(var_group)),

    pltby_type = "radioGroupButtons",
    pltby_title = NULL,

    ...
  ) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # Step 1. Page structure -------------------------------------------------------
      output$incidences_ui <- renderUI(page_ui(ns(NULL)))

      # Step 2.a Title
      ptitle <- m_input_srv("title", "title", reactive(page_title), reactive(page_title))

      # Step 2.b Decile selection
      ndec_inp <- m_input_srv("ndec", ndec_type, ndec_title, ndec_choices)

      # Step 2.c Decile by selection
      decby_inp <- m_input_srv("decby", decby_type, decby_title, decby_choices)

      # Step 2.d Statistics selection
      incid_inp <- m_input_srv("incid", incid_type, f_get_app_text("incid_type"), incid_choices)
      
      # Step 2.e Group by selection
      grpby_inp <- m_input_srv("grpby", grpby_type, grpby_title, grpby_choices)

      # Step 2.z Plot selection
      pltby_inp <- m_input_srv("pltby", pltby_type, pltby_title, reactive(fig$id))

      # Step 3.A Data/Plots preparation -------------------------------------------
      sim_ready <- reactive(req(sim_res()))

      # Step 4. Calculations ------------------------------------------------

      # 4.1 Deciles 
      dta_1_deciles <- reactive({
        req(sim_ready())
        req(ndec_inp())
        req(decby_choices)
        f_calc_deciles_by_sim(
          sim_ready(),
          dec_var = decby_choices,
          wt_var = var_wt,
          n_dec = ndec_inp()
        )
      })

      # 4.2 Aggregate incidences
      dta_2_incid <- reactive({})

      # 4.3 Prepare formatted data for tables and plots
      dta_calc_formatted <- reactive({
        req(dta_calc())
        dta_calc() |> f_format_tbl()
      })

      # Filtering "by group" variable and statistics to plot
      dta_calc_fig <- reactive({
        # req(dta_calc())
        # req(grpby_inp())
        # dta_calc() |>
        #   filter(
        #     if_any(f_get_colname("measure")) %in%
        #       get_measure_nm(plt_options)$measure_title
        #   )        
      })

      dta_fig <- reactive({
        # req(dta_calc())
        # if (length((var_group)) == 0) {
        #   return(NULL)
        # }
        # out <- 
        #   var_group |>
        #   map(
        #     ~ {
        #       dta_local <- dta_calc_fig() |> f_filter_grouped_stats(.x)
        #       if (length(.x) == 1 && .x == "all") {
        #         fig_out <- f_plot_pov_by(
        #           dta_local,
        #           fig_by = "measure",
        #           fig_filter = get_measure_nm(plt_options)$measure_title,
        #           x_lab = f_get_app_text("title_plot_inccon"),
        #           facet_var = NULL,
        #           color_var = "sim"
        #         )
        #       } else {
        #         fig_out <- f_plot_pov_by(
        #           dta_local,
        #           fig_by = "measure",
        #           fig_filter = get_measure_nm(plt_options)$measure_title,
        #           x_lab = f_get_app_text("title_plot_inccon")
        #         )
        #       }
        #     }
        #   ) |>
        #   set_names(var_group)
        # out
      })
      
      # Generating plots based on filtered data
      observeEvent(
        {
          dta_fig()
          grpby_inp()
        },
        {
          req(dta_fig())
          req(grpby_inp())
          # browser()
          if (grpby_inp() %in% names(dta_fig())) {
            fig$ggs <- dta_fig()[[grpby_inp()]]
          } else {
            fig$ggs <- dta_fig()[[1]]
          }
          fig$id <- names(fig$ggs)
          fig$title <- names(fig$ggs)
        }, 
        ignoreInit = TRUE
      )

      # observeEvent(fig$dta, {
      #   req(fig$dta)
      #   fig$dta_out <- fig$dta |> f_format_tbl()
      # })

      # observeEvent(fig$dta_out, {
      #   req(dta_calc_formatted())
      #   fig$rt <- dta_calc_formatted() |> f_format_rt(col_min_groups = 1)
      # })

      # Step 3 Generating plots ------------------------------------------------
      fig <- reactiveValues(
        id = NULL,
        title = NULL,
        ggs = NULL,
        dta = NULL
      )

      # Step 10. Card with plot and data table --------------------------------
      m_figure_server(
        "fig1",
        figures = reactive(fig$ggs),
        selected = pltby,
        force_ly = T
      )
      
      m_figure_server(
        "tbl1",
        figures = reactive({
          dta_calc_formatted() |> f_format_rt(col_min_groups = 1)
        }),
        selected = pltby
      )

      # Step 20. Results export modal ----------------------------------------
      m_download_srv(
        "save_mod_local",
        all_figs = reactive({
          list(list(
            sheet_name = ptitle(),
            meta_tbl = tibble(),
            tbl = dta_calc_formatted(),
            ggs = purrr::list_flatten(dta_fig())
          ))
        }),
        file_name = reactive({
          paste0("poverty_stats_", Sys.Date(), ".xlsx")
        })
      )

      # Step 50. Results of the module ----------------------------------------
      mod_out <-
        reactive({
          list(
            inputs = list(
              ptitle = ptitle(),
              pl_choice = pl_choice(),
              grpby = grpby_inp(),
              pltby = pltby()
            ),
            id = fig$id,
            title = fig$title,
            sheet_name = ptitle(),
            tbl = dta_calc_formatted(),
            ggs = purrr::list_flatten(dta_fig())
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
f_incid_ui_linear2 <- function(id) {
  ns <- NS(id)
  tagList(
    m_input_ui(ns("title")),
    layout_columns(
      m_input_ui(ns("ndec")),
      m_input_ui(ns("decby")),
      m_input_ui(ns("incid")),
      m_input_ui(ns("grpby"))#,
      # m_input_ui(ns("pltby"))
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


#' @describeIn m_incid Incidence page template for output in a results layout
#' @importFrom shiny NS tagList
#' @import bslib
#' @export
#'
f_incid_ui_linear <- function(id, add_pl = TRUE) {
  ns <- NS(id)

  # plt-specific controls 
  input_elements <- list(
    m_input_ui(ns("ndec")),
    m_input_ui(ns("decby")),
    m_input_ui(ns("incid")),
    m_input_ui(ns("grpby")),
    m_input_ui(ns("pltby"))
  )
  col_widths <- c(3,3,3,3)

  # Drop NULL from list
  tagList(
    bslib::layout_columns(
      !!!input_elements,
      col_widths = col_widths
    ),
    navset_card_underline(
      full_screen = FALSE,
      title = m_input_ui(ns("title")),
      
      nav_panel(
        "Plot",
        card_body(
          m_figure_ui(ns("fig1")),
          fillable = TRUE, 
          min_height = "450px",
          max_height = "525px"
        ) #,
        # card_footer("Footer placeholder")
      ),

      nav_panel(
        "Data",
        card_body(
          m_figure_ui(ns("tbl1")),
          fillable = TRUE,
          min_height = "450px",
          max_height = "525px"
        )
      ),

      if (in_devmode()) {
        nav_panel(
          "Dignostics",
          m_diagnostics_ui(ns("dev_info"))
        )
      },

      nav_spacer(),

      nav_item(
        m_download_ui(
          ns("save_mod_local"),
          f_get_app_text("save_btn"),
          ui_fn = downloadButton,
          icon = icon("file-excel"),
          class = "btn btn-light btn-sm"
        )
      )
    )
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
      m_input_ui(ns("incid")),
      m_input_ui(ns("grpby")),
      m_input_ui(ns("pltby"))
    ),

    nav_panel(
      "Plot",
      card_body(
        m_figure_ui(ns("fig1")),
        min_height = "350px",
        max_height = "500px"
      ),
      card_footer("Footer placeholder")
    ),

    nav_panel(
      "Data",
      card_body(
        m_figure_ui(ns("tbl1")),
        min_height = "350px",
        max_height = "450px"
      )
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
  page_ui = f_incid_ui_linear,
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