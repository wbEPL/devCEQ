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
    pl_choices = c(
      "National PL" = "pl_nat",
      "PPP 1.90" = "pl_190",
      "PPP 5.00" = "pl_500"
    ),

    grpby_type = "selectizeInput", #"numericInput"
    grpby_title = "Compare by:",
    grpby_choices = c(
      "All observations" = "all",
      f_var_names_vector(get_var_nm(c("group_1", "group_2", "group_3"))),
      "Everything" = "total"
    ),

    pltby_type = "radioGroupButtons",
    pltby_title = NULL,

    plt_options = c("fgt0", "fgt1", "fgt2", "hc"),
    ...
  ) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # Step 1. Page structure -------------------------------------------------------
      output$incidences_ui <- renderUI(page_ui(id))

      # Step 2.a Title
      ptitle <- m_input_srv(
        "title",
        "title",
        title = reactive(page_title),
        choices = reactive(page_title)
      )

      # Step 2.b Poverty line selection
      pl_choice <- m_input_srv("pl_choice", pl_type, pl_title, pl_choices)
      grpby <- m_input_srv("grpby", grpby_type, grpby_title, grpby_choices)

      # Step 2.c Plot selection
      plt_options_name <- get_measure_nm(plt_options)$measure_title
      pltby <- m_input_srv("pltby", pltby_type, pltby_title, reactive(fig$id))

      # Step 3.A Data/Plots preparation -------------------------------------------
      dta_calc <- eventReactive(
        {
          sim_res()
          pl_choice()
        },
        {
          req(sim_res())
          req(pl_choice())
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

      # Filtering by group variable
      observeEvent(
        {
          dta_calc()
          grpby()
        },
        {
          req(dta_calc())
          req(grpby())
          dta_out <- dta_calc()
          group_var_all <- dta_out |>
            slice(1) |>
            pull(any_of(f_get_colname("group_var"))) |>
            as.character()
          if (all(grpby() == "all")) {
            dta_out <- dta_out |>
              filter(if_any(
                any_of(f_get_colname("group_var")),
                ~ . %in% group_var_all
              ))
          } else if (all(grpby() == "total")) {
            dta_out <- dta_out
          } else {
            dta_out <-
              dta_out |>
              filter(if_any(
                any_of(f_get_colname("group_var")),
                ~ . %in%
                  c(group_var_all, as.character(get_var_nm(grpby())$var_title))
              ))
          }
          dta_out <- dta_out |> filter(Statistics %in% plt_options_name)
          fig$dta <- dta_out
        }
      )

      observeEvent(
        {
          fig$dta
          grpby()
        },
        {
          req(fig$dta)
          if (length(grpby()) == 1 && grpby() == "all") {
            fig_out <- f_plot_pov_by(
              fig$dta,
              fig_by = "measure",
              fig_filter = get_measure_nm(plt_options)$measure_title,
              x_lab = "Income concepts",
              facet_var = NULL,
              color_var = "sim"
            )
          } else {
            fig_out <- f_plot_pov_by(
              fig$dta,
              fig_by = "measure",
              x_lab = "Income concepts"
            )
          }

          # Reorder plots based on plt_options
          fig$ggs <- fig_out
          req(fig$ggs)
          fig$id <- names(fig$ggs)
          fig$title <- names(fig$ggs)
        }
      )

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
      m_figure_server(
        "fig1",
        figures = reactive({
          fig$ggs
        }),
        selected = pltby,
        force_ly = T
      )
      m_figure_server(
        "tbl1",
        figures = reactive({
          fig$rt
        }),
        selected = pltby
      )

      # Step 20. Results export modal ----------------------------------------
      #TODO

      # Step 50. Results of the module ----------------------------------------
      mod_out <-
        reactive({
          list(
            inputs = list(
              ptitle = ptitle(),
              pl_choice = pl_choice(),
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
f_pov_ui_linear <- function(id, add_pl = TRUE) {
  ns <- NS(id)
  tagList(
    # m_input_ui(ns("title")),
    layout_columns(
      if (add_pl) m_input_ui(ns("pl_choice")) else NULL,
      m_input_ui(ns("grpby")),
      m_input_ui(ns("pltby"))
    ),
    navset_card_tab(
      full_screen = TRUE,
      title = m_input_ui(ns("title")),
      # navbar_options = navbar_options(position="fixed-bottom"),

      nav_panel(
        "Plot",
        card_body(
          m_figure_ui(ns("fig1")),
          min_height = "350px",
          max_height = "500px"
        )#,
        # card_footer("Footer placeholder")
      ),

      nav_panel(
        "Data",
        card_body(
          m_figure_ui(ns("tbl1")),
          min_height = "350px",
          max_height = "500px"
        )
      ),

      if (in_devmode()) {
        nav_panel(
          "Dignostics",
          m_diagnostics_ui(ns("dev_info"))
        )
      },

      nav_spacer()

      # nav_item(
      #   actionButton(ns("download_excel"), "Download Excel")
      #   m_download_ui(
      #   #   id,
      #   #   "Save Excel",
      #   #   ui_fn = downloadButton,
      #   #   icon = icon("file-excel"),
      #   #   class = "btn btn-light btn-sm"
      #   # )
      # )
    )
  )
}

#' @describeIn m_pov Incidence page template for Gini output in a results layout
#' @export
#' 
f_gini_ui_linear <- function(id) {
  f_pov_ui_linear(id, add_pl = FALSE)
}

#' @describeIn m_pov Module combining poverty and inequality analysis
#' @export
m_povgini_ui <- function(id = NULL) {
  ns <- NS(id)
  page_fluid(
    m_incid_ui(ns("pov1")),
    m_incid_ui(ns("pov2"))
  )
}


#' @describeIn m_pov Module combining poverty and inequality analysis
#' @export
m_povgini_srv <- function(id = NULL, sim_res, ...) {
  moduleServer(id, function(input, output, session) {
    m_pov_srv(
      "pov1",
      sim_res = sim_res,
      page_ui = f_pov_ui_linear,
      plt_options = c("fgt0", "fgt1", "fgt2", "hc"),
      ...
    )
    m_pov_srv(
      "pov2",
      sim_res = sim_res,
      page_ui = f_gini_ui_linear,
      plt_options = c("gini", "theil"),
      page_title = "Gini",
      ...
    )
  })
}

#' @describeIn m_pov Test app for m_incid module
#' @export
#'
test_m_pov <- function(
  page_ui = f_pov_ui_linear,
  sim_res = reactive(dta_sim),
  ...
) {
  library(shiny)
  library(shinyWidgets)
  library(bslib)

  ui <- m_povgini_ui(NULL)

  server <- function(input, output, session) {
    m_povgini_srv(NULL, sim_res = sim_res, ...)
  }

  shinyApp(ui, server)
}