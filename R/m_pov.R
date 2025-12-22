#' Module: Poverty Incidence Analysis
#' @name m_pov
NULL

#' @describeIn m_pov New incidence server logic
#'
#' @param ndec_type Type of input for number of deciles: "numericInput" or "selectInput"
#'
#' @export
#'
m_pov_srv <-
  function(
    id,
    sim_res,
    page_ui = f_incid_ui_linear,

    var_inc = get_inc_nm()$var,
    var_wt = get_wt_nm(),
    var_group = get_group_nm()$var,

    page_title = f_get_app_text("m_pov"),

    pl_type = "selectInput", #"numericInput"
    pl_title = f_get_app_text("title_pl"),
    pl_choices = get_pl_nm() |> f_var_names_vector(),

    grpby_type = "selectizeInput", #"numericInput"
    grpby_title = f_get_app_text("title_compare"),
    grpby_choices = f_var_names_vector(get_var_nm(var_group)),

    pltby_skip = TRUE,
    pltby_type = "radioGroupButtons",
    pltby_title = NULL,

    plt_options = c("fgt0", "fgt1", "fgt2", "hc"),

    stats = c("fgt0", "fgt1", "fgt2", "hc", "gini", "thiel", "n", "pop"),
    ...
  ) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # Step 1. Page structure -------------------------------------------------------
      ever_rendered <- reactiveVal(FALSE)
      output$incidences_ui <- renderUI({
        req(isolate(!ever_rendered()))
        validate(need(isTruthy(sim_res()), "Press 'Run' to execute simulaitons."))
        page_ui(ns(NULL), pltby_skip = pltby_skip)
      })

      # Step 2.a Title
      ptitle <- m_input_srv("title", "title", title = reactive(page_title), choices = reactive(page_title))

      # Step 2.b Poverty line selection
      pl_choice <- m_input_srv("pl_choice", pl_type, pl_title, pl_choices)
      grpby <- m_input_srv("grpby", grpby_type, grpby_title, grpby_choices)

      # Step 2.c Plot selection
      # plt_options_name <- get_measure_nm(plt_options)$measure_title
      pltby <- if (!pltby_skip) {
        m_input_srv("pltby", pltby_type, pltby_title, reactive(fig$id))
      } else {
        reactive(NULL)
      }

      # Step 3.A Data/Plots preparation -------------------------------------------
      sim_ready <- reactive({
        # req(sim_res())
        # browser()
        # All simulations have names and data
        validate(
          need(isTruthy(sim_res()), "Press 'Run' to execute simulaitons."),
          need(length(sim_res()) > 0, "No simulation data found."),
          need(
            all(map_chr(sim_res(), ~ unique(.x$policy_name)) != ""),
            "Not all simulations have names."
          ),
          need(
            any(map_lgl(sim_res(), ~ !is.null(.x$policy_sim_raw))),
            "Some simulations do not have simulation data."
          ),
        )
        req(sim_res())
        sim_res() |> keep(~ !is.null(.x$policy_sim_raw) && nrow(.x$policy_sim_raw) > 0)
      })

      # Estimates all poverty measures
      dta_calc <- eventReactive(
        {
          sim_ready()
          pl_choice()
        },
        {
          req(sim_ready())
          req(pl_choice())
          f_calc_pov_stats(
            dta = sim_ready(),
            var_inc = var_inc,
            var_wt = var_wt,
            group_vars = var_group,
            pl_var = pl_choice(),
            pl_val = NULL,
            stats = stats
          )
        }
      )

      dta_calc_formatted <- reactive({
        req(dta_calc())
        dta_calc() |> f_format_tbl()
      })

      # Filtering "by group" variable and statistics to plot
      dta_calc_fig <- reactive({
        req(dta_calc())
        req(grpby())
        dta_calc() |>
          filter(
            if_any(f_get_colname("measure")) %in%
              get_measure_nm(plt_options)$measure_title
          )
      })

      dta_fig <- reactive({
        req(dta_calc())
        if (length((var_group)) == 0) {
          return(NULL)
        }
        out <-
          var_group |>
          map(
            ~ {
              dta_local <- dta_calc_fig() |> f_filter_grouped_stats(.x)
              if (length(.x) == 1 && .x == "all") {
                fig_out <- f_plot_pov_by(
                  dta_local,
                  fig_by = "measure",
                  fig_filter = get_measure_nm(plt_options)$measure_title,
                  x_lab = f_get_app_text("title_plot_inccon"),
                  facet_var = NULL,
                  color_var = "sim"
                )
              } else {
                fig_out <- f_plot_pov_by(
                  dta_local,
                  fig_by = "measure",
                  fig_filter = get_measure_nm(plt_options)$measure_title,
                  x_lab = f_get_app_text("title_plot_inccon")
                )
              }
            }
          ) |>
          set_names(var_group)
        out
      })

      # Generating plots based on filtered data
      # fig_out <- reactive()
      observeEvent(
        {
          dta_fig()
          grpby()
        },
        {
          req(dta_fig())
          req(grpby())
          # browser()
          if (grpby() %in% names(dta_fig())) {
            fig$ggs <- dta_fig()[[grpby()]]
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
      m_output_srv(
        "fig1",
        figures = reactive(fig$ggs),
        selected = if (pltby_skip) reactive(NULL) else pltby,
        force_ly = T
      )

      m_output_srv(
        "tbl1",
        figures = reactive({
          dta_calc_formatted() |> f_format_rt(col_min_groups = 1)
        }),
        selected = if (pltby_skip) reactive(NULL) else pltby
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
              grpby = grpby(),
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
#' @importFrom shiny NS tagList
#' @import bslib
#' @export
#'
f_pov_ui_linear <- function(id, add_pl = TRUE, pltby_skip = TRUE) {
  ns <- NS(id)

  # plt-specific controls
  input_elements <- list(
    if (add_pl) m_input_ui(ns("pl_choice")) else NULL,
    m_input_ui(ns("grpby")),
    if (!pltby_skip) m_input_ui(ns("pltby")) else NULL
  )
  input_elements <- input_elements[!sapply(input_elements, is.null)]
  if (length(input_elements) == 2) {
    col_widths  <- c(5, 7)
  } else if (length(input_elements) == 3) {
    col_widths  <- c(3, 4, 5)
  } else if (length(input_elements) == 1) {
    col_widths  <- c(12)
  } else {
    col_widths  <- NULL
  }

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
          m_output_ui(ns("fig1")),
          fillable = TRUE,
          min_height = "450px",
          max_height = "600px"
        ) #,
        # card_footer("Footer placeholder")
      ),

      nav_panel(
        "Data",
        card_body(
          m_output_ui(ns("tbl1")),
          fillable = TRUE,
          min_height = "450px",
          max_height = "800px"
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

#' @describeIn m_pov Incidence page template for Gini output in a results layout
#' @export
#'
f_gini_ui_linear <- function(id, pltby_skip = TRUE) {
  f_pov_ui_linear(id, add_pl = FALSE, pltby_skip = pltby_skip)
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
      stats = c("fgt0", "fgt1", "fgt2", "hc"),
      ...
    )
    m_pov_srv(
      "pov2",
      sim_res = sim_res,
      page_ui = f_gini_ui_linear,
      plt_options = c("gini", "theil"),
      page_title = f_get_app_text("m_ineq"),
      stats = c("gini", "theil"),
      ...
    )
  })
}

#' @describeIn m_pov Test app for m_incid module
#' @export
#'
test_m_pov <- function(
  idd = NULL,
  page_ui = f_pov_ui_linear,
  sim_res = reactive(dta_sim),
  ...
) {
  library(shiny)
  library(shinyWidgets)
  library(bslib)

  ui <- m_povgini_ui(idd)

  server <- function(input, output, session) {
    m_povgini_srv(idd, sim_res = sim_res, ...)
  }

  shinyApp(ui, server)
}
