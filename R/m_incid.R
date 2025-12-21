#' Insidences and other vis template module
#'
#' @name m_incid
#'
NULL

#' @describeIn m_incid New incidences UI placeholder
#'
#' @importFrom shiny NS uiOutput
#' @importFrom shinycssloaders withSpinner
#' @export
m_incid_ui <-
  function(id) {
    ns <- NS(id)
    uiOutput(ns("incidences_ui")) |> shinycssloaders::withSpinner() |> card()
    # tags$div(style = "overflow-y: auto; height: calc(100vh - 8rem); overflow-x: hidden;") %>%
    # tagList()
  }


#' @describeIn m_incid New incidence server logic
#'
#' @param ndec_type Type of input for number of deciles: "numericInput" or "selectInput"
#'
#' @export
m_incid_srv <-
  function(
    id,
    sim_res,
    page_ui = f_incid_ui_linear,

    var_inc = get_inc_nm()$var,
    var_wt = get_wt_nm(),
    var_group = get_group_nm()$var,

    var_agg = get_var_nm()$var[1:10],

    page_title = f_get_app_text("m_incidence"),

    ndec_type = "selectInput",
    ndec_title = f_get_app_text("ndec_title"),
    ndec_choices = c(5, 10, 15, 25, 50),

    decby_type = "selectizeInput",
    decby_title = f_get_app_text("decby_title"),
    decby_choices = f_var_names_vector(get_inc_nm()),

    incid_type = "selectizeInput",
    incid_title = f_get_app_text("incid_title"),
    incid_choices = c("relative", "absolute", "level"),

    grpby_type = "selectizeInput",
    grpby_title = f_get_app_text("title_compare"),
    grpby_choices = f_var_names_vector(get_var_nm(var_group)),

    grpfltr_type = "checkboxGroupButtons",
    grpfltr_title = f_get_app_text("title_filter"),
    grpfltr_choices = NULL,

    pltby_type = "radioGroupButtons",
    pltby_title = NULL,

    ...
  ) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # Step 1. Page structure -------------------------------------------------------
      ever_rendered <- reactiveVal(FALSE)
      output$incidences_ui <- renderUI({

        req(isolate(!ever_rendered()))
        validate(need(
          isTruthy(sim_res()),
          "Press 'Run' to execute simulaitons."
        ))
        page_ui(ns(NULL))
      })

      # Step 2.a Title
      ptitle <- m_input_srv("title", "title", reactive(page_title), reactive(page_title))

      # fltr 1: N deciles ----------------------------------------------------
      ndec_inp <- m_input_srv("ndec", ndec_type, ndec_title, ndec_choices)

      # fltr 2: Deciles by ---------------------------------------------------
      decby_inp <- m_input_srv(
        "decby",
        decby_type,
        decby_title,
        reactive({
          if (!isTruthy(dta_1_incid())) {
            return(decby_choices)
          } else {
            f_get_var_uniq_vals(dta_1_incid(), "decile_var")
          }
        })
      )

      # fltr 3: Incidences type-----------------------------------------------
      incid_inp <- m_input_srv(
        "incid",
        incid_type,
        incid_title,
        reactive({
          if (!isTruthy(dta_2_a())) {
            return(incid_choices)
          } else {
            f_get_var_uniq_vals(dta_2_a(), "measure")
          }
        })
      )

      # fltr 4: Group by ------------------------------------------------------
      grpby_inp <- m_input_srv(
        "grpby",
        grpby_type,
        grpby_title,
        reactive(grpby_choices)
      )

      # fltr 5: Group filter ------------------------------------------------------
      upd_grpfltr <- reactiveVal(FALSE)
      grpfltr_choice_react <- reactive({
        req(dta_2_c())
        isolate(upd_grpfltr(TRUE))
        f_get_var_uniq_vals(dta_2_c(), "group_val")
      })
      grpfltr_inp <- m_input_srv(
        "grpfltr",
        grpfltr_type,
        grpfltr_title,
        grpfltr_choice_react
      )
      # observe({
      #   req(grpfltr_inp())
      #   browser()
      # })

      pltby_inp <- m_input_srv("pltby", pltby_type, pltby_title, reactive(fig$id))

      # Step 3.A Data/Plots preparation -------------------------------------------
      sim_ready <- reactive(req(sim_res()))

      # Step 4. Calculations ------------------------------------------------

      # 4.1 Deciles
      dta_1_incid <- reactive({
        req(sim_ready())
        req(ndec_inp())
        valid_groups <- grpby_choices |> unname() |> setdiff("all_groups")

        sim_ready() |>
          f_calc_deciles_by_sim(
            dec_var = unname(decby_choices),
            wt_var = var_wt,
            n_dec = ndec_inp()
          ) |>
          f_agg_by_decile_by_sim(
            var_decile = str_c(decby_choices, "___decile"),
            var_agg = var_agg,
            var_group = valid_groups,
            wt_var = var_wt
          ) |>
          f_calc_incidence(force_abs = TRUE) |>
          f_format_incidence()
      })

      dta_2_a <- reactive({
        isolate(req(dta_1_incid()))
        req(decby_inp())
        dta_1_incid() |> f_filter_var_generic(decby_inp(), "decile_var")
      })


      dta_2_b <- reactive({
        isolate(req(dta_2_a()))
        req(incid_inp())
        dta_2_a() |> f_filter_var_generic(incid_inp(), "measure")
      })

      dta_2_c <- reactive({
        isolate(req(dta_2_b()))
        req(grpby_inp())
        isolate(upd_grpfltr(FALSE))
        dta_2_b() |> f_filter_grouped_stats(grpby_inp())
      })

      dta_out <- reactive({
        isolate(req(dta_2_c()))
        req(grpfltr_inp())
        req(isTRUE(upd_grpfltr()))
        # browser()
        dta_2_c() |> f_filter_var_generic(grpfltr_inp(), "group_val")
      })

      dta_out_formatted <- reactive({
        dta_2_b() |> f_format_decile_tbl()
      })

      dta_export <- reactive({
        dta_1_incid() |> f_format_decile_tbl()
      })


      dta_fig <- reactive({
        req(dta_out())
        req(grpby_inp())
        # browser()
        dta_out() |>
          f_get_var_uniq_vals("var") |>
          (\(x) set_names(x, x))() |>
          imap(
            ~ {
              title_local <- .y
              if (
                all(grpby_inp() == "all") ||
                  all(grpfltr_inp() == "All observations") ||
                  length(grpfltr_inp()) == 1
              ) {
                dta_out() |>
                  f_filter_var_generic(.x, "var") |>
                  f_plot_gg(
                    x_var = "decile",
                    y_var = "value",
                    x_lab = f_get_app_text("decile"),
                    color_var = "sim",
                    facet_var = NULL,
                    type = "bar",
                    title = title_local,
                    subtitle = str_c(grpfltr_inp(), collapse = ", ")
                  )
              } else {
                dta_out() |>
                  f_filter_var_generic(.x, "var") |> # count(Variable)
                  f_plot_gg(
                    x_var = "decile",
                    y_var = "value",
                    x_lab = f_get_app_text("decile"),
                    color_var = "group_val",
                    facet_var = "sim",
                    type = "bar",
                    title = title_local
                  )
              }

            }
          )
      })

      # Generating plots based on filtered data
      observeEvent(
        dta_fig(),
        {
          req(dta_fig())
          fig$ggs <- dta_fig()
          fig$id <- names(fig$ggs)
          fig$title <- names(fig$ggs)
        },
        ignoreInit = TRUE
      )

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
        selected = pltby_inp,
        force_ly = T
      )

      m_figure_server(
        "tbl1",
        figures = reactive({
          dta_out_formatted() |> f_format_rt(col_min_groups = 1)
        }),
        selected = pltby_inp
      )

      # Step 20. Results export modal ----------------------------------------
      m_download_srv(
        "save_mod_local",
        all_figs = reactive({
          list(list(
            sheet_name = ptitle(),
            meta_tbl = tibble(),
            tbl = dta_export(),
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
              grpby_inp = grpby_inp(),
              pltby_inp = pltby_inp(),
              ndec_inp = ndec_inp(),
              decby_inp = decby_inp(),
              incid_inp = incid_inp()
            ),
            id = fig$id,
            title = fig$title,
            sheet_name = ptitle(),
            tbl = dta_out_formatted(),
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
f_incid_ui_linear <- function(id, add_pl = TRUE) {
  ns <- NS(id)

  # plt-specific controls
  row_1 <- list(m_input_ui(ns("ndec")), m_input_ui(ns("decby")), m_input_ui(ns("incid")))
  row_2 <- list(m_input_ui(ns("grpby")), m_input_ui(ns("grpfltr")))
  row_3 <- list(m_input_ui(ns("pltby")))

  # Drop NULL from list
  tagList(
    bslib::layout_columns(!!!row_1, col_widths = c(4, 4, 4)),
    bslib::layout_columns(!!!row_2, col_widths = c(4, 8)),
    bslib::layout_columns(!!!row_3, col_widths = c(12)),

    navset_card_underline(
      full_screen = TRUE,
      title = m_input_ui(ns("title")),

      nav_panel(
        "Plot",
        card_body(
          m_figure_ui(ns("fig1")),
          fillable = TRUE,
          min_height = "450px",
          max_height = "600px"
        ) #,
        # card_footer("Footer placeholder")
      ),

      nav_panel(
        "Data",
        card_body(
          m_figure_ui(ns("tbl1")),
          fillable = TRUE,
          min_height = "450px" #,
          # max_height = "800px"
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
