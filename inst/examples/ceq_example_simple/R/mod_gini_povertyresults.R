
local_get_dta_gini <- function(dta, policy_name, ...) {
  get_dta_gini(
    dta = dta,
    policy_name = policy_name,
    wt_var = get_wt_nm(),
    income_vars_tbl = get_inc_nm(),
    para_names =
      tibble(
        parameter = c("Gini", "Theil"),
        label = c("Indice de Gini", "Indice de Theil")
      ),
    ...
  )
}

local_get_dta_pov <-
  function(dta,
           policy_name,
           poverty_line_var,
           poverty_line_value = 500000,
           ...) {
    get_dta_pov(
      dta = dta,
      policy_name = policy_name,
      wt_var = get_wt_nm(),
      income_vars_tbl = get_inc_nm(),
      poverty_line_var = poverty_line_var,
      poverty_line_value = poverty_line_value,
      para_names =
        tibble(
          parameter = c("rate", "headcount", "gap", "severity"),
          label = c(
            "L'incidence de pauvreté (rate)",
            "Le nombre de pauvres (headcount)",
            "La profondeur de la pauvreté (gap)",
            "La sévérité de la pauvreté (severity)"
          ) %>% factor(., levels = .)
        ),
      ...
    )
  }

local_gini_server <-
  function(id, sim_res, ...) {
    mod_gini_pov_gen_server(
      id = id,
      sim_res = sim_res,
      title = "Inégalité",
      export_btn_title = "Enregistrer",
      pl_choices = NULL,
      pl_title = NULL,
      get_dta_fn = local_get_dta_gini,
      make_plot_fn = function(dta, ...) {
        make_gini_pov(
          dta = dta,
          x = Income,
          color = Simulation,
          title = "Inégalité",
          xlab = "Revenu",
          ...
        )
      },
      ...
    )
  }

local_pov_server <-
  function(id, sim_res, ...) {
    mod_gini_pov_gen_server(
      id = id,
      sim_res = sim_res,
      title = "Pauvreté",
      export_btn_title = "Enregistrer",
      pl_choices = c(
        "National" = "zref",
        "2.15 USD (2017 PPP)" = "line_1",
        "3.65 USD (2017 PPP)" = "line_2",
        "6.85 USD (2017 PPP)" = "line_3"
      ),
      pl_title = "Seuil de pauvreté",
      get_dta_fn = local_get_dta_pov,
      make_plot_fn = function(dta, ...) {
        make_gini_pov(
          dta = dta,
          x = Income,
          color = Simulation,
          title = "Pauvreté",
          xlab = "Revenu",
          ...
        )
      },
      ...
    )
  }

local_gini_pov_ui <- function(id, ...) {
  ns <- NS(id)
  fluidRow(
    column(6, mod_gini_ui(ns("gini-srv"))),
    column(6, mod_gini_ui(ns("pov-srv")))
  )
}


local_gini_pov_ui1 <- function(id, ...) {
  ns <- NS(id)
  # fluidRow(
    # column(6, mod_gini_ui(ns("gini-srv"))),
    column(12, mod_gini_ui(ns("pov-srv")))
  # )
}

local_gini_pov_ui2 <- function(id, ...) {
  ns <- NS(id)
  # fluidRow(
    column(12, mod_gini_ui(ns("gini-srv")))#,
    # column(6, mod_gini_ui(ns("pov-srv")))
  # )
}

local_gini_pov_server <-
  function(id, sim_res, ...) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      local_gini_server("gini-srv", sim_res = sim_res, ...)
      local_pov_server("pov-srv", sim_res = sim_res, ...)
      
    })
  }

