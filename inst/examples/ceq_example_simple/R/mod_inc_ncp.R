local_make_bar_dta <-
  function(dta, ...) {
    make_bar_dta(
      dta = dta,
      plot_types = c("relative", "absolute"),
      #, "level"),
      title_glue = "{first_source}: Incidence relative (% de {income_type}) et absolue (% de tous les déciles)",
      x_title = "Déciles de {income_type}",
      y_titles =
        list(
          relative = "Incidence relative (% de {income_type})",
          absolute = "Incidence absolue (% de tous les déciles)",
          level = "Franc CFA"
        ),
      digits = 2,
      ...
    )

  }


local_mod_inc_ser <- function(id, sim_res, dec_vars, ...) {
  mod_incidences_server(
    id = id,
    sim_res = sim_res,
    dec_vars = dec_vars,
    make_bar_fn = local_make_bar_dta,
    n_dec_label = "Nombre de déciles",
    dec_by_label = "Déciles par :",
    ...
  )
}


# NCP ---------------------------------------------------------------------

local_agg_ncp_total <- function(dta, ...) {
  # browser()

  dta_1 <-
    dta %>%
    dplyr::group_by(Decile, Income_value, Income, Simulation, name) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Source = "Trésorerie nette totale")

  dta_2 <-
    dta %>%
    filter(!Source %in%
             get_var_nm(
               c(
                 "dtx_total",
                 "itx_total",
                 "pen"
               )
             )$var_title) %>%
    dplyr::group_by(Decile, Income_value, Income, Simulation, name) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Source = "Net cash position")

  bind_rows(dta_1, dta_2)
}

local_make_ncp_dta <-
  function(dta, ...) {
    make_nct_dta(
      dta = dta,
      plot_types = c("relative", "absolute"), #, "level"),
      title_glue =  "Net cash position by deciles of {income_type}",
      x_title = "Deciles of {income_type}",
      y_titles =
        list(relative = "Relative incidence (% of {income_type})",
             absolute = "Absolute incidence (% of all deciles)",
             level = "Currency units"),
      ncp_agg_fn = local_agg_ncp_total,
      digits = 2,
      ...
    )
  }


local_mod_ncp_ser <- function(id, sim_res, dec_vars, ...) {
  mod_incidences_server(
    id = id,
    sim_res = sim_res,
    dec_vars = dec_vars,
    make_bar_fn = local_make_ncp_dta,
    n_dec_label = "Number of deciles",
    dec_by_label = "Déciles par:",
    ...
  )
}

