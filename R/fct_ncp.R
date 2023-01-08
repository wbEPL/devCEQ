#' ncp data to make long
#'
#' @noRd
#' @export
fct_ncp_long_dta <- function(dta) {
  dta %>%
    bind_rows() %>%
    left_join(get_var_nm(), by = c("Source" = "var_title")) %>%
    mutate(
      factor = ifelse(is.na(factor), 1, factor),
      value = value * factor
    ) %>%
    select(-factor, -var) %>%
    group_by(Income, Simulation) %>%
    mutate(tot = sum(abs(value), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(`Relative Incidence` = value / Income_value,
           `Absolute Incidence` = value / tot,
           `Level` = value,
           Simulation = as.factor(Simulation)) %>%
    select(-tot, -value)

}


#' NCP wrapper
#' @noRd
#' @export
agg_ncp_total <- function(dta, ...) {
  dta %>%
    dplyr::group_by(Decile, Income_value, Income, Simulation, name) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), ~ sum(., na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Source = "Total Net Cash")
}

#' NCP wrapper
#' @noRd
#' @importFrom stringr str_to_lower regex
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom forcats fct_drop
#'
#' @export
make_nct_dta <- function(dta,
                         # plot_types = c("relative", "absolute", "level"),
                         title_glue =  "Net cash position by deciles of {income_type}",
                         x_title = "Deciles of {income_type}",
                         y_titles =
                           list(relative = "Relative incidence (% of {income_type})",
                                absolute = "Absolute incidence (% of all deciles)",
                                level = "West African CFA franc"),
                         ncp_agg_fn = agg_ncp_total,
                         digits = 2,
                         ...) {

  # browser()
  # Main table
  tbl_long <- dta %>% fct_ncp_long_dta()
  income_type <- tbl_long$Income %>% unique() %>% `[[`(1) %>% str_to_lower()

  # Export tables
  tbl_exp <-
    tbl_long %>%
    dplyr::select(-dplyr::any_of(c("Income_value", "value", "Income"))) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("incid"), ~ scales::percent(., 1 / 10 ^ digits))) %>%
    dplyr::mutate(dplyr::across(dplyr::contains("Level"), ~ scales::number(., 1 / 10 ^ digits, big.mark = ""))) %>%
    tidyr::pivot_longer(c(dplyr::everything(), -Decile, -Simulation, -Source)) %>%
    tidyr::pivot_wider(names_from = Source,
                       values_from = dplyr::contains("value"),
                       names_glue = "{Source}") %>%
    dplyr::mutate(name = factor(name, levels = unique(name))) %>%
    dplyr::arrange(name, Simulation, Decile) %>%
    dplyr::select(`NCP Type` = name, Simulation, Decile , dplyr::everything())

  first_source <- tbl_long$Source %>% unique() %>% sort() %>% `[[`(1) %>% as.character()

  tbl_flex <-
    tbl_exp %>%
    flextable_config(title = glue::glue(title_glue), digits = digits)


  # Preparing key plotting ingredients.

  # Pal
  n_cols <- tbl_long$Source  %>% forcats::fct_drop() %>% levels() %>% length()
  colors_to_expand <- RColorBrewer::brewer.pal(min(max(3, n_cols-1), 12), "Set1")
  plotly_pal <- colorRampPalette(colors_to_expand)

  #
  # tbl_long %>%
  #   group_by(Simulation == unique(Simulation)[[1]]) %>%

  all_plt_dta <-
    tbl_long %>%
    tidyr::pivot_longer(
      cols = c(contains("Relative"),
               contains("Absolute"),
               contains("Level"))) %>%
    # filter(str_detect(name, "Relative")) %>%
    # group_by(Source) %>%
    # mutate(total = sum(value)) %>%
    # ungroup() %>%
    # arrange(desc(total))
    dplyr::mutate(Source = Source %>% forcats::fct_drop()# %>%
                  # fct_reorder(total) %>%
                  # fct_expand("Total Net Cash") %>%
                  # fct_rev()
    ) %>%
    # select(-total)%>%
    dplyr::mutate(name2 = name) %>%
    dplyr::group_by(name2) %>%
    tidyr::nest() %>%
    purrr::pmap(~{
      list(
        plot_dta = .y,
        line_dta = .y %>% ncp_agg_fn()
      ) %>%
        list() %>%
        setNames(.x)
    }) %>%
    unlist(recursive = F)

  all_ggs <-
    all_plt_dta %>%
    purrr::imap(~{
      y_title <- y_titles[str_detect(.y, stringr::regex(names(y_titles),
                                                        ignore_case = T))][[1]]
      fct_make_ncp_gg(
        .x,
        type = .y,
        digits = 2,
        title = glue::glue(title_glue),
        x_title = glue::glue(x_title),
        y_title = glue::glue(y_title),
        plotly_pal = plotly_pal)
    })

  all_lys <-
    all_plt_dta %>%
    purrr::imap(~{
      y_title <- y_titles[str_detect(.y, regex(names(y_titles),
                                               ignore_case = T))][[1]]
      fct_make_ncp_ly(
        .x,
        type = .y,
        digits = 2,
        title = glue::glue(title_glue),
        x_title = glue::glue(x_title),
        y_title = glue::glue(y_title),
        plotly_pal = plotly_pal)
    })

  plt_indx <- purrr::set_names(x = names(all_lys), nm = names(all_lys))

  list(
    title = glue::glue(title_glue),
    tbl_exp = tbl_exp,
    tbl_long = tbl_long,
    tbl_flex = tbl_flex,
    tbl_dt = tbl_exp,
    gg = all_ggs,
    ly = all_lys,
    plt_indx = plt_indx
  )
}
