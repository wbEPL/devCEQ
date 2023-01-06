#' Prepares data for incidences bar cart
#'
#' @importFrom stringr str_remove_all str_c str_to_lower regex
#' @import patchwork
#' @noRd
#' @export
#'
make_bar_dta <-
  function(dta,
           plot_types = c("relative", "absolute"), #, "level"),
           title_glue = "{first_source}: Relative (% of {income_type}) and absolute (% of all deciles) incidence",
           x_title = "Deciles of {income_type}",
           y_titles =
             list(
               relative = "Relative incidence (% of {income_type})",
               absolute = "Absolute incidence (% of all deciles)",
               level = "West African CFA franc"
             ),
           digits = 2,
           ...
  ) {

    tbl_long <-
      dta %>%
      bind_rows() %>%
      group_by(Income, Simulation, Source) %>%
      mutate(tot = sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(`Relative Incidence` = value / Income_value,
             `Absolute Incidence` = value / tot,
             `Level` = value,
             Simulation = as.factor(Simulation)) %>%
      select(-tot, -value)

    income_type <- tbl_long$Income %>% unique() %>% `[[`(1) %>% str_to_lower()

    tbl_exp <-
      tbl_long %>%
      select(-any_of(c("Income_value", "value", "Income"))) %>%
      mutate(across(contains("incid"), ~ scales::percent(., 1 / 10 ^ digits))) %>%
      mutate(across(contains("Level"), ~ scales::number(., 1 / 10 ^ digits, big.mark = ""))) %>%
      pivot_wider(names_from = Source,
                  values_from = c(contains("incid"), contains("Level")),
                  names_glue = "{.value}: {Source}") %>%
      select(Simulation, Decile , everything())

    first_source <- tbl_long$Source %>% unique() %>% sort() %>% `[[`(1)

    tbl_flex <-
      tbl_exp %>%
      flextable_config(
        title = glue::glue(title_glue),
        digits = digits
      )

    tbl_dt <- tbl_exp

    # plotly_pal <- wb_pal(reverse = F)
    n_cols <- tbl_long$Simulation %>% forcats::fct_drop() %>% levels() %>% length()
    colors_to_expand <- RColorBrewer::brewer.pal(min(max(3, n_cols-1), 12), "Set1")
    plotly_pal <- colorRampPalette(colors_to_expand)

    all_plots <-
      tbl_long %>%
      group_by(Source) %>%
      nest() %>%
      mutate(#
        ly_comb = map2(data, Source, ~ {
          single_tbl <- .x

          plot_n <- length(plot_types)
          if (length(plot_types) == 1) title_positions <- c(0.5)
          if (length(plot_types) == 2) title_positions <- c(0.25, 0.75)
          if (length(plot_types) == 3) title_positions <- c(0.165, 0.495, 0.825)
          plot_titles <-
            plot_types %>%
            map2(title_positions, ~{
              list(x = .y,
                   y = 1.0,
                   text = glue::glue(y_titles[[.x]]),
                   showarrow = F,
                   font = list(size = 16),
                   xanchor = "center",
                   yanchor = "top",
                   xref='paper',
                   yref='paper'
              )
            })

          all_ly <-
            plot_types %>%
            list(seq_along(.)) %>%
            pmap(~ {
              single_tbl %>%
                make_bar_ly(type = ..1,
                            showleg = ..2 == 1,
                            digits = digits,
                            y_titles = y_titles,
                            x_title = x_title,
                            income_type = income_type,
                            plotly_pal = plotly_pal)
            })


          all_ly %>%
            plotly::subplot(
              .,
              nrows = 1,
              titleX = T,
              titleY = T,
              margin = c(0.025, 0.025, 0.55, 0.05)
            ) %>%
            layout(
              title = list(text = paste0("<b>", .y, "</b>"), y = 0.99, x = 0.5, xanchor = 'center', yanchor =  'top',
                           font = list(size = 20)),
              annotations  = plot_titles)
        }),

        gg_comb = map2(data, Source, ~ {

          # browser()
          single_tbl <- .x
          all_ggplts <-
            plot_types %>%
            list(seq_along(.)) %>%
            pmap( ~ {
              single_tbl %>%
                make_bar_gg(type = ..1,
                            digits = digits,
                            y_titles = y_titles,
                            x_title = x_title,
                            income_type = income_type,
                            plotly_pal = plotly_pal)
            })

          # %>%
          #   `[`(1)

          # library(patchwork)
          # patch <-
          all_ggplts %>%
            reduce(~ .x | .y) +
            patchwork::plot_layout(guides = "collect") +
            patchwork::plot_annotation(title = as.character(.y)) &
            theme(legend.position = "bottom")


          #   + plot_annotation(title = as.character(.y))
          # all_ggplts %>%
          #   ggpubr::ggarrange(
          #     plotlist = .,
          #     nrow = 1,
          #     common.legend = TRUE,
          #     legend = "right"
          #     ) %>%
          #   ggpubr::annotate_figure(., top = ggpubr::text_grob(.y)) #%>%
          #   # list() %>%
          #   # set_names(.y)
        }))

    # browser()

    plot_codes <-
      all_plots$Source %>%
      stringr::str_remove_all("[^[:alnum:]]") %>%
      stringr::str_c("gg", .) %>%
      stringr::str_to_lower()

    plt_indx <- set_names(x = plot_codes, nm = all_plots$Source )

    plot_gg <- set_names(all_plots$gg_comb,
                         nm = all_plots$Source %>%
                           stringr::str_remove_all("[^[:alnum:]]") %>%
                           stringr::str_c("gg", .) %>%
                           stringr::str_to_lower())

    plot_ly <- set_names(all_plots$ly_comb,
                         nm = all_plots$Source %>%
                           stringr::str_remove_all("[^[:alnum:]]") %>%
                           stringr::str_c("gg", .) %>%
                           stringr::str_to_lower())


    list(
      title = glue::glue(title_glue),
      tbl_exp = tbl_exp,
      tbl_long = tbl_long,
      tbl_flex = tbl_flex,
      tbl_dt = tbl_dt,
      gg = plot_gg,
      ly = plot_ly,
      plt_indx = plt_indx
    )

  }



#' Make a plotly for one source.
#' @importFrom scales number_format percent_format
#' @import ggplot2
#' @noRd
#'
make_bar_ly <-
  function(one_bar_dta,
           type = "relative",
           showleg = TRUE,
           digits = 2,
           y_titles = NULL,
           x_title = NULL,
           income_type,
           plotly_pal,
           ...) {
    plot_var_name <-
      one_bar_dta %>%
      select(contains(type)) %>%
      names()
    plot_var_name2 <-
      plot_var_name %>%
      str_c("`", ., "`")

    if (str_detect(type, regex("relat", ignore_case = T)) ||
        str_detect(type, regex("absol", ignore_case = T))) {
      num_round <- scales::percent_format(accuracy = 1 / 10 ^ digits)
      y_percent <- list(tickformat = str_c(1 / 10 ^ digits, "%"))
    } else {
      num_round <- scales::number_format(accuracy = 1 / 10 ^ digits)
      y_percent <- list(NULL)
    }

    y_title <- y_titles[[type]]
    # browser()
    one_bar_dta %>%
      select(Decile, Simulation, one_of(plot_var_name)) %>%
      plotly::plot_ly() %>%
      plotly::add_bars(
        x = ~ Decile ,
        y = ~ eval(parse(text = plot_var_name2)),
        text = ~ num_round(eval(parse(text = plot_var_name2))),
        hoverinfo = "text+name",
        name = ~ Simulation ,
        color = ~ Simulation ,
        colors = ~ plotly_pal(length(unique(Simulation))),
        showlegend = showleg
      ) %>%
      plotly::layout(
        xaxis = list(title = glue::glue(x_title),
                     dtick = 1),
        yaxis = list(
          title = glue::glue(y_title),
          rangemode = "tozero"
        ) %>%
          append(y_percent)
      )
  }

#' Make a plotly for one source.
#' @import ggplot2
#' @noRd
#'
make_bar_gg <-
  function(one_bar_dta,
           type = "relative",
           digits = 2,
           y_titles = NULL,
           x_title = NULL,
           income_type,
           plotly_pal,
           ...) {

    plot_var_name <-
      one_bar_dta %>%
      select(contains(type)) %>%
      names()

    plt_dta <-
      one_bar_dta %>%
      select(Decile, Simulation, one_of(plot_var_name)) %>%
      pivot_longer(cols = matches(regex(str_c(type, collapse = "|"), ignore_case = T)))

    if (str_detect(type, regex("relat", ignore_case = T)) ||
        str_detect(type, regex("absol", ignore_case = T))) {
      num_round <- scales::percent_format(accuracy = 1 / 10 ^ digits)
    } else {
      ndig <- max(plt_dta$value) %>% log10() %>% floor(.) + 1
      if (ndig <= 3) {
        rnd <- list("", 1)
      } else if (ndig < 6) {
        rnd <- list(" K", 1000)
      } else if (ndig < 9) {
        rnd <- list(" M", 1000000)
      } else {
        rnd <- list(" B", 1000000000)
      }
      num_round <- scales::number_format(accuracy = 1 / 10 ^ digits,
                                         scale = 1/rnd[[2]], suffix = rnd[[1]])

    }
    y_title <- y_titles[[type]]
    plt_dta %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = Decile, y = value, colour = Simulation, fill = Simulation) +
      ggplot2::geom_bar(position = "dodge", stat = "identity") +
      ggplot2::theme_minimal() +
      # ggplot2::theme(legend.position = "bottom") +
      # ggplot2::labs(title = glue::glue(title_glue)) +
      ggplot2::scale_y_continuous(labels = num_round) +
      ggplot2::scale_color_manual(
        aesthetics = c("colour", "fill"),
        values = plotly_pal(length(levels(plt_dta$Simulation))),
        breaks = levels(plt_dta$Simulation)
      ) +

      # ggplot2::scale_color_brewer(palette = "Paired") +
      # ggplot2::scale_fill_brewer(palette = "Paired") +
      ggplot2::xlab(glue::glue(x_title)) +
      ggplot2::ylab(glue::glue(y_title))

  }

