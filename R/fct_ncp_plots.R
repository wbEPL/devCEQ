
#' NCP ggplot function
#' @noRd
#' @export
#'
fct_make_ncp_gg <- function(one_plt_dta,
                            type = "relative",
                            digits = 2,
                            title = NULL,
                            x_title = x_title,
                            y_title = y_title,
                            plotly_pal,
                            ...) {

  plt_dta <- one_plt_dta$plot_dta
  line_dta <- one_plt_dta$line_dta
  # type <- "Level"

  if (str_detect(type, regex("relat", ignore_case = T)) ||
      str_detect(type, regex("absol", ignore_case = T))) {
    num_round <- scales::percent_format(accuracy = 1 / 10 ^ digits)
  } else {
    # browser()
    ndig <- abs(max(plt_dta$value, na.rm = TRUE)) %>% log10() %>% floor(.) + 1
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

  plt_dta %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = Decile, y = value, fill = Source) +
    ggplot2::geom_bar(position = "stack", stat = "identity", colour = NA) +
    ggplot2::geom_path(
      data = line_dta,
      aes(x = Decile, y = value, group = Source, color = Source, linetype = Source),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = line_dta,
      aes(x = Decile, y = value, group = Source, color = Source, shape = Source),
      inherit.aes = FALSE
    ) +
    ggplot2::facet_grid(~ Simulation) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right") +
    ggplot2::labs(title = title) +
    ggplot2::scale_y_continuous(labels = num_round, n.breaks = 10) +
    ggplot2::scale_color_grey(start = 0.01, end = 0.7) +
    ggplot2::scale_fill_manual(
      values = (plotly_pal(length(levels(plt_dta$Source)))),
      breaks = (levels(plt_dta$Source))
    ) +
    ggplot2::xlab(glue::glue(x_title)) +
    ggplot2::ylab(glue::glue(y_title)) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL, order = 1),
                    colour = ggplot2::guide_legend(title = NULL, order = 2),
                    linetype = ggplot2::guide_legend(title = NULL, order = 2),
                    shape = ggplot2::guide_legend(title = NULL, order = 2))

}




#' NCP plotly function
#' @noRd
#' @export
#'
fct_make_ncp_ly <- function(one_plt_dta,
                            type = "relative",
                            digits = 2,
                            title = NULL,
                            x_title = x_title,
                            y_title = y_title,
                            plotly_pal,
                            ...) {


  plt_dta <- one_plt_dta$plot_dta
  line_dta <- one_plt_dta$line_dta
  # y_title <- y_titles[str_detect(type, regex(names(y_titles), ignore_case = T))][[1]]

  # type <- "Level"

  if (str_detect(type, regex("relat", ignore_case = T)) ||
      str_detect(type, regex("absol", ignore_case = T))) {
    num_round <- scales::percent_format(accuracy = 1 / 10 ^ digits)
    y_percent <- list(tickformat = str_c(1 / 10 ^ digits, "%"))
  } else {
    ndig <- abs(max(plt_dta$value, na.rm = TRUE)) %>% log10() %>% floor(.) + 1
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
    y_percent <- list(NULL)

  }


  # if (str_detect(scale_suffix, regex("\\%", ignore_case = T))) {
  #   num_round <- scales::percent_format(accuracy = 1 / 10 ^ digits)
  # } else {
  #   num_round <- scales::number_format(accuracy = 1 / 10 ^ digits, suffix = scale_suffix)
  #
  # }

  plt_dta %>%
    mutate(sim2 = Simulation) %>%
    group_by(Simulation) %>%
    tidyr::nest() %>%
    left_join(
      line_dta %>%
        mutate(sim2 = Simulation) %>%
        group_by(Simulation) %>%
        tidyr::nest() %>%
        rename(data2 = data) %>%
        ungroup(),
      "Simulation"
    ) %>%
    mutate(
      data = map(
        data,
        ~ {
          .x %>%
            # filter(!str_detect(Source, regex("Total net", ignore_case = T))) %>%
            arrange(Decile, desc(Source)) %>%
            group_by(Decile, value < 0) %>%
            mutate(base_val = 0) %>%
            mutate(n = row_number(), base_val = 0) %>%
            mutate(base_val = ifelse(n == 1, base_val, lag(value)) %>% cumsum()) %>%
            ungroup() #%>%
          # bind_rows(filter(.x, str_detect(Source, regex("Total net", ignore_case = T))))
        })) %>%
    ungroup() %>%
    mutate(nnn = row_number()) %>%
    mutate(
      ply = pmap(list(data, Simulation, nnn, data2), ~{
        col_tibble <-
          tibble(
            Source = unique(..4$Source),
            col =  grDevices::grey.colors(length(unique(..4$Source)))
          )
        line_dta <- ..4 %>% left_join(col_tibble, by = "Source")
        plotly::plot_ly() %>%
          plotly::add_bars(
            data = ..1,
            x = ~ Decile,
            y = ~ value,
            base = ~ base_val,
            text = ~ num_round(value),
            hoverinfo = "text+name",
            name = ~ Source,
            color = ~ Source,
            colors = ~ plotly_pal(length(unique(Source))),
            legendgroup = ~ "Source",
            showlegend = ..3 == 1
          )  %>%
          plotly::add_trace(
            inherit = FALSE,
            mode = "lines+markers",
            type = "scatter",
            data = line_dta,
            x = ~ Decile,
            y = ~ value,
            linetype = ~ Source,
            text = ~ num_round(value),
            hoverinfo = "text+name",
            name = ~ Source,
            showlegend = ..3 == 1,
            line = list(color = line_dta$col),
            marker = list(color = line_dta$col)
          ) %>%
          plotly::layout(
            xaxis = list(
              title = glue::glue(x_title),
              # rangemode = "tozero",
              dtick = 1
            ),
            yaxis = list(
              title = glue::glue(y_title),
              rangemode = "tozero",
              range = NULL
            ) %>%
              append(y_percent),
            # legend = list(orientation = 'horizontal', x = 0.7, y =-.1),
            barmode = "stack",
            annotations =
              list(
                x = 0.5,
                y = 0.98,
                text = ..2,
                xref = "paper",
                yref = "paper",
                xanchor = "center",
                yanchor = "bottom",
                showarrow = FALSE
              )
          )
      })) %>%
    pull(ply) %>%
    plotly::subplot(., nrows = 1, shareY = T, titleY = F) %>%
    plotly::layout(
      # legend = list(orientation = 'horizontal', x = 0.6),
      xaxis = list(title = glue::glue(x_title)),
      yaxis = list(title = glue::glue(y_title))
    )

}


