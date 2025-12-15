#' Plotting helpers
#' @name f_gg
NULL


#' @describeIn f_gg General ggplot2 plotting function for line and bar plots
#' @param dta Data frame containing the data to plot
#' @param x_var Name of the variable for the x-axis (string)
#' @param y_var Name of the variable for the y-axis (string)
#' @param type Type of plot: "line" or "bar" (default: "line")
#' @param color_var Name of the variable for coloring lines/bars (string, optional)
#' @param facet_var Name of the variable for faceting the plot (string, optional)
#' @param ... Additional arguments (currently unused)
#' @return A ggplot2 object representing the plot
#' @export
#' 
f_plot_gg <- function(
  dta, 
  x_var,
  y_var,
  x_lab = "Income concept",
  y_lab = "Value",
  type = "line",
  color_var = NULL,
  facet_var = NULL,
  ...
) {

  # Check if the columns are as specified or they are in their names
  x_var <- ifelse(x_var %in% colnames(dta), x_var, f_get_colname(x_var))
  y_var <- ifelse(y_var %in% colnames(dta), y_var, f_get_colname(y_var))
  color_var <- if (!is.null(color_var)) {
    ifelse(color_var %in% colnames(dta), color_var, f_get_colname(color_var))
  } else {
    NULL
  }

  facet_var <- if (!is.null(facet_var)) {
    ifelse(facet_var %in% colnames(dta), facet_var, f_get_colname(facet_var))
  } else {
    NULL
  }

  # Check if columns exist
  required_cols <- c(x_var, y_var, color_var, facet_var) |> na.omit()
  missing_cols <- setdiff(required_cols, colnames(dta))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing columns in data: {missing_cols}")
  }

  # Check if X is factor an convert it to one is not
  if (!is.factor(dta[[x_var]])) {
    dta <- dta |> mutate(across(any_of(x_var), as_factor))
  }

  # Add appropriate labels
  if (is.null(color_var) || !color_var %in% names(dta)) {
    color_var <- "color_var_temp"
    dta <- dta |> mutate(!!sym(color_var) := "")
  }
  
  if (is.null(facet_var) || !facet_var %in% names(dta)) {
    facet_var <- "facet_var_temp"
    dta <- dta |> mutate(!!sym(facet_var) := "")
  }

  col_measure <- f_get_colname("measure")
  if (!col_measure %in% colnames(dta)) {
    dta <- dta |> mutate(!!sym(col_measure) := "")
  }

  col_group_var <- f_get_colname("group_var")
  if (!col_group_var %in% colnames(dta)) {
    dta <- dta |> mutate(!!sym(col_group_var) := "")
  }

  
  # Check if f_get_colname("measure") exists and use its first element as x label
  y_lab <- "Value"
  if (col_measure %in% colnames(dta)) {
    measure_nm <- dta |> select(any_of(col_measure)) |> pull() |> first()
    if (!is.null(measure_nm) && !is.na(measure_nm) && measure_nm != "") {
      y_lab <- measure_nm
    }
  }  

  # Y scale in % if % is present in the y_lab name
  label_local <- f_num_by_title(xx, title = as.character(y_lab))
  # browser()
  dta <-
    dta |>
    mutate(
      tooltip = glue::glue(
        "{.data[[col_measure]]}: {label_local(.data[[y_var]])}
        {.data[[x_var]]}
        {.data[[col_group_var]]}
        {.data[[color_var]]}
        {.data[[facet_var]]}"
      )
    ) 


  # If x axis has text and it is long, break it
  if (is.character(dta[[x_var]]) || is.factor(dta[[x_var]])) {
    dta <- dta |> mutate(across(any_of(x_var), ~ str_wrap(as.character(.), width = 15) |> as_factor()))
  }

  # Base plot
  p <- dta |>
    ggplot() +
    aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      color = .data[[color_var]],
      fill = .data[[color_var]],
      group = .data[[color_var]],
      text = tooltip
    )

  # Geom based on type
  if (type == "line") {
    p <- p +
      geom_line() +
      geom_point()
  } else if (type == "bar") {
    p <- p +
      geom_col(stat = "identity", position = "dodge")
  } else {
    cli::cli_abort("Unsupported plot type: {type}")
  }

  # Facet if specified
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(.data[[facet_var]]))
  }

  p <- p + scale_y_continuous(labels = label_local)
  p <- p + theme_minimal()
  p <- p + labs(x = x_lab, y = y_lab)
  p <- p + f_scale_color_custom() + f_scale_fill_custom()
  # Rotate x axis text if too long
  if (is.character(dta[[x_var]]) || is.factor(dta[[x_var]])) {
    p <- p + theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  }
  p 

}

#' @describeIn f_gg Safe wrapper around \code{f_plot_gg} to handle errors gracefully
#' @export
#' @importFrom cli cli_warn
f_plot_gg_safe <- function(
  ...
) {
  tryCatch(
    {
      f_plot_gg(...)
    },
    error = function(e) {
      cli::cli_warn("Error in plotting ggplot: {e$message}")
      NULL
    }
  )
}

#' @describeIn f_gg Default colour palette
#' @return A character vector of hex colour codes
#' @export
f_default_colours <- function() {
  c(
    "#1b9e77", "#d95f02", "#7570b3", "#e7298a",
    "#66a61e", "#e6ab02", "#a6761d", "#666666"
  )
}

#' Custom palette function for colour and fill scales Infinite length based on the initial set of colors
#' @describeIn f_gg Custom ggplot2 colour scale using a predefined palette
#' @param ... Additional arguments passed to \code{scale_color_manual}
#' @export
#' 
f_scale_color_custom <- function(...) {
  scale_color_manual(values = rep(f_default_colours(), length.out = 100), ...)
}

#' @describeIn f_gg Custom ggplot2 fill scale using a predefined palette
#' @param ... Additional arguments passed to \code{scale_fill_manual}
#' @export
#' 
f_scale_fill_custom <- function(...) {
  scale_fill_manual(values = rep(f_default_colours(), length.out = 100), ...)
}


#' @describeIn f_gg Function to format a plotly object with consistent layout and configuration
#' @importFrom plotly layout config
#'
format_plotly <- function(
  pltly,
  legend = NULL,
  # list(
  #   # title=list(text='species'),
  #   orientation = "h",
  #   yanchor = "bottom",
  #   y = 1.02,
  #   xanchor = "left",
  #   x = 0.05
  # ),
  xaxis = NULL,
  ...
) {
  # Formating spaces

  aa <- 
    pltly |>
    # plotly::layout(
    #   # legend = legend,
    #   # xaxis = xaxis,
    #   # margin = list(t = 0, b = 0, l = 0, r = 0)
    # ) |>
    plotly::config(
      displaylogo = FALSE,
      showAxisDragHandles = FALSE,
      displayModeBar = FALSE,
      modeBarButtonsToRemove = c(
        'pan2d',
        'select2d',
        'lasso2d',
        "toggleSpikelines",
        "resetScale2d"
      )
    )

  # # Increasing space between axis title and axis labels
  # aa$x$layout$yaxis$title$standoff <- 15
  # aa$x$layout$xaxis$title$standoff <- 10

  # for (i in seq_along(aa$x$layoutAttrs)) {
  #   if (!is.null(aa$x$layoutAttrs[[i]]$yaxis2)) {
  #     if (!is.null(aa$x$layoutAttrs[[i]]$yaxis2$title)) {
  #       aa$x$layoutAttrs[[i]]$yaxis2$title$standoff <- 15
  #     }

  #     if (!is.null(aa$x$layoutAttrs[[i]]$autosize)) {
  #       aa$x$layoutAttrs[[i]]$autosize <- TRUE
  #     }

  #     if (!is.null(aa$x$layoutAttrs[[i]]$legend)) {
  #       aa$x$layoutAttrs[[i]]$legend$x <- 1.15
  #     }
  #   }

  #   if (!is.null(aa$x$layoutAttrs[[i]]$margin)) {
  #     aa$x$layoutAttrs[[i]]$margin$r <- 25
  #   }
  # }

  aa

}