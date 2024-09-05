

#' Compute Gini coefficient from a numeric vector
#'
#' @param x,w numeric vectors of values and population weights of equal length
#' @param na.rm logical `TRUE` bu default. To omit NA values
#' @param drop_zero_and_less logical `TRUE` bu default to drop negarive values
#'   and zeros
#'
#' @export
calc_gini <-
  function(x, w = rep(1, length(x)), na.rm = TRUE, drop_zero_and_less = TRUE) {
    if (!na.rm && any(is.na(x))) {
      return(NA_real_)
    }
    if (is.null(w)) {
      w <- rep(1, length(x))
    }
    if(drop_zero_and_less) {
      drop <- x<=0
      w <- w[!drop]
      x <- x[!drop]
    }
    x.sort <- sort(x)
    x.order <- order(x)
    x <- x.sort
    n <- length(x)
    w <- w[x.order] / sum(w)
    w.c <- cumsum(w)
    xw.c <- cumsum(w * x)
    xw.c <- xw.c / xw.c[n]
    Gini <- t(xw.c[-1]) %*% w.c[-n] - t(xw.c[-n]) %*% w.c[-1]
    Gini[[1]]
  }


#' Compute Theil index from a numeric vector
#'
#' @inheritParams calc_gini
#'
#' @export
calc_theil <-
  function(x,
           w = rep(1, length(x)),
           na.rm = TRUE,
           drop_zero_and_less = TRUE)
  {
    if (!na.rm && any(is.na(x))) {
      return(NA_real_)
    }

    if (na.rm) {
      drop <- is.na(x)
      w <- w[!drop]
      x <- x[!drop]
    }

    if (is.null(w)) {
      w <- rep(1, length(x))
    }

    if (drop_zero_and_less) {
      drop <- x <= 0
      w <- w[!drop]
      x <- x[!drop]
    }
    w_ratio <- w / sum(w, na.rm = na.rm)
    x_wt_mean <- sum(x * w_ratio, na.rm = na.rm) / sum(w_ratio, na.rm = na.rm)
    x_ratio <- x / x_wt_mean
    theil <- sum(w_ratio * x_ratio * log(x_ratio), na.rm = na.rm)
    return(theil)
  }




#' Compute Theil index from a numeric vector
#'
#' @inheritParams calc_gini
#' @param pl poverty line
#' @param alpha Foster-Greer-Thorbecke (FGT) povert parameter
#'
#' @export
calc_pov_fgt <-
  function(x,
           pl,
           alpha = 0 ,
           w = rep(1, length(x)),
           na.rm = TRUE,
           ...) {

    if (is.null(w)) {
      w <- rep(1, length(x))
    }

    if (na.rm) {
      drop1 <- is.na(x)
      drop2 <- is.na(w)
      drop3 <- is.na(pl)
      x <- x[!(drop1 | drop2 | drop3)]
      w <- w[!(drop1 | drop2 | drop3)]
      pl <- pl[!(drop1 | drop2 | drop3)]
    }

    sum(w * (ifelse(x < pl, ((pl - x) / pl)^ alpha, 0)), na.rm = TRUE) /
      sum(w, na.rm = TRUE)
}

#' Calculate Gini index for all market concepts for a single simulation
#'
#' @param dta data frame with the simulation results.
#' @param income_vars_tbl a data frame with two character columns: `var` and
#'   `var_title`. `var` is the column with the variable names returned from
#'   the data set. `var_title` is the variable name as we want it to appear
#'   in the resulting table. See for example results of the functions
#'   `get_inc_nm()` or `get_var_nm()`
#' @param wt_var character name of a single variable that should be used
#'   as a weighting variable in calculation. Default is get_wt_nm()$var.
#'   If wt_var is not provided or the one provided is not present in `dta`,
#'   warning will be prompted.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect any_of
#'
#' @export
get_dta_gini <- function(
    dta,
    policy_name = NULL,
    income_vars_tbl = NULL,
    wt_var = "pcweight",
    para_names =
      tibble(
        parameter = c("Gini", "Theil"),
        label = c("Gini index", "Theil index")
      ),
    ...) {

  if (is.null(income_vars_tbl)) income_vars_tbl <- get_inc_nm()

  if (is.null(wt_var)) {
    warning("`wt_var` was not specified. ",
            "Non-weighted statistics is computed.")
    dta <- mutate(dta, dummy_weighting_variable = 1)
    wt_var <- "dummy_weighting_variable"
  }

  if (!is.null(wt_var) && !wt_var %in% names(dta)) {
    warning("Weighting variable '", wt_var, "' is not present in the `dta`. ",
            "Non-weighted statistics is computed."
    )
    dta <- mutate(dta, dummy_weighting_variable = 1)
    wt_var <- "dummy_weighting_variable"
  }

  check_income_vars_tbl <-
    all(c("var", "var_title") %in% names(income_vars_tbl)) %>% isTRUE()

  if (!check_income_vars_tbl) {
    stop("In the argument `income_vars_tbl` some of the required comuns are
         not present. Required coluns are: `var` and `var_title`")
  }

  wt_var_sym <- sym(wt_var)

  dta %>%
    dplyr::select(any_of(income_vars_tbl$var), any_of(wt_var)) %>%
    dplyr::summarise(
      dplyr::across(
        any_of(income_vars_tbl$var),
        ~ calc_gini(., !!wt_var_sym) * 100,
        .names = "Gini_____{.col}"),
      dplyr::across(
        any_of(income_vars_tbl$var),
        ~ calc_theil(., !!wt_var_sym) * 100,
        .names = "Theil_____{.col}"),
      ) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "var",
      values_to = "Value"
      ) %>%
    tidyr::separate(var, into = c("parameter", "var"), sep = "_____") %>%
    # tidyr::pivot_wider(names_from = "para", values_from = "Value") %>%
    dplyr::left_join(income_vars_tbl, by = "var") %>%
    dplyr::left_join(para_names, by = "parameter") %>%
    dplyr::filter(!is.na(label)) %>%
    # dplyr::mutate(Parameter = if_else(is.na(label), parameter, label)) %>%
    # dplyr::mutate(Parameter = label %>% forcats::fct_drop()) %>%
    dplyr::mutate(
      Parameter = label %>%
        factor(., levels = para_names$label) %>%
        forcats::fct_drop()
    ) %>%
    {
      dta <- .
      if (!is.null(policy_name))
        dta <- dplyr::mutate(dta, Simulation = policy_name)
      dta
    } %>%
    dplyr::select(
      Income = var_title,
      tidyselect::any_of("Simulation"),
      any_of(c("Parameter", "Value", "Gini", "Theil"))
    )  %>%
    arrange(Income, Parameter)
}



#' Calculate poverty  for a single simulation at the national poverty line
#'
#' @inheritParams get_dta_gini
#' @param poverty_line numeric, indicating the poverty line in the  currency used.
#'
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect any_of
#'
#' @export
get_dta_pov <- function(
    dta,
    policy_name = NULL,
    poverty_line_var = NULL,
    poverty_line_value = NULL,
    income_vars_tbl = NULL,
    wt_var = NULL,
    para_names =
      tibble(
        parameter = c("rate", "headcount", "gap", "severity"),
        label = c(
          "Poverty rate",
          "Poverty headcount",
          "Poverty gap",
          "Poverty severity"
        ) %>% factor(., levels = .)
      ),
    ...) {

  if (is.null(income_vars_tbl)) income_vars_tbl <- get_inc_nm()

  if (is.null(wt_var)) {
    warning("`wt_var` was not specified. ",
            "Non-weighted statistics is computed.")
    dta <- mutate(dta, dummy_weighting_variable = 1)
    wt_var <- "dummy_weighting_variable"
  }

  if (!is.null(wt_var) && !wt_var %in% names(dta)) {
    warning("Weighting variable '", wt_var, "' is not present in the `dta`. ",
            "Non-weighted statistics is computed."
    )
    dta <- mutate(dta, dummy_weighting_variable = 1)
    wt_var <- "dummy_weighting_variable"
  }

  if ((!is.null(poverty_line_var) &&
       !poverty_line_var %in% names(dta) |
       is.null(poverty_line_var) ) &&
      is.null(poverty_line_value)) {
    stop("Poverty line variable '", poverty_line_var, "' is not present.",
         " Or poverty_line_value is not provided")
  }

  if ((!is.null(poverty_line_var) &&
       !poverty_line_var %in% names(dta) ||
       is.null(poverty_line_var)) &&
      !is.null(poverty_line_value)) {
    warning(
      "Using fixed value '",
      poverty_line_value,
      "' as the poverty line. ",
      "Variable '", poverty_line_var,
      "' was not present in the data."
    )
    dta <- mutate(dta, pl_var = poverty_line_value)
  }

  if (!is.null(poverty_line_var) && poverty_line_var %in% names(dta)) {
    poverty_line_var_2 <- sym(poverty_line_var)
    dta <- mutate(dta, pl_var = {{poverty_line_var_2}})
  }

  wt_var_sym <- sym(wt_var)
  dta %>%
    dplyr::select(
      dplyr::any_of(income_vars_tbl$var),
      dplyr::any_of("pl_var"),
      dplyr::any_of(wt_var)
    ) %>%
    tidyr::pivot_longer(names_to = "var", cols = any_of(income_vars_tbl$var)) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(dplyr::across(
      value,
      list(
        `rate` = ~ calc_pov_fgt(
          x = .,
          pl = pl_var,
          alpha = 0,
          w = !!wt_var_sym,
          na.rm = TRUE
        ) * 100,

        `headcount` = ~ calc_pov_fgt(
          x = .,
          pl = pl_var,
          alpha = 0,
          w = !!wt_var_sym,
          na.rm = TRUE
        ) * sum(!!wt_var_sym, na.rm = TRUE) |> round(),

        `gap` = ~ calc_pov_fgt(
          x = .,
          pl = pl_var,
          alpha = 1,
          w = !!wt_var_sym,
          na.rm = TRUE
        ) * 100,

        `severity` = ~ calc_pov_fgt(
          x = .,
          pl = pl_var,
          alpha = 2,
          w = !!wt_var_sym,
          na.rm = TRUE
        ) * 100
      ),
      .names = "{.fn}"
    ))  %>%
    tidyr::pivot_longer(
      names_to = "parameter",
      cols = dplyr::any_of(c("rate", "headcount", "gap", "severity")),
      values_to = "Value"
    ) %>%
    dplyr::left_join(income_vars_tbl, by = "var")  %>%
    {
      dta <- .
      if (!is.null(policy_name))
        dta <- dplyr::mutate(dta, Simulation = policy_name)
      dta
    } %>%
    dplyr::left_join(para_names, by = "parameter") %>%
    filter(!is.na(label)) %>%
    dplyr::mutate(
      Parameter = label %>%
        factor(., levels = para_names$label) %>%
        forcats::fct_drop()
      ) %>%
    dplyr::select(Income = var_title,
                  tidyselect::any_of("Simulation"),
                  tidyselect::any_of(c("Parameter", "Value"))) %>%
    arrange(Income, Parameter)
}







#' Gini index. prepare clean tables and plots
#'
#' @importFrom rlang eval_tidy quo_squash quo
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom ggplot2 ggplot aes geom_point geom_path scale_x_discrete theme_minimal
#' @importFrom ggplot2 theme_minimal theme labs xlab ylab
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stringr str_wrap
#' @noRd
#' @export
make_gini_pov <-
  function(dta,
           x,
           color,
           title = "",
           y = NULL,
           ylab = NULL,
           xlab = NULL,
           digits = 3,
           ...) {

    tbl_exp <-
      dta %>%
      bind_rows() %>%
      tidyr::pivot_wider(names_from = {{color}}, values_from = any_of(c("value", "Value")))

    tbl_long <- dta %>% bind_rows()
    tbl_flex <- tbl_exp %>% flextable_config(title = title, digits = digits)

    tbl_dt <- tbl_exp
    local_colours <- tbl_long$Simulation %>% unique() %>% length()
    local_colours <- wb_pal(reverse = F)(local_colours)

    plot_dta <-
      tbl_long %>%
      dplyr::group_by(Parameter) %>%
      tidyr::nest()
    plot_dta <- setNames(plot_dta$data, plot_dta$Parameter)

    plot_ly <-
      plot_dta %>%
      imap(~{
        rlang::eval_tidy(rlang::quo_squash(quo({
          .x %>%
            plotly::plot_ly() %>%
            plotly::add_trace(
              x = ~ {{x}},
              y = ~ Value,
              color = ~ {{color}},
              colors = local_colours,
              text = ~ scales::number(Value, 1 / 10 ^ digits),
              hoverinfo = "text+name",
              type = "scatter",
              mode =  "markers+lines"
            )
        }))) %>%
          plotly::layout(
            title = str_c(title, ". ", .y),
            xaxis = list(title = xlab, rangemode = "normal", dtick = 1),
            yaxis = list(title = .y, rangemode = "normal"),
            legend = list(x = 100, y = 0.5)
          ) %>%
          plotly_config()
      })

    plot_gg <-
      plot_dta %>%
      imap( ~ {
        .x %>%
          ggplot2::ggplot() +
          ggplot2::aes(x = {{x}}, y =  Value, color = {{color}}, group = {{color}},
                       text = scales::number( Value, 1 / 10 ^ digits),
                       label = scales::number( Value, 1 / 10 ^ digits)) +
          ggplot2::geom_point(size = 2.5, alpha = 0.75) +
          ggplot2::geom_path(size = 1)  +
          ggplot2::scale_color_manual(values = local_colours) +
          ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
          ggplot2::scale_y_continuous(labels = scales::label_number(1 / 10 ^ digits)) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = c(.85, .85)) +
          ggplot2::labs(title = str_c(title, ". ", .y)) +
          ggplot2::xlab(xlab) +
          ggplot2::ylab(.y)
      })


    plt_indx <- purrr::set_names(x = names(plot_ly), nm = names(plot_ly))

    list(
      title = glue::glue(title),
      tbl_exp = tbl_exp,
      tbl_long = tbl_long,
      tbl_flex = tbl_flex,
      tbl_dt = tbl_dt,
      gg = plot_gg,
      ly = plot_ly,
      plt_indx = plt_indx
    )

  }





