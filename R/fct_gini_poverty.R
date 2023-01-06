

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
    dplyr::summarise(dplyr::across(any_of(income_vars_tbl$var),
                                   ~ calc_gini(., !!wt_var_sym))) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = "var",
                        values_to = "Gini") %>%
    dplyr::left_join(income_vars_tbl, by = "var") %>%
    {
      dta <- .
      if (!is.null(policy_name))
        dta <- dplyr::mutate(dta, Simulation = policy_name)
      dta
    } %>% 
    dplyr::select(Income = var_title, tidyselect::any_of("Simulation"), Gini)
}



#' Calculate poverty  for a single simulation at the national poverty line
#' 
#' @inheritParams get_dta_gini
#' @param poverty_line numeric, indicating the poverty line in Indonesian Rupiah
#'     per person per day.
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
    wt_var = "pcweight",
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
  
  if (!is.null(poverty_line_var) && !poverty_line_var %in% names(dta) | 
      is.null(poverty_line_var) && is.null(poverty_line_value)) {
    stop("Poverty line variable '", poverty_line_var, "' is not present.",
         " Or poverty_line_value is not provided")
  }
  
  if (!is.null(poverty_line_var) && poverty_line_var %in% names(dta)) {
    poverty_line_var_2 <- sym(poverty_line_var)
    dta <- mutate(dta, local_povline_variable = {{poverty_line_var_2}})
  }
  
  if (!is.null(poverty_line_value)) {
    dta <- mutate(dta, local_povline_variable = poverty_line_value)
  }
  
  wt_var_sym <- sym(wt_var)
  
  dta %>% 
    select(any_of(income_vars_tbl$var), any_of("local_povline_variable"), 
           any_of(wt_var)) %>% 
    summarise(
      across(any_of(income_vars_tbl$var), 
             ~ sum((. < local_povline_variable ) * !!wt_var_sym, na.rm = TRUE) /
               sum(!!wt_var_sym, na.rm = TRUE) * 
               100)
    ) %>% 
    tidyr::pivot_longer(cols = everything(), names_to = "var", values_to = "Poverty") %>%
    left_join(income_vars_tbl, by = "var")  %>% 
    {
      dta <- .
      if (!is.null(policy_name))
        dta <- dplyr::mutate(dta, Simulation = policy_name)
      dta
    } %>% 
    select(Income = var_title, tidyselect::any_of("Simulation"), Poverty)
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
  function(dta, y, x, color,
           title = "", ylab = NULL, xlab = NULL,
           digits = 3) {
    tbl_exp <- dta %>% bind_rows() %>%
      tidyr::pivot_wider(names_from = {{color}}, values_from = {{y}})
    
    tbl_long <- dta %>% bind_rows()
    tbl_flex <- tbl_exp %>% flextable_config(title = title, digits = digits)
    
    tbl_dt <- tbl_exp
    local_colours <- tbl_long$Simulation %>% unique() %>% length()
    local_colours <- wb_pal(reverse = F)(local_colours)
    
    plot_ly <-
      rlang::eval_tidy(rlang::quo_squash(quo({
        tbl_long %>%
          plotly::plot_ly() %>%
          plotly::add_trace(
            x = ~ {{x}},
            y = ~ {{y}},
            color = ~ {{color}},
            colors = local_colours,
            text = ~ scales::number({{y}}, 1 / 10 ^ digits),
            hoverinfo = "text+name",
            type = "scatter",
            mode =  "markers+lines"
          )
        
      }))) %>%
      plotly::layout(
        title = title,
        xaxis = list(title = xlab, rangemode = "normal", dtick = 1),
        yaxis = list(title = ylab, rangemode = "normal"),
        legend = list(x = 100, y = 0.5)
      ) %>%
      plotly_config()
    
    plot_gg <-
      tbl_long %>%
      ggplot2::ggplot() +
      ggplot2::aes(x = {{x}}, y =  {{y}}, color = {{color}}, group = {{color}},
                   text = scales::number( {{y}}, 1 / 10 ^ digits),
                   label = scales::number( {{y}}, 1 / 10 ^ digits)) +
      ggplot2::geom_point(size = 2.5, alpha = 0.75) +
      ggplot2::geom_path(size = 1)  +
      ggplot2::scale_color_manual(values = local_colours) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
      ggplot2::scale_y_continuous(labels = scales::label_number(1 / 10 ^ digits)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = c(.85, .85)) +
      ggplot2::labs(title = title) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab)
    
    list(
      title = glue::glue(title),
      tbl_exp = tbl_exp,
      tbl_long = tbl_long,
      tbl_flex = tbl_flex,
      tbl_dt = tbl_dt,
      gg = plot_gg,
      ly = plot_ly
    )
    
  }





