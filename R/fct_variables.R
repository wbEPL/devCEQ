#' Variable defiitions and dictionaries helpers.
#' 
#' Key approach is the following:
#' 1. Package contains default dictionaries for each variable. 
#'    These are defined in functions with \code{f_var_<type>_default} names.
#'    These functions are: \code{f_var_dic_default}, \code{f_var_wt_default}, 
#'   \code{f_var_inc_default}, \code{f_var_group_default}, \code{f_measure_dic_default}.
#'   \code{f_colnames_dic_default}.
#' 2. User can define custom dictionaries by defining functions with names
#'   \code{f_var_<type>} and \code{f_measure_dic} in the global environment.
#' 3. Main functions use either default or custom dictionaries depending on
#'   whether custom functions exist.
#' 
#' 
#' @name f_var_helpers
NULL

#' @describeIn f_var_helpers Variable IDs and labels returned in a data frame
#' 
#' @param vars Character vector of variable IDs to filter (default: NULL, all variables)
#' @param suffix Optional suffix to append to variable IDs (default: NULL)
#' @param reorder Logical, whether to reorder output based on \code{vars} order (default: TRUE)
#' @param dic_default Function returning the default variable dictionary (default: \code{f_var_dic_default})
#' @param dic_custom_name Name of custom variable dictionary function (default: "f_var_dic")
#' @param ... Additional arguments (currently unused)
#'
#' @returns data frame with variables names
#' @importFrom shiny isTruthy
#' @importFrom stringr str_c
#' @importFrom dplyr mutate select filter
#'
#' @export
get_var_nm <- function(
  vars = NULL,
  suffix = NULL,
  reorder = TRUE,
  dic_default = f_var_dic_default,
  dic_custom_name = "f_var_dic",
  ...
) {
  var_dic_local <- dic_default()

  if (exists(dic_custom_name, mode = "function")) {
    var_dic_local <- dic_custom_name |> paste0("()") |> parse(text = _) |> eval()
  }
  
  rename_measure <- FALSE
  if ("measure" %in% colnames(var_dic_local)) {
    rename_measure <- TRUE
    var_dic_local <- var_dic_local %>% rename(var = measure)
  }

  rename_measure_title <- FALSE
  if ("measure_title" %in% colnames(var_dic_local)) {
    rename_measure_title <- TRUE
    var_dic_local <- var_dic_local %>% rename(var_title = measure_title)
  }

  dta <-
    var_dic_local |>
    mutate(var_title = var_title |> fct_keep_dup_string() |> as_factor()) |>
    select(var, var_title, any_of("factor"))

  if (!missing(vars) && !is.null(vars)) {
    dta <- dta %>% filter(var %in% vars)

    if (any(!vars %in% dta$var)) {
      missing_vars <- vars[!vars %in% dta$var]
      cli::cli_warn(
        c(x = "`get_var_nm()`: Variables '{missing_vars}' are not found in the dictionary",
          i = "They are added with default titles. Please check the custom variables dictionary.")
      )
      dta_missing <- tibble(
        var = missing_vars,
        var_title = missing_vars |> fct_keep_dup_string(),
         factor = 1
      )
      dta <- bind_rows(dta, dta_missing)
    }
    
    dta <- dta %>%
      mutate(var_title = factor(var_title, levels = var_title, labels = var_title))
  }

  if (reorder) {
    dta <- dta |> 
      mutate(var = as_factor(var) |> fct_relevel(vars)) |>
      arrange(var) |>
      mutate(var = as.character(var), var_title = as_factor(var_title))
  }
  
  if (!missing(suffix) && !is.null(suffix)) {
    dta <- dta %>% mutate(var = str_c(var, suffix))
  }

  if (rename_measure) {
    dta <- dta %>% rename(measure = var)
  }

  if (rename_measure_title) {
    dta <- dta %>% rename(measure_title = var_title)
  }  

  return(dta)
}

#' @describeIn f_var_helpers  Converts variables nems and IDs from a data frame to a named vector
#' @returns a named character vector with labels in names and codes in values
#' 
#' @export
f_var_names_vector <- function(var_nms = get_inc_nm()) {
  if (all(c("var", "var_title") %in% colnames(var_nms))) {
    set_names(var_nms$var, var_nms$var_title)
  } else if (all(c("measure", "measure_title") %in% colnames(var_nms))) {
    set_names(var_nms$measure, var_nms$measure_title)
  } else {
    cli::cli_abort(
      c(
        x = "Input data frame had columns {colnames(var_nms)}.",
        i = "It must have either 'var' and 'var_title' or 'measure' and 'measure_title' columns."
      )
    )
  }
}


#' @describeIn f_var_helpers Helper to label duplicated variable names as different factors levels.
#' 
#' @export
fct_keep_dup_string <- function(val) {
  zero_width_space <- "\u200b"
  val_chr <- as.character(val)
  n_dup <- ave(seq_along(val_chr), val_chr, FUN = seq_along)
  val_unique <- paste0(val_chr, strrep(zero_width_space, n_dup - 1))
  val_unique
}


#' @describeIn f_var_helpers All measures IDs and labels returned in a data frame
#' @returns a data frame with measures names
#' @export
get_measure_nm <- function(x = NULL) {
  get_var_nm(
    vars = x,
    suffix = NULL,
    reorder = TRUE,
    dic_default = f_measure_dic_default,
    dic_custom_name = "f_measure_dic"
  )
}


#' @describeIn f_var_helpers Income Concepts variables IDs and labels returned in a data frame
#'
#' @returns a data frame with variables names
#'
#' @export
get_inc_nm <- function(suffix = NULL, reorder = TRUE) {
  inc_vars <- f_var_inc_default()
  if (exists("f_var_inc", mode = "function")) {
    inc_vars <- f_var_inc()
  } 
  inc_vars |> get_var_nm(suffix = suffix, reorder = reorder)
}

#' @describeIn f_var_helpers Weight variable IDs and labels in a data frame
#'
#' @returns a data frame with variables names
#'
#' @export
get_wt_nm <- function(suffix = NULL) {
  wt_var <- f_var_wt_default()
  if (exists("f_var_wt", mode = "function")) {
    wt_var <- f_var_wt()
  } 

  wt_var |> get_var_nm(suffix = suffix, reorder = TRUE) |> pull(var)
}


#' @describeIn f_var_helpers Grouping variables IDs and labels in a data frame
#' @returns a data frame with variables names
#' @export
get_group_nm <- function(suffix = NULL, reorder = TRUE) {
  group_vars_fn <- f_var_group_default()
  if (exists("f_var_group", mode = "function")) {
    group_vars_fn <- f_var_group()
  } 
  group_vars_fn |> get_var_nm(suffix = suffix, reorder = TRUE)
}

#' @describeIn f_var_helpers Grouping variables IDs and labels in a data frame
#' @returns a data frame with variables names
#' @export
get_pl_nm <- function(suffix = NULL, reorder = TRUE) {
  group_vars_fn <- f_var_pl_default()
  if (exists("f_var_pl", mode = "function")) {
    group_vars_fn <- f_var_pl()
  } 
  group_vars_fn |> get_var_nm(suffix = suffix, reorder = TRUE)
}


#' @describeIn f_var_helpers Default dictionary for variable labels
#'
f_var_dic_default <- function() {
    tribble(
      ~factor,  ~ var                , ~var_title,

      # Benefits
      1, "ben_pen"   , "Pension benefits",
      1, "ben_unemp", "Unemployment benefits",
      1, "ben_total"     , "Benefits total",

      # Direct Transfers
      1, "dtr_prog1"  , "Directs transfer progarm 1",
      1, "dtr_prog2"  , "Directs transfer progarm 2",
      1, "dtr_total"  , "Direct transfers total",


      # Subsidy
      1, "sub_energy"      , "Energy subsidy",
      1, "sub_food"      , "Food subsidy",
      1, "sub_total"      , "Subsidies total",
  
      # Direct taxes
      -1, "dtx_prog1"      , "Direct taxes program 1",
      -1, "dtx_prog2"      , "Direct taxes program 2",
      -1, "dtx_prog3"      , "Direct taxes on self-employed",
      -1, "dtx_total"      , "Direct taxes on employers income",

      # Indirect taxes
      -1, "itx_vat"           , "Indirect taxes: VAT",
      -1, "itx_excise"        , "Indirect taxes: Excise duties",
      -1, "itx_total"         , "Indirect taxes, total",

      # # Contributions for health and pension
      # -1, "dtp_pen", "Pension contribution paid by employees",
      # -1, "dtp_health", "Health contribution paid by employees",
      # -1, "dtp_total", "Total contributions ",

      
      # In kind
      1, "ink_helth"    , "Health",
      1, "ink_education", "Education",
      1, "ink_total"    , "In-kind transfers total",

      # Income concepts
      1,        "ym"             ,  "Market income",
      1,        "yp"             ,  "Market income plus pension",
      1,        "yn"             ,  "Net market income" ,
      1,        "yg"             ,  "Gross income" ,
      1,        "yd"             ,  "Disposable income" ,
      1,        "yc"             ,  "Consumable income" ,
      1,        "yf"             ,  "Final income" ,

      # Auxiliary variables
      1,        "hhwt"             ,  "Weight: households" ,
      1,        "hhid"             ,  "HH id" ,
      1,        "hhsize"           ,  "HH size",

      1,       "group_1"         ,  "Gender typology",
      1,       "group_2"         ,  "Urban/Rural typology",
      1,       "group_3"         ,  "Age group typology",

      1,       "all"             ,  "All observations",
      1,       "all_groups"      ,  "All groups combined",

      1,       "pl_nat"           ,  "National poverty line",
      1,       "pl_190"           ,  "Poverty line at PPP 1.90",
      1,       "pl_500"           ,  "Poverty line at PPP 5.00"

    )
}

#' @describeIn f_var_helpers Weight variables names 
#'
f_var_wt_default <- function() {
  "hhwt"
}

#' @describeIn f_var_helpers Income variables names
#'
f_var_inc_default <- function() {
    c("ym", "yp", "yg", "yd", "yc", "yf")
}


#' @describeIn f_var_helpers Grouping variables names
#'
f_var_group_default <- function() {
    c("all", "group_1", "group_2", "group_3", "all_groups")
}

#' @describeIn f_var_helpers Income variables names
#'
f_var_pl_default <- function() {
    c("pl_190", "pl_nat", "pl_500")
}



#' @describeIn f_var_helpers Default dictionary for measures labels
#' 
f_measure_dic_default <- function() {
  tribble(
    ~measure, ~measure_title,
    "hc",     "Number of poor",
    "fgt0",   "Poverty rate (FGT0), %",
    "fgt1",   "Poverty gap index (FGT1)",
    "fgt2",   "Poverty severity index (FGT2)",
    "gini",   "Gini coefficient",
    "theil",  "Theil index",
    "n",      "N observations",
    "pop",    "Population"
  )
}


#' @describeIn f_var_helpers Default column names dictionary
#' 
f_colnames_dic_default <- function() {
  c(
    "group_var" = "Grouping variable",
    "group_val" = "Group",
    "var" = "Variable",
    "measure" = "Statistics",
    "value" = "Value",
    "sim" = "Simulation"
  )
}


#' @describeIn f_var_helpers Default dictionary for module pages
f_app_text_dic_default <- function() {
  tribble(
    ~id            , ~title                       ,
    "m_pov_ineq"   , "Poverty and inequality",
    "m_pov"        , "Poverty",
    "title_pl"     , "Poverty lines"              ,
    "title_compare", "Compare by",
    "save_btn"     , "Save results"               ,
    "title_plot_inccon"  , "Income"     ,
    "m_ineq"       , "Inequality"        ,
    "m_povineq"    , "Poverty and inequality"     ,
    "m_growtheq"   , "Growth and equity"          ,
    "m_taxben"     , "Tax and benefit impact"     ,
    "m_simcompare" , "Compare simulations"
  )
}

#' @describeIn f_var_helpers Get page titles dictionary
#' @returns a data frame with page titles
#' @export
f_get_app_text_dic <- function() {
  pages_dic_local <- f_app_text_dic_default()

  if (exists("f_app_text_dic", mode = "function")) {
    pages_dic_local <- f_app_text_dic()

    # Combine local and custom dictionaries, prioritizing custom entries
    pages_dic_local <- pages_dic_local |> rows_update(pages_dic_local, by = "id")
  } 

  pages_dic_local
}

#' @describeIn f_var_helpers Get titles from a dictionary
#' @param id Identifier to get the title for
#' @param dic is a data frame with \code{id} and \code{title} columns
#' @returns The title corresponding to the given \code{id} or provided ID if not found
#' @export
f_get_app_text <- function(id, dic = f_get_app_text_dic(), ...) {
  title <- dic %>% filter(id == !!id) |> slice(1) |> pull(title)
  if (length(title) == 0) {
    title <- id
  }
  title
}