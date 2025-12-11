#' Key variables names and labels
#'
#' @returns data frame with variables names
#' @importFrom shiny isTruthy
#' @importFrom stringr str_c
#' @importFrom dplyr mutate select filter
#'
#' @export
get_var_nm <- function(vars = NULL, suffix = NULL) {
  
  var_dic_local <- f_var_dic_default()

  if (exists("f_var_dic", mode = "function")) {
    var_dic_local <- f_var_dic()
  }

  dta <-
    var_dic_local |>
    mutate(var_title = var_title |> fct_keep_dup_string() |> as_factor()) |>
    select(var, var_title, factor)

  if (!missing(suffix) && !is.null(suffix)) {
    dta <- dta %>% mutate(var = str_c(var, suffix))
  }

  if (!missing(vars) && !is.null(vars)) {
    dta <-
      dta %>%
      filter(var %in% vars) %>%
      mutate(
        var_title = factor(var_title, levels = var_title, labels = var_title)
      )
  }

  return(dta)
}

#' @describeIn get_var_nm All measures IDs and labels returned in a data frame
#' @returns a data frame with measures names
#' @export
get_measure_nm <- function(x = NULL) {
  measure_nm <- f_measure_dic_default()
  if (exists("f_measure_dic", mode = "function")) {
    measure_nm <- f_measure_dic()
  } 
  if (is.null(x)) {
    return(measure_nm)
  } else {
    # Filter and preserve order of x
    filtered <- measure_nm %>% filter(measure %in% x)
    # Reorder by the order in x
    filtered[match(x, filtered$measure), ] %>% na.omit()
  }
}


#' @describeIn get_var_nm Income Concepts variables IDs and labels returned in a data frame
#'
#' @returns a data frame with variables names
#'
#' @export
get_inc_nm <- function(suffix = NULL) {
  inc_vars <- f_var_inc_default()
  if (exists("f_var_inc", mode = "function")) {
    inc_vars <- f_var_inc()
  } 
  inc_vars |>
    str_c(suffix) %>%
    get_var_nm(suffix = suffix)
}

#' @describeIn get_var_nm Weight variable IDs and labels in a data frame
#'
#' @returns a data frame with variables names
#'
#' @export
get_wt_nm <- function(suffix = NULL) {
  wt_var <- f_var_wt_default()
  if (exists("f_var_wt", mode = "function")) {
    wt_var <- f_var_wt()
  } 

  wt_var %>%
    str_c(., suffix) %>%
    get_var_nm(suffix = suffix) %>%
    pull(var)
}


#' @describeIn get_var_nm Grouping variables IDs and labels in a data frame
#' @returns a data frame with variables names
#' @export
get_group_nm <- function(suffix = NULL) {
  group_vars <- f_var_group_default()
  if (exists("f_var_group", mode = "function")) {
    group_vars <- f_var_group()
  } 
  group_vars |>
    str_c(suffix) %>%
    get_var_nm(suffix = suffix)
}


#' @describeIn get_var_nm Helper to label duplicated variable names as different factors levels.
#' 
#' @export
fct_keep_dup_string <- function(val) {
  zero_width_space <- "\u200b"
  val_chr <- as.character(val)
  n_dup <- ave(seq_along(val_chr), val_chr, FUN = seq_along)
  val_unique <- paste0(val_chr, strrep(zero_width_space, n_dup - 1))
  val_unique
}


#' @describeIn get_var_nm Variables dictionary 
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
      1,       "group_3"         ,  "Age group typology"

    )
}

#' @describeIn get_var_nm Weight variables names 
#'
f_var_wt_default <- function() {
  "hhwt"
}

#' @describeIn get_var_nm Income variables names
#'
f_var_inc_default <- function() {
    c("ym", "yp", "yg", "yd", "yc", "yf")
}


#' @describeIn get_var_nm Income variables names
#'
f_var_group_default <- function() {
    c("group_1", "group_2", "group_3")
}

#' Var nems to vector
#' 
f_var_names_vector <- function(var_nms = get_inc_nm()) {
  set_names(var_nms$var, var_nms$var_title)
}

f_get_measure <- function(x = NULL) {
  if (is.null(x)) {
    return(get_measure_nm()$measure)
  }
  measure_nm <- get_measure_nm()
  measure_nm %>%
    filter(measure == x) %>%
    pull(measure_title)
}

#' @describeIn get_var_nm Poverty and inequality measures names
#' 
f_measure_dic_default <- function() {
  tribble(
    ~measure, ~measure_title,
    "hc",     "Number of poor",
    "fgt0",   "Poverty headcount ratio (FGT0)",
    "fgt1",   "Poverty gap index (FGT1)",
    "fgt2",   "Poverty severity index (FGT2)",
    "gini",   "Gini coefficient",
    "theil",  "Theil index",
    "n",      "N observations",
    "pop",    "Population"
  )
}
