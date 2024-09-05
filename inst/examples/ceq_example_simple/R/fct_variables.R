
#' Key variables names and labels
#'
#' @returns data frame with variables names
#' @importFrom shiny isTruthy
#' @importFrom stringr str_c
#' @importFrom dplyr mutate select filter
#' @export
get_var_nm <- function(vars = NULL, suffix = "_pc") {
  dta <-
    tribble(
      ~factor,  ~ var                , ~var_title,

      # Direct Transfers
      # 1, "dirtransf_total"  , "Transferts directs",

      # In kind
      # 1, "inktransf_total"    , "Transferts en nature",
      # 1, "Sante_inKind"       , "Santé (en nature)",
      # 1, "education_inKind"   , "Éducation (en nature)",

      # Subsidy
      # 1, "subsidy_total"      , "Subventions",


      # Direct taxes
      -1, "dtx_total"       , "Direct taxes total",
      -1, "dtx_inc"         , "Direct taxes on employees income",
      -1, "dtx_con"         , "Direct taxes on employers income",

      # Excises

      # Indirect taxes
      -1, "itx_total"         , "Indirect taxes, total",

      # Contributions for health and pension
      -1, "pen", "Pension contribution",

      # Income concepts
      1,        "ym"             ,  "Market income",
      1,        "yd"             ,  "Disposable income" ,
      1,        "yc"             ,  "Consumable income" ,
      1,        "yf"             ,  "Final income" ,

      # Auxiliary variables
      1,        "hhwt"             ,  "Weight: households" ,
      1,        "hhid"             ,  "HH id" ,
      1,        "hhsize"           ,  "HH size"
    ) %>%
    distinct() %>%
    mutate(
      var_title = factor(var_title, levels = .$var_title, labels = .$var_title)
    ) %>%
    select(var, var_title, factor)

  if (!is.null(suffix)) {
    dta <- dta %>% mutate(var = str_c(var, suffix))
  }

  if (shiny::isTruthy(vars)) {
    dta <-
      dta %>%
      filter(var %in% vars) %>%
      mutate(var_title = factor(var_title, levels = var_title, labels = var_title))
  }

  return(dta)
}




#' @describeIn get_var_nm Income Concepts variables IDs and labels returned in a data frame
#'
#' @returns a data frame with variables names
#'
#' @export
get_inc_nm <- function(suffix = "_pc") {
  c("ymp", "yn", "yd", "yc", "yf") %>%
    str_c(., suffix) %>%
    get_var_nm(suffix = suffix)
}

#' @describeIn get_var_nm Weight variable IDs and labels in a data frame
#'
#' @returns a data frame with variables names
#'
#' @export
get_wt_nm <- function(suffix = NULL) {
  "hhwt" %>%
    str_c(., suffix) %>%
    get_var_nm(suffix = suffix) %>%
    pull(var)
}
