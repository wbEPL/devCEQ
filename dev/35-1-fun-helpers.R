
#' Key variables names and labels
#'
#' @returns data frame with variables names
#' @importFrom shiny isTruthy
#' @export
get_var_nm <- function(vars = NULL, suffix = "_pc") {
  dta <-
    tribble(
      ~factor,  ~ var                , ~var_title,

      # Direct Transfers
      1, "am_total"      , "Transferts directs",
      1, "am_bourse"     , "Bourse de L'education superieur",
      1, "am_Cantine"    , "Programme des Cantines Scolaires",
      1, "am_BNSF"       , "Programme National de Bourses de Sécurité Familiale",

      #  Transferts directs
      1, "am_subCMU"          , "Assurance CMU",
      1, "am_total2"          , "Subventions : Santé",
      1, "am_sesame"          , "Plan Sésame",
      1, "am_moin5"           , "Gratuité pour les moins de 5 ans",
      1, "am_cesarienne"      , "Gratuité de la césarienne",

      # In kin Education
      1, "education_inKind"   , "Éducation en nature",
      1, "am_pre_school_pub"  , "Éducation: préscolaire",
      1, "am_primary_pub"     , "Éducation: primaire",
      1, "am_secondary_pub"   , "Éducation: secondaire",
      1, "am_tertiary_pub"    , "Éducation: tertiaire",

      # In kin Health
      1, "Sante_inKind"       , "Santé : en nature",

      # Subsidy
      1, "subsidy_total"      , "Subventions",
      1, "subsidy_elec"       , "Subventions : électricité",
      1, "subvention_agric"   , "Subventions : agriculture",

      # Direct taxes
      -1, "income_tax"        , "Impôt sur le revenu",
      -1, "inctax_sal_f"      , "Impôt sur le revenu : salarié",
      -1, "inctax_self"       , "Impôt sur le revenu : travailleur indépendant",

      # Excises
      -1, "excise_taxes"      , "Taxes d'accise",
      -1, "ex_cof"            , "Excise on coffee",
      -1, "ex_tea"            , "Excise on Tea",
      -1, "ex_fat1"           , "Excise on Fatty products 1",
      -1, "ex_fat2"           , "Excise on Fatty products 2",
      -1, "ex_alc"            , "Excise on Alcoholic Beverages",
      -1, "ex_nal"            , "Excise on Beverages",
      -1, "ex_tab"            , "Excise on Tobacco",

      # VAT
      -1, "Tax_TVA"           , "Taxe sur la valeur ajoutée",

      # Contributions for health and pension
      -1, "cs_total"    , "Cotisations",
      -1, "csh_css"     , "Cotisations Santé: CSS",
      -1, "csp_fnr"     , "Cotisations retraites: FNR",
      -1, "csp_ipr"     , "Cotisations retraites: IPRES",

      # 1,        "yl"             ,  "Market income",
      1,        "ymp"             ,  "Market income plus pensions",
      1,        "yn"             ,  "Net market income",
      # 1,        "yg"             ,  "Gross income",
      1,        "yd"             ,  "Disposable income" ,
      1,        "yc"             ,  "Consumable income" ,
      1,        "yf"             ,  "Final income" ,

      1,        "hhweight"             ,  "Weight: households" ,
      1,        "pondih"               ,  "Weight: households with hh size" ,
      # 1,        "pcweight"             ,  "Per capita weight"
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

get_wt_nm <- function(suffix = NULL) {
  c("hhweight") %>%
    str_c(., suffix) %>%
    get_var_nm(suffix = suffix) %>%
    pull(var)
  # tibble(var = c("weind"),
  #        var_title = c("Population weight") %>%
  #          factor(., levels = ., labels = .)
  # ) %>%
  #   select(-any_of("factor"))
}
