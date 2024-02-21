
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
      1, "dirtransf_total"  , "Transferts directs",
      1, "am_BNSF"          , "BNSF",
      1, "am_Cantine"       , "Cantines Scolaires", 
      1, "am_bourse"        , "Bourse d'Éducation Universitaire", 
      1, "am_subCMU"        , "Assurance CMU", 

      # 1, "am_total"      , "Transferts directs",
      # 1, "am_bourse"     , "Bourse de L'education superieur",
      # 1, "am_Cantine"    , "Programme des Cantines Scolaires",
      # 1, "am_BNSF"       , "Programme National de Bourses de Sécurité Familiale",
      
      #  Programmes CMU
      1, "am_CMU_progs", "Programmes CMU",
      1, "am_sesame", "Plan Sésame",
      1, "am_moin5", "Soins gratuits pour les enfants moins 5 ans",
      1, "am_cesarienne", "Cesarienne gratuite",
      
      # # 1, "am_subCMU"          , "Assurance CMU",
      # 1, "am_total2"          , "Subventions : Santé",
      # 1, "am_sesame"          , "Plan Sésame",
      # 1, "am_moin5"           , "Gratuité pour les moins de 5 ans",
      # 1, "am_cesarienne"      , "Gratuité de la césarienne",
      
      # In kind
      1, "inktransf_total", "Transferts en nature", 
      1, "Sante_inKind", "Santé (en nature)", 
      1, "education_inKind", "Éducation (en nature)", 
      
      
      # # Education
      # 1, "education_inKind"   , "Éducation en nature",
      # 1, "am_pre_school_pub"  , "Préscolaire",
      # 1, "am_primary_pub"     , "Primaire",
      # 1, "am_secondary_pub"   , "Secondaire",
      # 1, "am_tertiary_pub"    , "Tertiaire",
      # 
      # # In kin Health
      # 1, "Sante_inKind"       , "Santé : en nature",
      
      # Subsidy
      # 1, "subsidy_total"      , "Subventions",
      # 1, "subsidy_elec"       , "Subventions : électricité",
      # 1, "subvention_agric"   , "Subventions : agriculture",
      1, "subsidy_total", "Subventions",
      1, "subsidy_elec", "Subv. Électricité",
      1, "subsidy_elec_direct", "Effet Direct Élec.",
      1, "subsidy_elec_indirect", "Effet Indirect Élec.",
      1, "subsidy_fuel", "Subv. Carburants",
      1, "subsidy_fuel_direct", "Effet Direct Carb.",
      1, "subsidy_fuel_indirect", "Effet Indirect Carb.",
      1, "subsidy_eau", "Subv. Eau",
      1, "subsidy_eau_direct", "Effet Direct Eau",
      1, "subsidy_eau_indirect", "Effet Indirect Eau",
      

      # Direct taxes
      -1, "dirtax_total", 	"Impôts directs",
      -1, "income_tax", "Impôt sur le Revenu",
      -1, "income_tax_reduc", 	"Déductions et Quotient Familial",
      -1, "trimf", "TRIMF",

      # -1, "income_tax"        , "Impôt sur le revenu",
      # -1, "inctax_sal_f"      , "Impôt sur le revenu : salarié",
      # -1, "inctax_self"       , "Impôt sur le revenu : travailleur indépendant",
      
      
      # Excises
      -1, "excise_taxes"      , "Taxes d'accise",
      -1, "ex_cof"            , "Café",
      -1, "ex_tea"            , "Thé",
      -1, "ex_fat1"           , "Produits gras 1",
      -1, "ex_fat2"           , "Produits gras 2",
      -1, "ex_alc"            , "Breuvages alcoolisés",
      -1, "ex_nal"            , "Breuvages",
      -1, "ex_tab"            , "Le tabac",
      
      # Indirect taxes 
      # -1, "Tax_TVA"           , "Taxe sur la valeur ajoutée",
      -1, "indtax_total", "Taxes Indirectes",
      -1, "excise_taxes", "Droits d'Accise",
      -1, "Tax_TVA", "TVA",
      -1, "TVA_direct", "Effet Direct TVA",
      -1, "TVA_indirect", "Effet Indirect TVA",

      
      # Contributions for health and pension
      -1, "sscontribs_total", "Cotisations à l'Assurance Sociale", 
      -1, "csh_css", "Risque Maladie et Allocation Familiale",
      -1, "csh_ipm", "Cotisation Santé à IPM", 

      # -1, "cs_total"    , "Cotisations",
      # -1, "csh_css"     , "Cotisations Santé: CSS",
      # -1, "csp_fnr"     , "Cotisations retraites: FNR",
      # -1, "csp_ipr"     , "Cotisations retraites: IPRES",
      
      # 1,        "yl"             ,  "Market income",
      1,        "ymp"             ,  "Revenu du marché plus pensions",
      1,        "yn"             ,  "Revenu net du marché",
      # 1,        "yg"             ,  "Gross income",
      1,        "yd"             ,  "Revenu disponible" ,
      1,        "yc"             ,  "Revenus consommables" ,
      1,        "yf"             ,  "Revenu final" ,
      
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

#' @describeIn get_var_nm Weight variable IDs and labels in a data frame
#' 
#' @returns a data frame with variables names
#' 
#' @export
get_wt_nm <- function(suffix = NULL) {
  "pondih" %>%
    str_c(., suffix) %>%
    get_var_nm(suffix = suffix) %>%
    pull(var)
}
