% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_gini_poverty.R
\name{get_dta_gini}
\alias{get_dta_gini}
\title{Calculate Gini index for all market concepts for a single simulation}
\usage{
get_dta_gini(
  dta,
  policy_name = NULL,
  income_vars_tbl = NULL,
  wt_var = "pcweight",
  para_names = tibble(parameter = c("Gini", "Theil"), label = c("Gini index",
    "Theil index")),
  ...
)
}
\arguments{
\item{dta}{data frame with the simulation results.}

\item{income_vars_tbl}{a data frame with two character columns: `var` and
`var_title`. `var` is the column with the variable names returned from
the data set. `var_title` is the variable name as we want it to appear
in the resulting table. See for example results of the functions
`get_inc_nm()` or `get_var_nm()`}

\item{wt_var}{character name of a single variable that should be used
as a weighting variable in calculation. Default is get_wt_nm()$var.
If wt_var is not provided or the one provided is not present in `dta`,
warning will be prompted.}
}
\description{
Calculate Gini index for all market concepts for a single simulation
}
