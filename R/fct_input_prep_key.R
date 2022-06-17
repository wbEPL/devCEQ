#' prep_key_inp 
#'
#' @description Prepares inputs to a specific key inputs structure.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_prep_key_inp <- function(dta) {
  
  dta_list <- 
    dta %>%
    group_by(policy_choice) %>%
    tidyr::nest()
  
  current_values <- 
    dta_list %>%
    purrr::pmap( ~ {
      dts <- rlang::dots_list(...)
      fct_compare_key_inp(dts$data, dts$policy_choice)
    }) %>%
    unlist(recursive = F) 
  
  base_values <- 
    dta_list %>% 
    ungroup() %>% 
    slice(1) %>% 
    pull(data) %>% 
    `[[`(1) %>% 
    fct_base_key_inp("policy0")
  
  current_values %>% 
    prepend(base_values) 
  
}

#' Compare key inputs
#' 
#' @importFrom dplyr near filter mutate select pull
#' @noRd
fct_compare_key_inp <- function(data, policy_choice) {
  # browser()
  for_list <-
    data %>%
    dplyr::filter(type != "textInput") %>%
    dplyr::select(id, current_value, base_value, factor) %>% 
    dplyr::mutate(current_value = as.numeric(current_value)) %>%
    mutate(across(c(current_value, base_value), ~ (.) * factor)) %>% 
    select(-factor)
  policy_choices = setNames(for_list$current_value, for_list$id)
  policy_base_choices = setNames(for_list$base_value , for_list$id)
  comparison_results <-
    list(
      policy_name =
        data %>%
        dplyr::filter(type == "textInput") %>%
        dplyr::pull(current_value) %>%
        unlist() %>%
        as.character(),
      # policy_name_same = FALSE,
      policy_choices = setNames(for_list$current_value, for_list$id),
      policy_as_base = all(dplyr::near(policy_choices, policy_base_choices)),
      # policy_choices_as_before = FALSE,
      # policy_choices_new = TRUE,
      timestamp = Sys.time()
    ) %>%
    list() %>%
    set_names(policy_choice)
  
  comparison_results
}


fct_base_key_inp <- function(data, policy_choice = "policy0") {
  
  for_list <-
    data %>%
    filter(type != "textInput") %>%
    dplyr::select(id, current_value, base_value, factor) %>% 
    dplyr::mutate(current_value = as.numeric(current_value)) %>%
    mutate(across(c(current_value, base_value), ~ (.) * factor)) %>%
    mutate(current_value = base_value) %>% 
    select(-factor)
  
  policy_choices = setNames(for_list$current_value, for_list$id)
  policy_base_choices = setNames(for_list$base_value , for_list$id)
  
  comparison_results <-
    list(
      policy_name = "Baseline",
      policy_choices = policy_choices,
      policy_as_base =  TRUE,
      timestamp = Sys.time()
    ) %>%
    list() %>%
    set_names(policy_choice)
  
  comparison_results
}
