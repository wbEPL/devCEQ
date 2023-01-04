
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
      # browser()
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
  policy_as_base <-
    map2_lgl(policy_choices, policy_base_choices, ~ {
      (is.na(.x) && is.na(.y)) || dplyr::near(.x, .y)
    }) %>% all(na.rm = TRUE)
  # policy_as_base <-
  #   map2(policy_choices, policy_base_choices, ~ {
  #
  #     list(x = .x,
  #          y = .y,
  #          compare = dplyr::near(.x, .y))
  #     # (is.na(.x) && is.na(.y)) ||
  #   })
  # aa <- all(dplyr::near(policy_choices, policy_base_choices), na.rm = T)

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
      policy_as_base = policy_as_base,
      # policy_choices_as_before = FALSE,
      # policy_choices_new = TRUE,
      timestamp = Sys.time()
    ) %>%
    list() %>%
    set_names(policy_choice)

  comparison_results
}


#' Base key inputs
#'
#' @importFrom dplyr near filter mutate select pull
#' @noRd
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


#' Return all inputs from the raw inputs structure
#'
#' @param data dataframe with the raw inputs structure
#'
#' @export
get_all_inps <- function(dta) {
  dta %>%
    mutate(base_value = base_value * factor) %>%
    select(inputId, base_value) %>%
    pmap(~ set_names(list(.y), .x)) %>%
    unlist(recursive = F)
}
