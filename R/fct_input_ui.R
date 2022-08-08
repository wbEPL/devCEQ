
#' @noRd
#'
#' @export
#' @importFrom readxl read_excel
load_input_xlsx <- function(path) {
  readxl::read_excel(path, sheet = 1) %>%
    tidyr::fill(contains("group")) %>%
    dplyr::select(group_name, group_order,
                  tidyselect::any_of("include"),
                  tidyselect::contains("para__"), factor ,
                  tidyselect::contains("tooltip__")) %>%
    dplyr::rename_with(
      .fn = ~ stringr::str_replace_all(., "para__", ""),
      .cols = tidyselect::contains("para__")
    ) %>%
    mutate(
      id = inputId,
      base_value = value,
      row = row_number(),
      label = ifelse(is.na(label), id, label),
      across(tidyselect::any_of(c("value", "min", "max", "step", "factor")),
             ~ suppressWarnings(as.numeric(.))),
      `max` = ifelse(`max` < value, NA_real_, `max`),
      `min` = ifelse(`min`> value, NA_real_, `min`)
    ) #%>%
    # group_by(group_name, id, type, factor, base_value)# %>%
    # nest() %>%
    # rename(para = data) %>%
    # ungroup()
}




#' Test if RAW inputs structure generate a set of UIs
#'
#' @noRd
#'
#' @export
#' @importFrom readxl read_excel
#' @importFrom purrr transpose
inp_str_test <-
  function(inp_raw_str,
           fn_inp_str = gen_inp_str,
           fn_inp_ui = gen_inp_ui,
           ns = NS(NULL)) {
    all_structures <-
      c(1:3) %>%
      map( ~ {
        do.call(fn_inp_str,
                list(inp_raw_str = inp_raw_str,
                     n_choices = .x,
                     ns = ns)
                ) %>%
          list() %>%
          set_names(.x)
      }) %>%
      unlist(recursive = F)

    all_uis <-
      all_structures %>%
      imap( ~ {
        do.call(fn_inp_ui, list(.x, add_rest_btn = TRUE, ns = ns))
      })

    list(all_structures = all_structures,
         all_uis = all_uis) %>%
      purrr::transpose()
  }



#' Generate numeric input from the list of arguments
#'
#' @noRd
#' @importFrom rlang dots_list
#' @importFrom magrittr extract
#' @importFrom shiny numericInput
#' @importFrom bsplus shinyInput_label_embed shiny_iconlink bs_embed_popover
#' @importFrom tippy tippy
#' @export
gen_num_inpt_ui <- function(...) {
  inputs <-
    rlang::dots_list(...)%>%
    unlist(recursive = T) %>%
    as.list()
  out_ui <-
    inputs  %>%
    magrittr::extract(names(.) %in%
                        c("inputId", "label", "value", "min", "max", "step", "width")) %>%
    magrittr::extract(!is.na(.)) %>%
    do.call(what = shiny::numericInput, args = .)
  # if (!is.na(inputs$tooltip__body)) {
  #   out_ui <-
  #     out_ui %>%
  #     bsplus::shinyInput_label_embed(
  #       tippy::tippy(
  #         '<i class="fa fa-info-circle"></i>',
  #         tooltip = shiny::markdown(inputs$tooltip__body),
  #         placement = "left",
  #         theme = "light-border",
  #         arrow = "round",
  #         animation = "shift-away",
  #         interactive = TRUE,
  #         allowHTML = T
  #       )
  #     )
  # }
  out_ui %>%
    div(class = inputs$class)
}

#' Generate text input from the list of arguments
#'
#' @noRd
#' @importFrom rlang dots_list
#' @importFrom magrittr extract
#' @importFrom shiny textInput
#' @export
gen_text_inpt_ui<- function(..., value = NULL) {
  rlang::dots_list(...) %>%
    unlist(recursive = T) %>%
    as.list() %>%
    magrittr::extract(names(.) %in%
                        c("inputId", "label", "value", "width")) %>%
    magrittr::extract(!is.na(.)) %>%
    do.call(what = shiny::textInput, args = .)
}




#' Generate dataframe of the input structure
#'
#' @noRd
#' @import shiny
#' @importFrom rlang dots_list
#' @importFrom dplyr select mutate
#' @importFrom purrr map_dfr pmap
#' @importFrom stringr str_c
#' @importFrom glue glue
#' @export

gen_inp_str <-
  function(inp_raw_str,
           n_choices,
           ns = NS(NULL)) {
    n_choices <- min(n_choices, 12)
    # browser()
    inp_raw_str2 <-
      tibble(row = 0, inputId = "name", id = "name",
             label = "Policy choice name:",
             type = "textInput", width = "100%") %>%
      bind_rows(
        inp_raw_str %>%
          mutate(
            tooltip__body =
              str_c(
                ifelse(!is.na(label),        str_c("__", label , "__ <br/>")                  , ""),
                ifelse(!is.na(label),        str_c("*(", group_name , ")* <br/>")             , ""),
                ifelse(!is.na(tooltip__body),str_c(tooltip__body , " <br/><hr/>")             , ""),
                ifelse(!is.na(base_value),   str_c("Baseline value: ", base_value, "; <br/>") , ""),
                ifelse(!is.na(min),          str_c("Minimum value: ", min, "; <br/>") , ""),
                ifelse(!is.na(max),          str_c("Maximum value: ", max, "; <br/>") , "")
              ) #%>%
              # shiny::markdown()
            )
        )

    c(1:n_choices) %>%
      purrr::map_dfr( ~ {
        choice_no <- .x
        .x <- stringr::str_c("policy", .x)
        ns2 <- .x %>% ns() %>% NS()
        ns1 <- .x %>% NS()
        inp_raw_str2 %>%
          dplyr::mutate(
            inputId_local = ns1(inputId),
            inputId = ns2(inputId),
            policy_choice = .x,
            class = ns2(str_c("inp", row_number()))) %>%
          dplyr::mutate(
            single_ui = purrr::pmap(., ~ {
              dts <- rlang::dots_list(...)
              if ("numericInput" %in% dts$type) {
                gen_num_inpt_ui(dts)
              } else if ("pseudoNumericInput" %in% dts$type) {
                gen_text_inpt_ui(dts)
              } else if("textInput"  %in% dts$type) {
                dts$value <- str_c("Policy choice ", choice_no)
                gen_text_inpt_ui(dts)
              } else {
                list()
              }
            })
          ) %>%
          dplyr::select(group_name,
                        group_order,
                        order,
                        row,
                        policy_choice,
                        id,
                        inputId,
                        inputId_local,
                        label,
                        min,
                        max,
                        type,
                        base_value,
                        factor,
                        single_ui) %>%
          arrange(group_order, order)
      })
  }

#
# # V1 on 2 copies no tippy 137 Mean
# # V1 on 2 copies with tippy 171 Mean
#
# microbenchmark::microbenchmark(
#   inp_str_test_dta %>%
#     gen_inp_str(2) %>%
#     gen_inp_ui(type = "fluid")
# )


#' Generate IU of the inputs
#'
#' @noRd
#' @import shiny
#' @importFrom rlang dots_list
#' @importFrom dplyr select mutate distinct left_join summarise arrange filter
#' @importFrom purrr map_dfr pmap
#' @importFrom stringr str_c
#' @export
gen_inp_ui <- function(inp_ui_str, type = "fixed",
                       add_rest_btn = T,
                       ns = NS(NULL)) {

  input_cols_spec <-
    inp_ui_str %>%
    dplyr::distinct(policy_choice) %>%
    dplyr::mutate(
      width = floor(12 / nrow(.)),
      style = ifelse(row_number() %% 2 == 0, "background-color:#F8F8F8;", NA_real_)
    )

  if (sum(input_cols_spec$width) < 12 ) {
    input_cols_spec <-
      input_cols_spec %>%
      mutate(width = ifelse(
        row_number() == min(row_number()),
        width + 12 - sum(input_cols_spec$width),
        width
      ))
  }

  rowwing_fn <- switch (type,
                       "fixed" = shiny::fixedRow,
                       "fluid" = shiny::fluidRow
  )

  out <- NULL

  # browser()
  # Panel structure with columns in rows.
  # out$panel <-
  #   inp_ui_str %>%
  #   # dplyr::filter(row > 0) %>%
  #   dplyr::left_join(input_cols_spec, by = "policy_choice") %>%
  #   dplyr::mutate(single_ui = purrr::pmap(., ~ {
  #     dts <- rlang::dots_list(...)
  #     shiny::column(dts$width, style = dts$style, dts$single_ui)
  #   })) %>%
  #   dplyr::group_by(group_name, row) %>%
  #   dplyr::summarise(single_row = single_ui %>% shiny::tagList(.), .groups = "keep") %>%
  #   dplyr::mutate(single_row = purrr::map(single_row, ~ rowwing_fn(.x))) %>%
  #   dplyr::group_by(group_name) %>%
  #   dplyr::arrange(row) %>%
  #   dplyr::summarise(single_well = shiny::tagList(single_row), .groups = "keep")

  # Panel structure with rows in columns
  # browser()
  out$panel <-
    inp_ui_str %>%
    dplyr::left_join(input_cols_spec, by = "policy_choice") %>%
    dplyr::arrange(group_order, order, row) %>%
    dplyr::group_by(group_order, group_name, policy_choice, style, width ) %>%
    dplyr::summarise(single_col = single_ui %>% shiny::tagList(.), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(single_col = purrr::pmap(., ~ {
      dts <- rlang::dots_list(...)
      column(dts$width, style = dts$style, shiny::verticalLayout(dts$single_col, fluid = T))
    })) %>%
    dplyr::group_by(group_order, group_name) %>%
    dplyr::summarise(single_well = shiny::tagList(single_col), .groups = "keep")  %>%
    dplyr::mutate(single_well = purrr::map(single_well, ~rowwing_fn(.x)))  %>%
    ungroup() %>%
    arrange(group_order)

  out$head <-
    out$panel %>%
    filter(is.na(group_name)) %>%
    pull(single_well) %>%
    shiny::column(sum(input_cols_spec$width), .) %>%
    rowwing_fn()

  if (add_rest_btn) {
    out$reset_btns <-
      inp_ui_str %>%
      dplyr::distinct(policy_choice) %>%
      mutate(single_ui = map(policy_choice, ~ {
        actionButton(ns(str_c("reset_", .x)), label = "Reset to baseline")
      })) %>%
      dplyr::left_join(input_cols_spec, by = "policy_choice") %>%
      dplyr::group_by( policy_choice, style, width ) %>%
      dplyr::summarise(single_col = single_ui %>% shiny::tagList(.), .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(single_col = purrr::pmap(., ~ {
        dts <- rlang::dots_list(...)
        column(dts$width, style = dts$style, shiny::verticalLayout(dts$single_col, fluid = T))
      })) %>%
      dplyr::summarise(single_well = shiny::tagList(single_col), .groups = "keep")  %>%
      dplyr::mutate(single_well = purrr::map(single_well, ~rowwing_fn(.x)))  %>%
      ungroup()
  } else {
    out$reset_btns  <- NULL
  }

  out$panel <-
    out$panel %>%
    filter(!is.na(group_name)) %>%
    purrr::pmap( ~ {
      dts <- rlang::dots_list(...)
      dts$group_name %>%
        shiny::h3() %>%
        shiny::column(sum(input_cols_spec$width), .) %>%
        rowwing_fn() %>%
        shiny::tagList(dts$single_well)
    }) %>%
    shiny::tagList()

  out$ui <-
    tagList(out$head, out$reset_btns, tags$hr()) %>%
    shiny::column(sum(input_cols_spec$width), .) %>%
    rowwing_fn(style = "overflow-y: auto; padding-right: 19px;") %>%
    tagList(out$panel %>%
              shiny::column(sum(input_cols_spec$width), .) %>%
              rowwing_fn(style = "max-height: calc(70vh); overflow-y: scroll;" ,
                         id = ns("policy-options-inputs"))
            ) %>%
    shiny::column(sum(input_cols_spec$width), .) %>%
    shiny::tabPanelBody(value = "panel1", .) %>%
    list()

  # out$ui #<-
    # tabsetPanel(
    #   id = ns("policy_tabs"),
    #   type = "hidden",
    #   out$ui,
    #   shiny::tabPanelBody(
    #     value = "summary",
    #     DT::DTOutput(ns("inputs_ui_values"))
    #     )
    # )


  # out$ui <-
  #   list(
  #     # id = "hidden_tabs",
  #     # type = "hidden",
  #     out$ui,
  #     shiny::tabPanelBody(value = "summary",
  #                         DT::DTOutput(ns("inputs_ui_values")))
  #   )

  switches_out <- list()
  switches_out$ui <-
    shinyWidgets::radioGroupButtons(
      inputId = ns("selected_input_tab"),
      label = NULL,
      choices = c("Policy choices" = "panel1",
                  "Summary Table" = "summary"),
      direction = "vertical",
      justified = TRUE
      )

  list(tabs = out, switches = switches_out)
}

