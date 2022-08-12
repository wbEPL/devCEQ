
#' Test input UI functions
#'
#' @noRd
#' @import shiny
#' @importFrom rlang dots_list
#' @importFrom dplyr select mutate distinct left_join summarise arrange filter
#' @importFrom purrr map_dfr pmap
#' @importFrom stringr str_c
#' @export
#'
test_genui_fn <- function(inp_raw_str,
                          gen_inp_str = gen_inp_str,
                          gen_ui_fn = gen_tabinp_ui,
                          full = FALSE,
                          n_choice = reactive(2)) {

  server <- function(input, output, session) {
    inp <-
      mod_build_inp_srv(
        NULL,
        inp_raw_str = inp_raw_str,
        inp_str_fn = gen_inp_str,
        ui_gen_fn = gen_ui_fn,
        n_choices = n_choice,
        n_max_choices = reactive(n_choice() + 1),
        reseter = reactive(NULL)
      )
    mod_render_inp_ui_srv(NULL, inp)
  }

  if (full) {
    server <- function(input, output, session) {
      mod_dyn_inp_srv(
        NULL,
        inp_raw_str = inp_raw_str,
        n_choices = n_choice,
        n_max_choices = reactive(n_choice() + 1),
        upd_inp = reactive(NULL),
        reseter = reactive(NULL),
        inp_str_fn = gen_inp_str,
        ui_gen_fn = gen_ui_fn
      )
    }
  }

  fluidPage(column(2, wellPanel(mod_inp_switches_ui(NULL))),
            column(10, mod_dyn_inp_ui(NULL))) %>%
    shinyApp(., server)
}

#' Generates a function based on `gen_tabinp_ui` for generating tabs UI
#'
#' @param inp_tab_str is a tibble with the structure of tabs in the app.
#' Requires two columns: `tab_name` and `group_order`. The first one indicates
#' the name of the tab. The second one indicates the order of a well-group for
#' for including under this tab.
#' @export
gen_tabinp_ui_front <- function(inp_tab_str = NULL, inp_table_str = NULL,...) {
  function(inp_ui_str, ns, type = "fixed", add_rest_btn = TRUE, ... ) {
    gen_tabinp_ui(inp_ui_str,
                  inp_tab_str = inp_tab_str,
                  inp_table_str = inp_table_str,
                  ns = NS(NULL),
                  type = type,
                  add_rest_btn = T,
                  ...)
  }
}

#' Generate IU of the inputs
#'
#' @noRd
#' @import shiny
#' @importFrom rlang dots_list
#' @importFrom dplyr select mutate distinct left_join summarise arrange filter
#' @importFrom purrr map_dfr pmap
#' @importFrom stringr str_c
#' @export
gen_tabinp_ui <-
  function(inp_ui_str,
           inp_tab_str = NULL,
           inp_table_str = NULL,
           ns = NS(NULL),
           type = "fixed",
           add_rest_btn = T,
           ...) {

    # Defining colours for policy columns
    input_cols_spec <-
      inp_ui_str %>%
      dplyr::distinct(policy_choice) %>%
      dplyr::mutate(
        width = floor(12 / nrow(.)),
        style = ifelse(row_number() %% 2 == 0, "background-color:#F8F8F8;", NA_real_)
      )

    # Adjusting columns width
    if (sum(input_cols_spec$width) < 12) {
      input_cols_spec <-
        input_cols_spec %>%
        mutate(width = ifelse(
          row_number() == min(row_number()),
          width + 12 - sum(input_cols_spec$width),
          width
        ))
    }

    # Defining functions for rows
    rowwing_fn <- switch (type,
                          "fixed" = shiny::fixedRow,
                          "fluid" = shiny::fluidRow)

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    ## Assigning inputs by table
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    if (isTruthy(inp_table_str) && length(inp_table_str) > 0) {


      # all_tbls <- bind_rows(inp_table_str)
      #
      # inp_ui_str_new <-
      #   # Add Table ID to each input in the main structure.
      #   inp_ui_str %>%
      #   left_join(all_tbls, c("id" = "col_content"))



      # Regenerate inputs whith table id without lables.



      # inp_tab_str_ordered <-
      #   inp_tab_str %>%
      #   left_join(distinct(., tab_name) %>%
      #               mutate(tab_order = row_number(),
      #                      tab_id = paste0("panel", tab_order)
      #               ),
      #             by = "tab_name")
    } else {

      # inp_tab_str_ordered <- tibble(
      #   tab_name = "Policy choices",
      #   tab_order = 1,
      #   tab_id = "panel1",
      #   group_order = inp_groups$group_order
      # )
    }


    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # Panel structure with rows in columns
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    panel <-
      inp_ui_str %>%
      dplyr::left_join(input_cols_spec, by = "policy_choice") %>%
      dplyr::arrange(group_order, order, row) %>%
      dplyr::group_by(group_order, group_name, policy_choice, style, width) %>%
      dplyr::summarise(single_col = single_ui %>% shiny::tagList(.),
                       .groups = "keep") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        single_col =
          purrr::pmap(., ~ {
            dts <- rlang::dots_list(...)
            column(dts$width, style = dts$style,
                   shiny::verticalLayout(dts$single_col, fluid = T))
      })) %>%
      dplyr::group_by(group_order, group_name) %>%
      dplyr::summarise(single_well = shiny::tagList(single_col),
                       .groups = "keep")  %>%
      dplyr::mutate(single_well = purrr::map(single_well, ~ rowwing_fn(.x)))  %>%
      ungroup() %>%
      arrange(group_order)
    # test_ui(panel$single_well)

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # Tabs header
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    head <-
      panel %>%
      filter(is.na(group_name)) %>%
      pull(single_well) %>%
      shiny::column(sum(input_cols_spec$width), .) %>%
      rowwing_fn()
    # test_ui(head)

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # Reset buttons for each policy scenario
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    if (add_rest_btn) {
      reset_btns <- gen_policy_reset(
        inp_ui_str = inp_ui_str,
        input_cols_spec = input_cols_spec,
        ns = ns) %>%
        dplyr::mutate(single_well = purrr::map(single_well, ~ rowwing_fn(.x)))
    } else {
      reset_btns  <- NULL
    }
    # test_ui(fluidRow(reset_btns$single_well))

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # Compiling header
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    intab_header <- tagList(head, reset_btns$single_well, tags$hr())

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    # Reset buttons for each policy scenario
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    inp_groups <-
      panel %>%
      filter(!is.na(group_name)) %>%
      mutate(
        single_well =
          list(group_name, single_well ) %>%
          purrr::pmap( ~ {
            shiny::h3(..1) %>%
              shiny::column(sum(input_cols_spec$width), .) %>%
              rowwing_fn() %>%
              shiny::tagList(..2)
          })
      )
    # test_ui(inp_groups$single_well)

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    ## Converting panels with UI into tabs.
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    if (isTruthy(inp_tab_str) && nrow(inp_tab_str) > 0) {
      inp_tab_str_ordered <-
        inp_tab_str %>%
        left_join(distinct(., tab_name) %>%
                    mutate(tab_order = row_number(),
                           tab_id = paste0("panel", tab_order)
                           ),
                  by = "tab_name")
    } else {
      inp_tab_str_ordered <- tibble(
        tab_name = "Policy choices",
        tab_order = 1,
        tab_id = "panel1",
        group_order = inp_groups$group_order
      )
    }

    out_wells_tabs <-
      inp_groups %>%
      left_join(inp_tab_str_ordered, "group_order") %>%
      tidyr::replace_na(list(tab_name = "Other Policy Choices",
                             tab_order = 9999,
                             tab_id = "panel9999")) %>%
      arrange(tab_order, group_order) %>%
      group_by(tab_order, tab_name, tab_id) %>%
      summarise(single_well = tagList(single_well)) %>%
      ungroup()

    out_tabs <-
      out_wells_tabs %>%
      mutate(#
        tab_ui =
          purrr::pmap(., ~ {
            dts <- rlang::dots_list(...)
            dts$single_well %>%
              shiny::column(., width = sum(input_cols_spec$width)) %>%
              rowwing_fn(style = "max-height: calc(60vh); overflow-y: scroll;" ,
                         id = ns("policy-options-inputs")) %>%
              shiny::tabPanelBody(value = paste0("panel", dts[[2]]), .)
          }))


    # test_ui(out_tabs$single_well[[1]])
    # browser()

    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    ## Converting panels with UI into tabs.
    ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
    switches <- list()

    switches$choices <-
      set_names(out_wells_tabs$tab_id, out_wells_tabs$tab_name) %>%
      c(., "Summary Table" = "summary")

    switches$ui <-
      shinyWidgets::radioGroupButtons(
        inputId = ns("selected_input_tab"),
        label = NULL,
        choices =   switches$choices,
        direction = "vertical",
        justified = TRUE
      )


    list(
      tabs = out_tabs,
      switches = list(choices = switches$choices, ui = switches$ui),
      header = list(ui = intab_header)
      )
}


#' Generate rest buttons for each policy scenario
#'
#' @noRd
#'
#' @export
gen_policy_reset <- function(inp_ui_str,
                             input_cols_spec = tibble(),
                             ns = NS(NULL)) {
  inp_ui_str %>%
    dplyr::distinct(policy_choice) %>%
    mutate(single_ui = map(policy_choice, ~ {
      actionButton(ns(str_c("reset_", .x)), label = "Reset to baseline")
    })) %>%
    dplyr::left_join(input_cols_spec, by = "policy_choice") %>%
    dplyr::group_by(policy_choice, style, width) %>%
    dplyr::summarise(single_col = single_ui %>% shiny::tagList(.),
                     .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(single_col = purrr::pmap(., ~ {
      dts <- rlang::dots_list(...)
      column(dts$width,
             style = dts$style,
             shiny::verticalLayout(dts$single_col, fluid = T))
    })) %>%
    dplyr::summarise(single_well = shiny::tagList(single_col),
                     .groups = "keep")  %>%
    ungroup()
}



#' @noRd
#'
#' @export
#' @importFrom readxl read_excel excel_sheets
load_inputtabs_xlsx <- function(path) {
  all_sheets <- readxl::excel_sheets(path)
  if ("tabs" %in% all_sheets) {
    readxl::read_excel(path, sheet = "tabs") %>%
      tidyr::fill(tidyselect::contains("group"),
                  tidyselect::contains("tab")) %>%
      dplyr::select(tidyselect::contains("tab"),
                    tidyselect::contains("group_order"))

  } else {
    return(NULL)
  }
}

#' @noRd
#'
#' @export
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tidyr pivot_longer pivot_longer
#' @importFrom tidyr pivot_longer pivot_longer
load_inputtables_xlsx <- function(path) {
  readxl::excel_sheets(path) %>%
    `[`(str_detect(., "table_")) %>%
    imap(~{
      dta <-
        suppressMessages(readxl::read_excel(path, sheet = .x, col_names = T)) %>%
        dplyr::select(tidyselect::contains("___")) %>%
        dplyr::select_if(function(x) !all(is.na(x)))

      dta %>%
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_to = "col_name",
          values_to = "col_content",
          values_transform = as.character
        ) %>%
        tidyr::separate(col_name, "___", into = c("col_name", "col_width") ) %>%
        dplyr::left_join(
          tibble(col_name = names(dta)) %>%
            tidyr::separate(col_name, "___", into = c("col_name", "col_width") ) %>%
            dplyr::mutate(col_order = row_number()),
          by = c("col_name", "col_width")
        ) %>%
        mutate(table_id = .y)
    })

}

