#' Loads the input structure data into R  from an Excel file.
#' @param path path to a pre-defined Excel file#'
#' @returns a data frame with the detailed input structure.
#' @export
#' @importFrom readxl read_excel
#' @importFrom tidyselect any_of contains
#' @importFrom dplyr select rename_with mutate across
#' @examples
#' \dontrun{
#' system.file(
#'   "examples",
#'   "ceq_example_simple",
#'   "data-app",
#'   "simple-inputs-structure.xlsx",
#'   package = "devCEQ"
#' ) |> load_input_xlsx()
#' }
#'
#'
load_input_xlsx <- function(path) {
  readxl::read_excel(path, sheet = 1) %>%
    tidyr::fill(tidyselect::contains("group")) %>%
    dplyr::select(group_name, group_order,
                  tidyselect::any_of("include"),
                  tidyselect::contains("para__"), factor ,
                  tidyselect::contains("tooltip__")) %>%
    dplyr::rename_with(
      .fn = ~ stringr::str_replace_all(., "para__", ""),
      .cols = tidyselect::contains("para__")
    ) %>%
    dplyr::mutate(
      id = inputId,
      base_value = value,
      row = row_number(),
      label = ifelse(is.na(label), id, label),
      dplyr::across(tidyselect::any_of(c("value", "min", "max", "step", "factor")),
             ~ suppressWarnings(as.numeric(.))),
      `max` = ifelse(`max` < value, NA_real_, `max`),
      `min` = ifelse(`min`> value, NA_real_, `min`)
    )
}

#' @describeIn load_input_xlsx Test if RAW inputs structure generates a set of UI
#' elements and returns a very long list with HTML tags and elements used for
#' building the UI. If it fails, it indicates that the app will likely fail
#' as well.
#' @param inp_raw_str inputs structure as returned by `load_input_xlsx()`
#' @param fn_inp_str,fn_inp_ui,ns internal supporting functions.
#' @export
#' @importFrom readxl read_excel
#' @importFrom purrr transpose
#' @examples
#' \dontrun{
# system.file(
#   "examples",
#   "ceq_example_simple",
#   "data-app",
#   "simple-inputs-structure.xlsx",
#   package = "devCEQ"
# ) |>
#   load_input_xlsx() |>
#   inp_str_test()
#' }
#'
inp_str_test <-
  function(inp_raw_str,
           fn_inp_str = gen_inp_str,
           fn_inp_ui = gen_tabinp_ui,
           ns = NS(NULL),
           ...) {
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


# UI generators -----------------------------------------------------------

#' @describeIn load_input_xlsx Wrapper around `gen_inp_str()` used for
#' providing table structure inside other functions. Returns `gen_inp_str()`
#' function with predefined parameters and data that can be executed for
#' generating input structure.
#'
#' @param inp_table_str table with the input structure
#' @param ... passed to functions within
#' @export
gen_inp_str_front <- function(inp_table_str = NULL, ...) {
  function(inp_raw_str, n_choices, ns = NS(NULL), ... ) {
    gen_inp_str(inp_raw_str = inp_raw_str,
                n_choices = n_choices,
                ns = ns,
                inp_table_str = inp_table_str,
                ...)
  }
}

#' @describeIn load_input_xlsx Generate a data frame of the inputs UI used in creating the inputs page UI
#'
#' @param inp_raw_str inputs structure as returned by `load_input_xlsx()`
#' @param n_choices number of policy choices to generate
#' @param inp_table_str structure of tables under which inputs are organized (NULL by default)
#' @param ns name space function `NS(NULL)` by default
#'
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
           inp_table_str = NULL,
           ns = NS(NULL), ...) {
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
              ifelse(
                !is.na(tooltip__body),
                str_c(
                  ifelse(!is.na(label),        str_c("__", label , "__ <br/>")                  , ""),
                  ifelse(!is.na(label),        str_c("*(", group_name , ")* <br/>")             , ""),
                  ifelse(!is.na(tooltip__body),str_c(tooltip__body , " <br/><hr/>")             , ""),
                  ifelse(!is.na(base_value),   str_c("Baseline value: ", base_value, "; <br/>") , ""),
                  ifelse(!is.na(min),          str_c("Minimum value: ", min, "; <br/>") , ""),
                  ifelse(!is.na(max),          str_c("Maximum value: ", max, "; <br/>") , "")
                ),
                NA_character_)
            )
        ) %>%
      mutate(table_id = FALSE)

    if (isTruthy(inp_table_str) && length(inp_table_str) > 0) {
      all_tbls <-
        bind_rows(inp_table_str) %>%
        select(col_content, table_id)
      inp_raw_str2 <-
        # Add Table ID to each input in the main structure.
        inp_raw_str2 %>%
        select(-any_of("table_id")) %>%
        left_join(all_tbls, c("id" = "col_content")) %>%
        mutate(table_id = !is.na(table_id))
    }

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
                gen_num_inpt_ui(dts, nolable = dts$table_id)
              } else if ("pseudoNumericInput" %in% dts$type) {
                gen_text_inpt_ui(dts, nolable = dts$table_id)
              } else if("textInput"  %in% dts$type) {
                dts$value <- str_c("Policy choice ", choice_no)
                gen_text_inpt_ui(dts, nolable = dts$table_id)
              } else if("checkboxInput"  %in% dts$type) {
                dts$value <- as.logical(dts$value)
                gen_checkbox_inpt_ui(dts, nolable = dts$table_id)
              } else if("radioButtons"  %in% dts$type) {
                gen_radiobuttons_inpt_ui(dts, nolable = dts$table_id)
              } else if("md"  %in% dts$type) {
                gen_md_ui(dts, nolable = dts$table_id)
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
                        any_of(c("allowna")),
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



# UI Generators --------------------------------------------------------------

#' Generate numeric input from the list of arguments
#'
#' @noRd
#' @importFrom rlang dots_list
#' @importFrom magrittr extract
#' @importFrom shiny numericInput
#' @importFrom bsplus shinyInput_label_embed shiny_iconlink bs_embed_popover
#' @importFrom tippy tippy
gen_num_inpt_ui <- function(..., nolable = FALSE) {
  inputs <-
    rlang::dots_list(...)%>%
    unlist(recursive = T) %>%
    as.list()

  if (nolable) {
    inputs$label <- NULL
    add_arg <- list(label = NULL)
  } else {
    add_arg <- list()
  }

  if (! "value" %in% names(inputs) || is.null(inputs$value)|| is.na(inputs$value)) {
    add_arg <- add_arg %>% append(list(value = NULL))
  }

  out_ui <-
    inputs  %>%
    magrittr::extract(names(.) %in%
                        c("inputId", "label", "value", "min", "max", "step", "width")) %>%
    magrittr::extract(!is.na(.)) %>%
    append(., add_arg) %>%
    do.call(what = shiny::numericInput, args = .)

  if (!is.na(inputs$tooltip__body)) {
    out_ui <-
      out_ui %>%
      bsplus::shinyInput_label_embed(
        tippy::tippy(
          '<i class="fa fa-info-circle"></i>',
          tooltip = shiny::markdown(inputs$tooltip__body),
          placement = "left",
          theme = "light-border",
          arrow = "round",
          animation = "shift-away",
          interactive = TRUE,
          allowHTML = T
        )
      )
  }
  out_ui %>%
    div(class = inputs$class)
}

#' Generate text input from the list of arguments
#'
#' @noRd
#' @importFrom rlang dots_list
#' @importFrom magrittr extract
#' @importFrom shiny textInput
gen_text_inpt_ui<- function(..., value = NULL, nolable = FALSE) {
  rlang::dots_list(...) %>%
    unlist(recursive = T) %>%
    as.list() %>%
    magrittr::extract(names(.) %in%
                        c("inputId", "label", "value", "width")) %>%
    magrittr::extract(!is.na(.)) %>%
    do.call(what = shiny::textInput, args = .)
}


#' Generate checkbox input from the list of arguments
#'
#' @noRd
#' @importFrom rlang dots_list
#' @importFrom magrittr extract
#' @importFrom shiny checkboxInput
gen_checkbox_inpt_ui<- function(..., nolable = FALSE) {
  inputs <-
    rlang::dots_list(...) %>%
    unlist(recursive = T) %>%
    as.list()

  if (nolable) {
    inputs$label <- NULL
    add_arg <- list(label = NULL)
  } else {
    add_arg <- list()
  }
  inputs <-
    inputs %>%
    magrittr::extract(names(.) %in% c("inputId", "label", "value")) %>%
    magrittr::extract(!is.na(.)) %>%
    append(., add_arg) #%>%
  # do.call(what = shiny::checkboxInput, args = .)

  shiny::checkboxInput(inputId = inputs$inputId,
                       value = isTRUE(inputs$value == "1" ||
                                        inputs$value == 1 ||
                                        inputs$value == "TRUE"),
                       label = inputs$label)
}


#' Generate checkbox input from the list of arguments
#'
#' @noRd
#' @importFrom rlang dots_list
#' @importFrom magrittr extract
#' @importFrom shiny radioButtons
gen_radiobuttons_inpt_ui<- function(..., nolable = FALSE) {
  inputs <-
    rlang::dots_list(...) %>%
    unlist(recursive = T) %>%
    as.list()

  # if (nolable) {
  #   inputs$label <- NULL
  #   add_arg <- list(label = NULL)
  # } else {
  #   add_arg <- list()
  # }
  # browser()

  inputs_main <- eval(parse(text = inputs$label))
  # inputs <-
  #   inputs %>%
  #   magrittr::extract(names(.) %in% c("inputId", "label", "value")) %>%
  #   magrittr::extract(!is.na(.)) %>%
  #   append(., add_arg) #%>%
  # do.call(what = shiny::checkboxInput, args = .)

  shiny::radioButtons(inputId = inputs$inputId,
                      choices = inputs_main$choices,
                      label = inputs_main$label,
                      selected = as.numeric(inputs$value),
                      width = inputs$width,
                      inline = if (is.null(inputs_main$inline)) FALSE else inputs_main$inline
                      )
}


#' Generate text input from the list of arguments
#'
#' @noRd
#' @importFrom rlang dots_list
#' @importFrom magrittr extract
#' @importFrom shiny markdown
#' @importFrom stringr str_c
gen_md_ui <- function(..., value = NULL, nolable = FALSE) {
  rlang::dots_list(...) %>%
    unlist(recursive = T) %>%
    as.list() %>%
    magrittr::extract(names(.) %in% c("label", "value")) %>%
    magrittr::extract(!is.na(.)) %>%
    str_c(., collapse = "") %>%
    shiny::markdown(.)
}



# Test functions -----------------------------------------------------------

#' @describeIn load_input_xlsx Test input page content generation in a single tab
#'
#' @param inp_raw_str,inp_tab_str,inp_table_str,n_choices input layout options
#' @export
#' @examples
#' \dontrun{
#' a <- system.file(
#'     "examples",
#'     "ceq_example_simple",
#'     "data-app",
#'     "simple-inputs-structure.xlsx",
#'     package = "devCEQ"
#'   ) |> load_input_xlsx()
#' a |> test_gen_inp_front_simple()
#' }
#'
test_gen_inp_front_simple <-
  function(inp_raw_str,
           inp_tab_str = NULL,
           inp_table_str = NULL,
           n_choices = 2) {
    local_inp_str_fn <- gen_inp_str_front(inp_table_str = inp_table_str)

    # inp_ui_str <- local_inp_str_fn(inp_raw_str, n_choices = 2, ns = NS(NULL))
    #
    local_tab_ui_fn <- gen_tabinp_ui_front(inp_tab_str, inp_table_str)

    all_outs <-
      local_inp_str_fn(inp_raw_str, n_choices = n_choices, ns = NS(NULL)) %>%
      local_tab_ui_fn()

    # gen_one_inp_table(inp_ui_str, inp_table_str[[2]])

    ui <- fluidPage(all_outs$tabs %>% slice(1) %>% pull(tab_ui))

    server <- function(input, output, session) {

    }


    a <- shinyApp(ui, server)
    runApp(a, launch.browser = T)

  }

#' @describeIn load_input_xlsx Test input UI generation process by providing
#' path to the inputs structure Excel file. It Launches a shiny app with
#' all inputs properly rendered in a single input tab.
#' @export
#' @examples
#' \dontrun{
# a <- system.file(
#     "examples",
#     "ceq_example_simple",
#     "data-app",
#     "simple-inputs-structure.xlsx",
#     package = "devCEQ"
#   ) |> load_input_xlsx()
# a |> test_gen_inp_front_tabs()
#' }
test_gen_inp_front_tabs <-
  function(inp_raw_str,
           inp_tab_str = NULL,
           inp_table_str = NULL,
           n_choices = 2) {
    local_inp_str_fn <- gen_inp_str_front(inp_table_str = inp_table_str)

    # inp_ui_str <- local_inp_str_fn(inp_raw_str, n_choices = 2, ns = NS(NULL))
    #
    local_tab_ui_fn <- gen_tabinp_ui_front(inp_tab_str, inp_table_str)

    all_outs <-
      local_inp_str_fn(inp_raw_str, n_choices = n_choices, ns = NS(NULL)) %>%
      local_tab_ui_fn()

    # gen_one_inp_table(inp_ui_str, inp_table_str[[2]])

    test_mod_inp_tabs_simple(id = NULL, switches = all_outs$switches, tabs = all_outs$tabs)

  }


#' @describeIn load_input_xlsx Does the same as `test_gen_inp_front_tabs()`, but
#' on top of input generation also creates tabs for switching between groups of
#' inputs.
#' @examples
#' \dontrun{
# system.file(
#     "examples",
#     "ceq_example_simple",
#     "data-app",
#     "simple-inputs-structure.xlsx",
#     package = "devCEQ"
#   ) |>
#   test_gen_inp_front_tabs_file()
#' }
test_gen_inp_front_tabs_file <- function(path) {
  inp_raw_str <- path %>% load_input_xlsx()
  inp_tab_str <- path %>% load_inputtabs_xlsx()
  inp_table_str <- path %>% load_inputtables_xlsx()
  test_gen_inp_front_tabs(inp_raw_str, inp_tab_str, inp_table_str)
}
