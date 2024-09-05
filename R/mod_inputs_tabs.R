
# Key input tab UI module ------------------------------------------------



#' UI for dynamic inputs generated / updated by server function
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @export
mod_inp_tabs_content_ui <- function(id) {
  ns <- NS(id)

  tagList(

    # Tabs header
    mod_inp_tab_header_ui(id) %>% div(id = ns("policy_names_holder")),

    # ui Output placeholder
    mod_inp_tabs_ui(id),

    # Ready tabs placeholder
    shiny::tabsetPanel(
      id = ns("policy_tabs"),
      type = "hidden",

      # Additional tabs headers just in case
      header = mod_inp_tab_header_intab_ui(id),

      # Body of the inputs summary page
      shiny::tabPanelBody(
        value = "summary",
        DT::DTOutput(ns("inputs_summary_dt"))
      )
    ) %>%
      div(id = ns("policy_choices_holder")) %>%
      div(id = ns("policy_choices_holder_1"))%>%
      div(id = ns("policy_choices_holder_2")),

    # Place for the footer just in case
    mod_inp_tab_footer_ui(id),

    # Debugging UI options
    if (golem::app_dev()) {
      tagList(
        h4("dev output for mod_inp_tabs_content_ui()"),
        shiny::verbatimTextOutput(ns("dynamic_inputs_ui_info"))
      )
    }
  )
}


#' @describeIn mod_inp_tabs_content_ui UI for input tabs switches.
#'
#' @noRd
#' @importFrom shiny radioButtons
#' @importFrom shinyWidgets radioGroupButtons
#' @export
mod_inp_tab_switches_ui <-
  function(id) {
    ns <- NS(id)
    shinyWidgets::radioGroupButtons(
      inputId = ns("inp_tab"),
      label = NULL,
      choices = c(
        "Policy choices" = "panel1",
        "Summary Table" = "summary"
      ),
      direction = "vertical",
      justified = TRUE
    )
  }

#' @describeIn mod_render_inp_tabs_srv UI for input tabs bodies
#' @noRd
#' @import shiny
#' @importFrom shiny uiOutput
#' @export
mod_inp_tabs_ui <-
  function(id) {
    ns <- NS(id)
    shiny::uiOutput(ns("tabs_ui"))
  }

#' @describeIn mod_render_inp_tabs_srv UI for tabs header
#' @noRd
#' @importFrom shiny uiOutput
#' @export
mod_inp_tab_header_ui <-
  function(id) {
    ns <- NS(id)
    shiny::uiOutput(ns("tab_header_ui"))
  }

#' @describeIn mod_render_inp_tabs_srv UI for tabs mod_inp_tab_header_intab_ui
#' @noRd
#' @importFrom shiny uiOutput
#' @export
mod_inp_tab_header_intab_ui <-
  function(id) {
    ns <- NS(id)
    shiny::uiOutput(ns("tab_header_intab_ui"))
  }


#' @describeIn mod_render_inp_tabs_srv UI for tabs footer
#' @noRd
#' @importFrom shiny uiOutput
#' @export
mod_inp_tab_footer_ui <-
  function(id) {
    ns <- NS(id)
    shiny::uiOutput(ns("tab_footer_ui"))
  }


# Servers -----------------------------------------------------------------


#' Module for generating and re-generating inputs UI on the server
#'
#' @param switches reactive that returns a list with key information needed
#'     for tab switches to be rendered:
#' @import shiny
#' @importFrom waiter Waiter spin_loader transparent
#' @importFrom shinyjs disable enable
#' @importFrom shinyWidgets updateRadioGroupButtons
#' @export
mod_render_inp_tabs_srv <-
  function(id,
           switches = reactive(get_test_tab_switches()),
           tabs = reactive(get_test_tabs(id)),
           summary_tab = reactive(mtcars),
           header = reactive(NULL),
           tab_header = reactive(NULL),
           tab_footer = reactive(NULL),
           ...
  ) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns

      ### TAB switch logic
      observe({
        req(input$inp_tab)
        shiny::updateTabsetPanel(session, "policy_tabs", selected = input$inp_tab)
      })

      ### Render switches
      observe({
        req(switches())
        isolate({
          shinyWidgets::updateRadioGroupButtons(
            session,
            inputId = "inp_tab",
            choices = switches()$choices,
            selected = switches()$selected,
            disabledChoices = switches()$disabledChoices,
            justified = TRUE
          )
        })
      },
      label = "inp-tab: update switches",
      priority = 10)

      ### Setting output priorities
      observe({
        if ("tab_header_ui" %in% names(outputOptions(output))) {
          outputOptions(output, "tab_header_ui", priority = 1000)
        }

        if ("inputs_summary_dt" %in% names(outputOptions(output))) {
          outputOptions(output, "inputs_summary_dt", priority = 10)
        }

      })

      ### Render headers and footers
      output$tab_header_ui <- shiny::renderUI({req(header())})
      output$tab_header_intab_ui <- shiny::renderUI({req(tab_header())})
      output$tab_footer_ui <- shiny::renderUI({req(tab_footer())})

      ### Render tabs
      old_tabs <- reactiveVal(NULL)
      observeEvent( #
        tabs(),
        {
          req(tabs())
          if (isTruthy(old_tabs())) {
            walk(old_tabs(),  ~ {
              removeTab(inputId = "policy_tabs",
                        target = .x,
                        session = session)
            })
          }

          tabs() %>%
            mutate(row = row_number()) %>%
            purrr::pwalk( ~ {
              dts <- rlang::dots_list(...)

              if (input$inp_tab %in% tabs()$tab_id) {
                select <- input$inp_tab == dts$tab_id
              } else {
                select <- dts$row == 1
              }

              shiny::insertTab(
                inputId = "policy_tabs",
                tab = shiny::tabPanelBody(value = dts$tab_id, dts$tab_ui),
                target = "summary",
                select = select,
                position = "before",
                session = session
              )
            })

          tabs()$tab_id %>% old_tabs()
        },
        ignoreNULL = FALSE,
        ignoreInit = FALSE,
        label = "inp-tab: render tabs UI",
        priority = 10
      )

      ### Render summary DT
      output$inputs_summary_dt <-
        DT::renderDT({
          shiny::validate(shiny::need(summary_tab(), "Error in data for vis policy diff"))
          req(summary_tab()) %>%
            select(-1) %>%
            fct_config_gen_dt("Policy scenarios summary", group_row = 1)
          },
        server = FALSE)
    })
  }



# Testers -----------------------------------------------------------------

#' Test simple logic of changing number of choices and tabs
#'
#'
#' @noRd
#' @export
test_mod_inp_n_choices_tabs <-
  function(path,
           id = NULL,
           n_policy = c(1, 5, 2),
           n_policy_type = c("numericInline", "numeric", "slider", "dropdown", "none"),
           inp_pre_structure = NULL,
           ...) {
    #
    srv <- function(input, output, session) {
      callModule(profvis::profvis_server, "prof")
      test_mod_inp_n_choices_tabs_srv(id,
                                      path = path,
                                      n_policy = n_policy,
                                      n_policy_type = n_policy_type,
                                      inp_pre_structure = inp_pre_structure,
                                      ...)
    }

    test_mod_inp_n_choices_tabs_ui(id) %>%
      tagList(
        profvis::profvis_ui("prof")
      ) %>%
      shinyApp(., srv)

  }



#' UI for the test module with input tabs and n choices
#'
#' @noRd
test_mod_inp_n_choices_tabs_ui <- function(id = NULL) {
  fluidPage(mod_inputs_ui_wrapper(id),
            # profvis::profvis_ui("prof"),
            golem_add_external_resources())
}


#' server for the test module with input tabs and n choices
#'
#' @noRd
test_mod_inp_n_choices_tabs_srv <-
  function(id = NULL,
           path,
           n_policy = c(1, 5, 2),
           n_policy_type = c("numericInline", "numeric", "slider", "dropdown", "none"),
           inp_pre_structure = NULL,
           ...) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns


      inp_raw_str <- path %>% load_input_xlsx()
      inp_tab_str <- path %>% load_inputtabs_xlsx()
      inp_table_str <- path %>% load_inputtables_xlsx()

      local_inp_str = gen_inp_str_front(inp_table_str = inp_table_str)
      local_ui_fn = gen_tabinp_ui_front(inp_tab_str = inp_tab_str,
                                        inp_table_str = inp_table_str)



      inps <- reactiveValues(inp_str = NULL)

      # inp_btns <- mod_inputs_btns_server(
      #   NULL,
      #   n_policy = c(1, 5, 2),
      #   n_policy_type = "dropdown"
      # )

      mod_browser_button_server(id)

      n_policy = c(1, 5, 2)
      n_policy_type = "dropdown"

      current_n_choices <-
        mod_inp_n_choices_server(
          id = id,
          value = reactive(n_policy[[length(n_policy)]]),
          min = reactive(n_policy[[1]]),
          max = reactive(n_policy[[2]]),
          n_policy_type = reactive(n_policy_type[[1]])
        )

      inps$inp_str <-
        mod_build_inp_srv(
          id,
          inp_raw_str = inp_raw_str,
          inp_str_fn = local_inp_str,
          ui_gen_fn = local_ui_fn,
          n_choices = current_n_choices,
          n_max_choices = reactive(n_policy[[2]]),
          reseter = reactive(NULL),
          inp_pre_structure = inp_pre_structure,
          ...
        )

      inps$export <- reactive({mtcars})

      mod_render_inp_tabs_srv(
        id,
        tabs = reactive(inps$inp_str()$all_uis$tabs),
        header = reactive(inps$inp_str()$all_uis$header),
        switches = reactive(inps$inp_str()$all_uis$switches),
        summary_tab = inps$export
      )

    })
  }


#' helper function with the example of the choices for the tab switches to update
#'
#' @noRd
get_test_tab_switches <- function() {
  list(
    choices = c(
      "Group 1" = "panel0",
      "Tab 1" = "panel1",
      "Tab 2" = "panel2",
      "Group 2" = "panel3",
      "Summary" = "summary"
    ),
    selected = c("Tab 1" = "panel1"),
    disabledChoices = c("Group 1" = "panel0", "Group 2" = "panel3"),
    enabled = c("Tab 1" = "panel1", "Tab 2" = "panel2")
  )
}

#' returns a list with dummy tab content for teting
#'
#' @noRd
get_test_tabs <- function(id = NULL) {
  ns = NS(id)
  tibble(
    tab_id = c("panel1", "panel2"),
    tab_order = c(1, 2),
    tab_ui = list(shiny::tags$h1("panel1"),
                  shiny::tags$h1("panel2"))
  )
}


#' Testing the tabs modules
#'
#' @noRd
test_mod_inp_tabs_simple <-
  function(id = NULL,
           switches = get_test_tab_switches(),
           tabs = fct_inp_make_dummy_tabs(switches)) {

    tst_ui <-
      tagList(
        column(width = 4, mod_inp_tab_switches_ui(id) %>% wellPanel()),
        column(width = 8, mod_inp_tabs_content_ui(id)),
        golem_add_external_resources()
      ) %>%
      fluidPage()

    tst_srv <- function(input, output, session) {
      mod_render_inp_tabs_srv(
        id = id,
        switches = reactive(switches),
        tabs = reactive(tabs)
        )
    }

    shinyApp(tst_ui, tst_srv)
  }

#' returns a list with dummy tab content for teting
#'
#' @noRd
fct_inp_make_dummy_tabs <- function(switches) {
  tibble(
    tab_id = unname(switches$enabled),
    tab_order = seq_along(switches$enabled),
    tab_ui =
      switches$enabled %>% imap(~shiny::tags$h1(str_c(.x, " - ", .y))) %>%
      unname()
  )

}


#' Returns a test version of the input tabs structure
#'
#' @noRd
get_test_inp_tab_str_raw <- function(type = 1) {
  list(
    tibble(
      tab_name = c("Group 1", "Tab1", NA, "Tab2", "Tab3", NA, NA, "hr", "Tab4", 'hr'),
      group_order = c(NA, 1, 2, 4, 5, 7, 8, NA, 34, NA)
    ),
    tibble(
      tab_name = c("Tab1", NA, "Tab2", "Tab3", NA, NA, "Tab4"),
      group_order = c( 1, 2, 4, 5, 7, 8, 34)
    ),
    NULL
  ) %>%
    `[[`(type)

}




