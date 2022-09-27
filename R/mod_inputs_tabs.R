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
      div(id = ns("policy_choices_holder")),

    # Place for the footer just in case
    mod_inp_tab_footer_ui(id),

    # Debugging UI options
    if (getOption("ceq_dev", FALSE)) {
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

    # shiny::radioButtons(
    #   inputId = ns("inp_tab"),
    #   label = NULL,
    #   choices = c("Policy choices" = "panel1",
    #               "Summary Table" = "summary"),
    #   inline = FALSE,
    #   width = "100%"
    # )

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
            purrr::pwalk( ~ {
              dts <- rlang::dots_list(...)
              shiny::insertTab(
                inputId = "policy_tabs",
                tab = shiny::tabPanelBody(value = dts$tab_id, dts$tab_ui),
                target = "summary",
                select = dts$tab_order == 1,
                position = "before",
                session = session
              )
            })

          tabs()$tab_id %>% old_tabs()
        },
        ignoreNULL = FALSE,
        ignoreInit = FALSE
      )

      ### Render summary DT
      output$inputs_summary_dt <-
        DT::renderDT({
          shiny::validate(shiny::need(summary_tab(), "Error in data for vis policy diff"))
          req(summary_tab())
        },
        server = FALSE)
    })
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




