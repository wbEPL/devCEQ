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
        DT::DTOutput(ns("inputs_ui_values"))
      )
    ) %>%
      div(id = ns("policy_choices_holder")),

    # Place for the footer just in case
    # mod_inp_tab_footer_ui(id),

    # Debugging UI optiosn
    if (getOption("ceq_dev", FALSE)) {
      taglist(h4("dev output for mod_inp_tabs_content_ui()"),
              shiny::verbatimTextOutput(ns("dynamic_inputs_ui_info")))
    }
  )

}



#' @describeIn mod_inp_tabs_content_ui UI for input tabs switches.
#'
#' @noRd
#' @importFrom shiny uiOutput
#' @export
mod_inp_tab_switches_ui <-
  function(id) {
    ns = NS(id)

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
      choices = c("Policy choices" = "panel1",
                  "Summary Table" = "summary"),
      direction = "vertical",
      justified = TRUE
    )

  }


#' @describeIn mod_render_inp_ui_srv UI for input tabs bodies
#' @noRd
#' @import shiny
#' @importFrom shiny uiOutput
#' @export
mod_inp_tabs_ui <-
  function(id) {
    ns = NS(id)
    shiny::uiOutput(ns("tabs_ui"))
  }

#' @describeIn mod_render_inp_ui_srv UI for tabs header
#' @noRd
#' @importFrom shiny uiOutput
#' @export
mod_inp_tab_header_ui <-
  function(id) {
    ns = NS(id)
    shiny::uiOutput(ns("tab_header_ui"))
  }

#' @describeIn mod_render_inp_ui_srv UI for tabs mod_inp_tab_header_intab_ui
#' @noRd
#' @importFrom shiny uiOutput
#' @export
mod_inp_tab_header_intab_ui <-
  function(id) {
    ns = NS(id)
    shiny::uiOutput(ns("tab_header_intab_ui"))
  }


#' @describeIn mod_render_inp_ui_srv UI for tabs footer
#' @noRd
#' @importFrom shiny uiOutput
#' @export
mod_inp_tab_footer_ui <-
  function(id) {
    ns = NS(id)
    shiny::uiOutput(ns("tab_footer_ui"))
  }

