#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @inheritParams mod_inputs_ui_wrapper
#'
#' @import shiny
#' @importFrom purrr map
#' @importFrom bsplus use_bs_popover
#' @importFrom tippy use_tippy
#' @importFrom profvis profvis_ui
#' @importFrom waiter use_waiter
#' @noRd
#' @export

CEQ_ui <- function(
    request,
    theme_fn = function() {
      bslib::bs_theme(version = 4, bootswatch = "flatly", "enable-rounded" = TRUE)
      }
    ) {

  spinner <- tagList(#
    waiter::spin_circle(),
    br(),
    span(paste0(get_app_name(), " loading..."), style = "color:white;")
    )

  pages <-
    navbarPage(
      id = "main_sidebar",
      title =
        div(
          div(id = "img-logo-navbar",
              style = "right: 80px; top:5px; z-index: 1;",
              img(src = "www/WBG_Horizontal-white_gradient-web.png",
                  style = "width: auto; height: 40px;")
              ),
          get_app_name()
        ),
      windowTitle = get_app_name(),
      collapsible = TRUE,
      theme = theme_fn(),
      selected = "Info",
      tabPanel("Info"),
      tabPanel(
        "Policy Choices",
        value = "pc2019",
        mod_inputs_ui_wrapper('generic_inputs')
      ),
      tabPanel("Results",
               shiny::h1("Results page")
               # mod_ceq2019_results_ui("ceq2019")
               ),
      tabPanel("DEV-Results", shiny::h1("Results page")),

      # if(isTRUE(getOption("ceq_results_dev"))) {
      #   tabPanel("DEV-Results", devCEQ::mod_dev_res_ui("devres"))
      # },

      tabPanel("How it works?", value = "howto")
    )

  tagList(
    golem_add_external_resources(),
    pages
  )
}

#' Return app name from options:
#'
#' @noRd
#' @export
get_app_name <- function() {
  app_name <- options("current.app.name")
  if (is.null(app_name[[1]])) app_name <- ""
  return(app_name[[1]])
}

#' Format app title
#'
#' @import shiny
#' @noRd
#' @export
app_title <-
  function(x) {
    span(tags$span(tags$b(x), class = "text-primary"), "")
  }


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom cicerone use_cicerone
#' @importFrom shinyalert useShinyalert
#' @importFrom shinyWidgets useSweetAlert
#' @importFrom shinyjs useShinyjs
#' @importFrom waiter use_waiter
#' @importFrom bsplus use_bs_popover
#' @importFrom shinyFeedback useShinyFeedback
#' @noRd
#' @export
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    # favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = get_app_name()
    ),
    cicerone::use_cicerone(),
    shinyjs::useShinyjs(),
    waiter::use_waiter(),
    # shinyalert::useShinyalert(),
    shinyWidgets::useSweetAlert(theme = "bootstrap-4"),
    shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
    # waiter::waiter_on_busy(),
    bsplus::use_bs_popover(),
  )
}
