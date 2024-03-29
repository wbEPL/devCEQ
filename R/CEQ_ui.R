#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom purrr map
#' @importFrom bsplus use_bs_popover
#' @importFrom tippy use_tippy
#' @importFrom profvis profvis_ui
#' @importFrom waiter use_waiter
#' @importFrom bslib bs_theme
#' @export
CEQ_ui <- function(
    request,
    theme_fn = function() {
      bslib::bs_theme(version = 4, bootswatch = "flatly", "enable-rounded" = TRUE)
    },
    style_logo_position = NULL,
    inp_nav_width = NULL,
    fn_results_ui = fn_results_ui_dummy,
    ...
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
          div(id = "img-logo-navbar", style = style_logo_position,
              # "position: fixed; right: 6rem; padding-top: 0.3125rem;
              # padding-bottom: 0.3125rem; z-index: 1; top: 5px;"
              img(src = "www/WBG_Horizontal-white_gradient-web.png", id = "logo-style")
              ),
          get_app_name()
        ),
      windowTitle = get_app_name(),
      collapsible = TRUE,
      theme = theme_fn(),
      selected = "Info",
      tabPanel("Info"),

      # Policy choices tab.
      tabPanel(
        "Policy Choices",
        value = "pc2019",
        mod_inputs_ui_wrapper('generic_inputs', inp_nav_width = inp_nav_width)
      ),

      # Results tab
      fn_results_ui(id = "ceqsim"),

      # # Dev results tab
      # if(golem::app_dev()) {
      #   tabPanel("DEV-Results", shiny::h1("Results page"))
      # },

      if(golem::app_dev()) {
        tabPanel("DEV-Results", devCEQ::mod_dev_res_ui("devres"))
      },

      tabPanel("How it works?", value = "howto")
    )

  tagList(
    golem_add_external_resources(),
    if (isTRUE(unlist(options("golem.app.prod"))))
    {
      waiter::waiter_show_on_load(spinner)
    },
    if (isTRUE(unlist(options("golem.app.prod"))))
    {
      waiter::waiter_hide_on_render(NS('generic_inputs')("n_policy_ui"))
    },
    pages
  )
}



#' Generate a local CEQ UI function
#'
#' @export
#' @importFrom bslib bs_theme
#' @importFrom shinythemes shinytheme
gen_ceq_ui <-
  function(#
    fn_results_ui = fn_results_ui_dummy,
    theme_fn = function() shinythemes::shinytheme("cerulean"),
      # function() NULL,
    # function() {
    #   bslib::bs_theme(version = 4, bootswatch = "cerulean", "enable-rounded" = TRUE)
    # },
    style_logo_position = NULL,
    inp_nav_width = NULL,
    ...
  ) {

    function(request) {
      CEQ_ui(
        request,
        theme_fn = theme_fn,
        style_logo_position = style_logo_position,
        inp_nav_width = inp_nav_width,
        fn_results_ui = fn_results_ui,
        ...
      )
    }
  }

#' Dummy results UI function
#'
#' @noRd
#' @export
fn_results_ui_dummy <- function(id) {
  ns = NS(id)
  tabPanel(
    "Results",
    value = "Results",
    shiny::h1(paste0("Results page in ns: ", ns("ID")))
  )
}

#' Dummy results UI function
#'
#' @noRd
#' @export
fn_results_ui_dummy2 <- function(id) {
  ns = NS(id)
  tabPanel(
    "Results",
    value = "Results",
    shiny::h1(paste0("2222Results page in ns: ", ns("ID")))
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
    # if (isTRUE(unlist(options( "golem.app.prod" )))) {waiter::waiterPreloader()},
    # shinyalert::useShinyalert(),
    shinyWidgets::useSweetAlert(theme = "bootstrap-4"),
    shinyFeedback::useShinyFeedback(feedback = TRUE, toastr = TRUE),
    # waiter::waiter_on_busy(),
    bsplus::use_bs_popover(),
  )
}
