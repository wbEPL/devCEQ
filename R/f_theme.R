#' New theming funcitons
#' @name f_theme
#' 
NULL


#' @describeIn f_theme Add a loading screen to a Shiny app using the `waiter` package
#' 
#' @importFrom shiny tagList div tags
#' @importFrom waiter useWaiter waiterPreloader bs5_spinner
#' 
waiter_on_load <- function() {
  tagList(
    waiter::useWaiter(),
    waiter::waiterPreloader(
      html = tagList(
        waiter::bs5_spinner(
          style = c("spin", "grow"),
          color = c("light")
        ),
        div(
          str_c("Loading ", get_app_name(), "..."),
          style = "margin-top: 1em; font-size: 1.25em; color: white;"
        )#,
        # tags$img(
        #   src = "www/logo/wbg-logo-prosperity-white.svg", # Replace with your logo path
        #   height = "60px",
        #   style = "margin-top: 1em;"
        
      ),
      color = "#004972" # WBG blue background
    )
  )
}