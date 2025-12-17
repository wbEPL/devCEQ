#' peaapp_dependency
#' 
#' @importFrom htmltools htmlDependency
#' @noRd
devCEQ_dependency <- function() {
  htmltools::htmlDependency(
    name = "devceq-assets", 
    version = "0.1",
    package = "devCEQ",
    src = "assets",
    script = "js/peapp.js",
    stylesheet = c("css/devceq.css")
  )
}

#' @importFrom shiny addResourcePath
#' @noRd
.onLoad <- function(libname, pkgname) {
    resources <- system.file("assets", package = "devCEQ")
    addResourcePath("www", resources)
}