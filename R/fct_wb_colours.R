##' @name wb_colours
##' @title Colours and pallets function for the WB corp colours scheme
##'
##' @export
##' @noRd
wb_colours <- c(
  `red`        = "#F05023",
  `yellow`     = "#FDB714",
  `blue`       = "#009CA7",
  `green`      = "#00AB51",


  `red2`        = "#EB1C2D",
  `yellow2`     = "#F78D28",
  `blue2`       = "#00A996",
  `violet2`     = "#872B90",


  `red3`        = "#98252B",
  `orange3`     = "#E16A2D",
  `yellow3`     = "#B88C1D",
  `green3`      = "#006450",
  `blue3`       = "#006068",
  `violet3`      = "#614776",

  `wb_solidblue` = "#002244",
  `wb_brightblue` = "#009FDA",
  `black` = "#000000",
  `white` = "#ffffff")


#' Function to extract wb colors as hex codes
#'
#' @param ... Character names of wb_colors
#' @examples
#'
#' \dontrun{
#' wb_cols()
#' wb_cols("red")
#' wb_cols("red", "blue")
#' wb_cols("blue", "red")
#'
#' library(ggplot2)
#' ggplot(mtcars, aes(hp, mpg)) +
#'   geom_point(color = wb_cols("red"),
#'              size = 4, alpha = .8)
#' }
#' @rdname wb_colours
#' @export
wb_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (wb_colours)

  wb_colours[cols]
}



#' @rdname wb_colours
#' @export
wb_palettes <- list(
  `main`  = wb_cols("blue", "green", "yellow", "red"),

  `main2`  = wb_cols("violet2", "blue2", "yellow2", "red2"),
  `main3`  = wb_cols("violet2", "blue", "green", "yellow", "yellow2", "red2"),

  `neutral_warm`   = wb_cols("yellow3", "orange3", "red3"),
  `neutral_cold`   = wb_cols("violet3", "blue3", "green3"),

  `neutral_all` = wb_cols("violet3", "blue3", "green3", "yellow3", "orange3", "red3")
)



#' Return function to interpolate a wb color palette
#'
#' @param palette Character name of palette in wb_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @examples
#'
#' \dontrun{
#' wb_pal("main2")
#' wb_pal("neutral_all")(10)
#' }
#'
#' @rdname wb_colours
#' @export
#'
wb_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- wb_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}



#' Color scale constructor for wb colors
#'
#' @param palette Character name of palette in wb_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @rdname wb_colours
#' @export
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
scale_color_wb <- function(palette = "main2", discrete = TRUE, reverse = FALSE, ...) {
  pal <- wb_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("wb_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for wb colors
#'
#' @param palette Character name of palette in wb_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
#' @examples
#'
#' \dontrun{
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point(size = 4) +
#'   scale_color_wb(palette = "main2")
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'   geom_point(size = 4, alpha = .6) +
#'   scale_color_wb(discrete = FALSE, palette = "neutral_warm")
#'
#'
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#'   geom_bar() +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#'   scale_fill_wb(palette = "main3", guide = "none")
#' }
#'
#' @rdname wb_colours
#' @export
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
scale_fill_wb <- function(palette = "main2", discrete = TRUE, reverse = FALSE, ...) {
  pal <- wb_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("wb_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
