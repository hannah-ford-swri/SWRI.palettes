SWRI_cols <- c(`SWRI pink` = "#f4546c",
               `SWRI purple` = "#34346c",
               `orange` = "#f77f00",
               `teal` = "#2a9d8f",
               `yellow` = "#e9c46a",
               `light grey` = "#8d99ae",
               `purple grey` = "#5c527f",
               `grey` = "#7a7f9a",
               `light blue` = "#a8d0e6",
               `light pink` = "#f2a6b5")

#' SWRI_colours function
#'
#' returns hex code from colour descriptor
#' @param The name of the colour(s)
#' @return the hexadecimal code of the colour
#' @export

SWRI_colours <- function(...){
  cols <- c(...)

  if (is.null(cols))
    return (SWRI_cols)

  SWRI_cols[cols]
}

SWRI_palettes <- list(
  `warm` = SWRI_colours("light pink", "SWRI pink", "orange", "yellow"),
  `blues` = SWRI_colours("teal", "SWRI purple", "purple grey", "light blue"),
  `pastel` = SWRI_colours("purple grey", "grey", "light blue", "light pink"),
  `bold` = SWRI_colours("SWRI pink", "orange", "yellow", "teal", "SWRI purple"),
  `bicolour` = SWRI_colours("SWRI pink", "SWRI purple")
)

#' SWRI_pal function
#'
#' function to palettise a list of colours in palette, including gradients and reverse
#'
#' @param palette name, reverse true or false
#' @return colour palette
#' @export
SWRI_pal <- function(palette, reverse = FALSE, ...){
  pal <- SWRI_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' scale_colour_SWRI function
#'
#' function to create colour gradients for "colour" in ggplot
#'
#' @param palette name, discrete true or false, reverse true or false
#' @return colour palette
#' @export
scale_colour_SWRI <- function(palette, discrete = TRUE, reverse = FALSE, ...){
  pal <- SWRI_pal(palette = palette, reverse = reverse)

  if (discrete){
    discrete_scale("colour", paste0("SWRI_", palette), palette = pal, ...)
  } else{
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' scale_fill_SWRI function
#'
#' function to create colour gradients for "fill" in ggplot
#'
#' @param palette name, discrete true or false, reverse true or false
#' @return colour palette
#' @export
scale_fill_SWRI <- function(palette, discrete = TRUE, reverse = FALSE, ...){
  pal <- SWRI_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("SWRI_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

