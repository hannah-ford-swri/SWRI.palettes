SWRI_cols <- c(`SWRI pink` = "#f4546c",
               `SWRI purple` = "#34346c",
               `orange` = "#f77f00",
               `dark teal` = "#2a9d8f",
               `yellow` = "#e9c46a",
               `light grey` = "#8d99ae",
               `purple grey` = "#5c527f",
               `grey` = "#7a7f9a",
               `light blue` = "#a8d0e6",
               `light pink` = "#f2a6b5",
               `white` = "#FFFFFF",
               `amber` = "#f0a235",
               `pale pink` = "#FAB4BE",
               `pale purple` = "#9696CD",
               `pale orange` = "#FFCC93",
               `pale teal` = "#AAE8E1",
               `pale yellow` = "#F6e8c6",
               `dark yellow` = "#E5BA4D",
               `mid purple` = "#B56392",
               `deep blue` = "#3385b3",
               `blue` = "#6aaed4",
               `teal` = "#69c2b8",
               `blue teal` = "#278b9a",
               `terracotta` = "#ec875c",
               `beige` = "#e8c4a2")

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
  `blues` = SWRI_colours("teal", "SWRI purple", "deep blue"),
  `pastel` = SWRI_colours("pale purple", "light blue", "light pink"),
  `bold` = SWRI_colours("SWRI pink", "orange", "yellow", "dark teal", "deep blue", "SWRI purple"),
  `bicolour` = SWRI_colours("SWRI pink", "SWRI purple"),
  `sunset` = SWRI_colours("SWRI purple", "mid purple", "SWRI pink", "orange", "yellow"),
  `tricolour` = SWRI_colours("SWRI pink", "white", "SWRI purple"),
  `stability` = SWRI_colours("SWRI purple", "light blue", "amber", "SWRI pink"),
  `pale bold` = SWRI_colours("pale pink", "pale orange", "pale yellow", "pale teal", "light blue", "pale purple"),
  `stability2` = SWRI_colours("SWRI purple", "mid purple", "SWRI pink", "dark yellow"),
  `candy` = SWRI_colours("amber", "SWRI pink", "SWRI purple", "pale purple"),
  `festival` = SWRI_colours("purple grey", "deep blue", "SWRI pink", "dark yellow"),
  `blossom` = SWRI_colours("SWRI pink", "orange", "yellow", "teal"),
  `fjord` = SWRI_colours("deep blue", "dark teal", "purple grey", "light grey"),
  `fruit` = SWRI_colours("orange", "SWRI pink", "SWRI purple", "dark teal"),
  `meadow` = SWRI_colours("dark teal", "yellow", "mid purple", "blue"),
  `orchard` = SWRI_colours("mid purple", "SWRI pink", "amber", "dark teal"),
  `harbour` = SWRI_colours("purple grey", "deep blue", "dark teal", "yellow"),
  `skittles` = SWRI_colours("SWRI pink", "yellow", "dark teal", "deep blue", "mid purple"),
  `ponyo` = SWRI_colours("light grey", "blue teal", "SWRI pink", "terracotta", "dark yellow", "beige"),
  `canyon` = SWRI_colours("terracotta", "dark yellow", "beige", "purple grey"),
  `lagoon` = SWRI_colours("blue teal", "teal", "light blue", "SWRI purple"),
  `tropic` = SWRI_colours("terracotta", "SWRI pink", "deep blue", "amber"),
  `pebble` = SWRI_colours("beige", "purple grey", "blue teal"),
  `summer` = SWRI_colours("deep blue", "yellow", "terracotta"),
  `prairie` = SWRI_colours("beige", "dark yellow", "dark teal", "mid purple"),
  `drift` = SWRI_colours("purple grey", "blue teal", "beige", "light grey"),
  `apple` = SWRI_colours("SWRI pink", "terracotta", "dark teal", "yellow"),
  `mosaic` = SWRI_colours("mid purple", "blue teal", "dark yellow", "terracotta", "beige"),
  `solstice` = SWRI_colours("terracotta", "amber", "yellow", "blue teal", "SWRI purple"),
  `fern` = SWRI_colours("blue teal", "dark teal", "pale teal", "beige"),
  `ceramic` = SWRI_colours("blue teal", "terracotta", "beige"),
  `marigold` = SWRI_colours("teal", "dark yellow", "SWRI pink", "SWRI purple"),
  `dune` = SWRI_colours("teal", "deep blue", "dark yellow", "terracotta"),
  `carnival` = SWRI_colours("blue", "SWRI purple", "SWRI pink", "orange"),
  `pollen` = SWRI_colours("SWRI pink", "dark yellow", "dark teal", "blue"),
  `resins` = c("Control" = "#f77f00",
               "Sulfonic Na+" = "#b33b4c",
               "Sulfonic H+" = "#f4546c",
               "Iminodiacetic Na+" = "#34346c",
               "Iminodiacetic H+" = "#727199",
               "Purolite Sulfonic Na+" = "#ff98a7",
               "Process Control" = "#2a9d8f")
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

