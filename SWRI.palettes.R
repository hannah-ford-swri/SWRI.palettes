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

SWRI_pal <- function(palette, reverse = FALSE, ...){
  pal <- SWRI_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_colour_SWRI <- function(palette, discrete = TRUE, reverse = FALSE, ...){
  pal <- SWRI_pal(palette = palette, reverse = reverse)
  
  if (discrete){
    discrete_scale("colour", paste0("SWRI_", palette), palette = pal, ...)
  } else{
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_SWRI <- function(palette, discrete = TRUE, reverse = FALSE, ...){
  pal <- SWRI_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("SWRI_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
  geom_point(size = 4) +
  scale_colour_SWRI(palette = "pastel")

ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
  geom_point(size = 4, alpha = .6) +
  scale_colour_SWRI(discrete = FALSE, palette = "pastel")

ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_SWRI(palette = "bold", reverse = TRUE, guide = "none")

colors <- c("#f4546c", "#34346c", "#f77f00", "#2a9d8f", "#e9c46a", 
            "#8d99ae", "#5c527f", "#7a7f9a", "#a8d0e6", "#f2a6b5")

df <- data.frame(color = colors, x = seq_along(colors))
df2 <- data.frame(x = seq(1,100),
                  y = rep("a",100))
df3 <- data.frame(x = seq(1, 5),
                  y = rep("A", 5))

ggplot(df3, aes(x = factor(x), y = factor(y), fill = x)) +
  geom_col(width = 1) +
  #scale_fill_identity() +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_SWRI(palette = "bicolour", discrete = FALSE, reverse = FALSE, guide = "none")
