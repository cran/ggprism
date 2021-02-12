## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggprism)
library(patchwork)

## -----------------------------------------------------------------------------
# define a base plot
base <- ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point(aes(colour = class))

## ---- fig.height=3.5----------------------------------------------------------
# apply default theme
p1 <- base + theme_prism() + 
  theme(legend.position = c(0.8, 0.75),
        legend.key.height = unit(10, "pt"))
p2 <- base + theme_prism() + 
  theme(legend.position = c(0.8, 0.75),
        legend.key.height = unit(10, "pt"),
        legend.title = element_text())

p1 + p2

## ---- fig.height=3.5----------------------------------------------------------
# redefine base plot without a legend for convenience
base <- ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point(aes(colour = class), show.legend = FALSE)

# adjust overall theme size
p1 <- base + theme_prism(base_size = 10)
p2 <- base + theme_prism(base_size = 16)

p1 + p2

## ---- fig.height=3.5----------------------------------------------------------
# adjust overall theme size with specific line size
p1 <- base + theme_prism(base_size = 14)
p2 <- base + theme_prism(base_size = 14, base_line_size = 0.2)

p1 + p2

## ---- fig.height=3.5----------------------------------------------------------
# change fontface or font family
p1 <- base + theme_prism(base_fontface = "plain")
p2 <- base + theme_prism(base_family = "mono")

p1 + p2

## ---- fig.height=3.5----------------------------------------------------------
# change x axis text angle
p1 <- base + theme_prism(axis_text_angle = 45)
p2 <- base + theme_prism(axis_text_angle = 90)

p1 + p2

## ---- fig.height=3.5----------------------------------------------------------
# add a border and adjust its thickness
p1 <- base + theme_prism(border = TRUE) + 
  coord_cartesian(clip = "off")
p2 <- base + theme_prism(border = TRUE, base_rect_size = 2) + # adjust thickness
  coord_cartesian(clip = "off")

p1 + p2

## -----------------------------------------------------------------------------
# see names of available theme_prism() palettes
names(ggprism_data$themes)

## ---- fig.height=3.5----------------------------------------------------------
# try out some different theme palettes
p1 <- base + theme_prism(palette = "purple_passion")
p2 <- base + theme_prism(palette = "candy_bright")

p1 + p2

## ---- fig.width=4.5-----------------------------------------------------------
preview_theme("flames")

## ---- fig.height=3.5----------------------------------------------------------
# compare two identical theme palettes
p1 <- base + theme_prism(palette = "black_and_white")
p2 <- base + theme_prism(palette = "plasma")

p1 + p2

## ---- fig.height=3.5----------------------------------------------------------
# try out two more theme palettes and their corresponding colour palettes
p1 <- base + theme_prism(palette = "summer") + 
  scale_colour_prism(palette = "summer")
p2 <- base + theme_prism(palette = "stained_glass") + 
  scale_colour_prism(palette = "stained_glass")

p1 + p2

## -----------------------------------------------------------------------------
# define a new theme function based on the stained_glass palette
theme_new <- function(base_size = 14,
                      base_family = "sans",
                      base_fontface = "bold",
                      base_line_size = base_size / 14,
                      base_rect_size = base_size / 14,
                      axis_text_angle = 0,
                      border = FALSE) {
  theme_prism(palette = "stained_glass",
              base_size = base_size,
              base_family = base_family,
              base_fontface = base_fontface,
              base_line_size = base_line_size,
              base_rect_size = base_rect_size,
              axis_text_angle = axis_text_angle,
              border = border) %+replace% 
    theme(panel.background = element_rect(fill = "white",
                                          colour = NA),
          plot.background = element_rect(fill = "red",
                                         colour = NA),
          axis.line = element_line(colour = "black"),
          axis.ticks = element_line(colour = "black"))
}

## ---- fig.height=3.5----------------------------------------------------------
# compare theme_prism() and our new theme function
p1 <- base + theme_prism()
p2 <- base + theme_new()

p1 + p2

