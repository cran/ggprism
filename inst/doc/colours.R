## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggprism)
library(patchwork)

## ----fig.width=3.6, fig.asp=0.9-----------------------------------------------
# create a base plot to compare colour scales
base <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(colour = factor(cyl), shape = factor(cyl)), size = 3) + 
  theme_prism() + 
  guides(colour = guide_legend(position = "inside")) +
  theme(legend.position.inside = c(0.8, 0.8))

base

## ----fig.width=7.2, fig.asp=0.5-----------------------------------------------
# compare manual colour scale with prism colour scale
p1 <- base + scale_colour_manual(values = c("blue", "red", "green3"))
p2 <- base + scale_colour_prism()

p1 + p2

## -----------------------------------------------------------------------------
# see names and lengths of available scale_colour_prism() palettes
lengths(ggprism_data$colour_palettes)

## ----fig.width=7.2, fig.asp=0.5-----------------------------------------------
# try out some different colour palettes
p1 <- base + scale_colour_prism(palette = "purple_passion")
p2 <- base + scale_colour_prism(palette = "candy_bright")

p1 + p2

## ----fig.width=4.5------------------------------------------------------------
preview_theme("flames")

## ----fig.width=3.6, fig.asp=0.9-----------------------------------------------
# create a base plot to compare fill scales
base <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(aes(fill = factor(cyl), shape = factor(cyl)), size = 3) + 
  theme_prism() + 
  guides(fill = guide_legend(position = "inside"),
         shape = guide_legend(position = "inside")) +
  theme(legend.position.inside = c(0.8, 0.8)) + 
  scale_shape_prism(palette = "filled")

base

## ----fig.width=7.2, fig.asp=0.5-----------------------------------------------
# compare manual fill scale with prism fill scale
p1 <- base + scale_fill_manual(values = c("blue", "red", "green3"))
p2 <- base + scale_fill_prism()

p1 + p2

## -----------------------------------------------------------------------------
# see names and lengths of available scale_fill_prism() palettes
lengths(ggprism_data$fill_palettes)

## ----fig.width=7.2, fig.asp=0.5-----------------------------------------------
# try out some different fill palettes
p1 <- base + scale_fill_prism(palette = "colorblind_safe")
p2 <- base + scale_fill_prism(palette = "neon")

p1 + p2

## ----fig.width=4.5------------------------------------------------------------
preview_theme("diazo")

## -----------------------------------------------------------------------------
# see names and lengths of available scale_shape_prism() palettes
lapply(ggprism_data$shape_palettes, nrow)

## ----fig.width=3.6, fig.asp=0.9-----------------------------------------------
# define a function for convenience
show_shapes <- function(palette) {
  df_shapes <- ggprism_data$shape_palettes[[palette]][, -1]
  df_shapes$pch_f <- factor(df_shapes$pch, levels = df_shapes$pch)

  ggplot(df_shapes, aes(x = 0, y = 0, shape = pch)) +
    geom_point(aes(shape = pch), size = 5, fill = 'red') +
    scale_shape_identity() +
    facet_wrap(~ pch_f) +
    theme_void()
}

# show the shapes in the palette "complete"
show_shapes("complete")

## ----fig.width=3.6, fig.asp=0.9-----------------------------------------------
# create a base plot to compare shape scales
base <- ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point(aes(colour = class, fill = class, shape = class)) + 
  theme_prism(base_size = 11, base_fontface = "plain", border = TRUE) +
  guides(colour = guide_legend(position = "inside"),
         fill = guide_legend(position = "inside"),
         shape = guide_legend(position = "inside")) +
  theme(plot.subtitle = element_text(face = "bold"),
        legend.position.inside = c(0.8, 0.75),
        legend.key.height = unit(10, "pt")) +
  coord_cartesian(clip = "off") + 
  scale_colour_prism(palette = "floral") + 
  scale_fill_prism(palette = "floral")

base

## ----fig.width=7, fig.height=6------------------------------------------------
# compare shape scales
p1 <- base
p2 <- base + scale_shape_prism(palette = "default") + 
  labs(subtitle = "default")
p3 <- base + scale_shape_prism(palette = "filled") + 
  labs(subtitle = "filled")
p4 <- base + scale_shape_prism(palette = "complete") + 
  labs(subtitle = "complete")

(p1 + p2) / (p3 + p4)

