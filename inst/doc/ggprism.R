## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggprism)
library(patchwork)

## ---- fig.width=8, fig.height=3-----------------------------------------------
# compare theme_grey() to theme_prism()
p1 <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  stat_summary(aes(fill = factor(dose)), na.rm = TRUE,
               geom = "col", fun = mean, colour = "black", size = 0.9) + 
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0))

p2 <- p1 + theme_prism(base_size = 14)

p1 + p2
# compare some of the available theme palettes
p3 <- p1 + theme_prism(palette = "mustard_field", base_size = 14)
p4 <- p1 + theme_prism(palette = "flames", base_size = 14)

p3 + p4

## ---- fig.width=8, fig.height=3-----------------------------------------------
# compare some colour and fill palettes with default theme_prism()
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(colour = factor(supp), fill = factor(supp))) + 
  theme_prism(base_size = 14)

p1 <- p + scale_colour_prism(palette = "floral") + 
  scale_fill_prism(palette = "floral")

p2 <- p + scale_colour_prism(palette = "flames") + 
  scale_fill_prism(palette = "flames")

p1 + p2
# try using the same palette for colour, fill, and theme
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(colour = factor(supp), fill = factor(supp)))

p3 <- p + theme_prism(palette = "candy_bright") + 
  scale_colour_prism(palette = "candy_bright") + 
  scale_fill_prism(palette = "candy_bright")

p4 <- p + theme_prism(palette = "neon") + 
  scale_colour_prism(palette = "neon") + 
  scale_fill_prism(palette = "neon")

p3 + p4

## ---- fig.width=8, fig.height=3-----------------------------------------------
# compare ggplot2 default shape order with ggprism default shape order
p1 <- ggplot(msleep[complete.cases(msleep), ], 
             aes(x = sleep_rem, y = sleep_total)) + 
  geom_point(aes(shape = factor(vore)), size = 3) + 
  theme_prism() + 
  theme(axis.title.y = element_blank())

p2 <- p1 + scale_shape_prism()

p1 + p2

## ---- fig.width=7, fig.height=6-----------------------------------------------
# show the 4 different axis guides included in ggprism
p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_jitter(aes(shape = factor(dose)), width = 0.2, size = 2) + 
  scale_shape_prism() + 
  theme_prism() + 
  theme(legend.position = "none")

p1 <- p + scale_y_continuous(limits = c(0, 40), guide = "prism_minor")

p2 <- p + scale_x_discrete(guide = "prism_bracket") + 
  scale_y_continuous(limits = c(0, 40))

p3 <- p + scale_y_continuous(limits = c(0, 40), guide = "prism_offset")

p4 <- p + scale_y_continuous(limits = c(0, 40), guide = "prism_offset_minor")

(p1 + p2) / (p3 + p4)

## ---- fig.width=6, fig.height=4-----------------------------------------------
# make a p-value table
df_p_val <- data.frame(
  group1 = "OJ",
  group2 = "VC",
  p.adj = 0.0606,
  y.position = 36
)

# make a plot
p1 <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(fill = factor(supp))) + 
  scale_fill_prism(palette = "candy_bright") + 
  theme_prism() + 
  theme(legend.position = "none")

# add the p-value
p2 <- p1 + add_pvalue(df_p_val)

p1 + p2

