## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggprism)
library(patchwork)

## ----fig.height=3.5-----------------------------------------------------------
# Compare methods for adding minor ticks
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(fill = factor(supp))) + 
  theme_prism() + 
  theme(legend.position = "none")

p1 <- p + scale_y_continuous(guide = guide_prism_minor())
p2 <- p + guides(y = guide_prism_minor())

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# refer to guide as string
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(fill = factor(supp))) + 
  theme_prism() + 
  theme(legend.position = "none")

p1 <- p + scale_y_continuous(guide = "prism_minor")
p2 <- p + guides(y = "prism_minor")

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# compare 1 minor ticks (default) vs 4 minor ticks per major tick
p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  stat_summary(aes(fill = factor(dose)), na.rm = TRUE,
               geom = "col", fun = mean, colour = "black", linewidth = 0.9) + 
  theme_prism() + 
  theme(legend.position = "none")

p1 <- p + scale_y_continuous(guide = "prism_minor",
                             limits = c(0, 30),
                             expand = c(0, 0))
p2 <- p + scale_y_continuous(guide = "prism_minor", 
                             limits = c(0, 30),
                             expand = c(0, 0),
                             minor_breaks = seq(0, 30, 2))
p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
p <- ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point(na.rm = TRUE) + 
  theme_prism()

p1 <- p + scale_x_log10(limits = c(1e0, 1e4),
                        guide = "prism_minor")
p2 <- p + scale_x_log10(limits = c(1e0, 1e4),
                        minor_breaks = rep(1:9, 4)*(10^rep(0:3, each = 9)),
                        guide = "prism_minor")
p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# change minor tick length
p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  stat_summary(aes(fill = factor(dose)), na.rm = TRUE,
               geom = "col", fun = mean, colour = "black", linewidth = 0.9) + 
  theme_prism() + 
  scale_y_continuous(guide = "prism_minor", 
                     limits = c(0, 30),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 30, 2))

p1 <- p + theme(legend.position = "none")
p2 <- p + theme(legend.position = "none",
                prism.ticks.length.y = unit(20, "pt"))

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# change minor tick length
p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  stat_summary(aes(fill = factor(dose)), na.rm = TRUE,
               geom = "col", fun = mean, colour = "black", linewidth = 0.9) + 
  theme_prism() + 
  scale_y_continuous(guide = "prism_minor", 
                     limits = c(0, 30),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 30, 2))

p1 <- p + theme(legend.position = "none",
                prism.ticks.length.y = unit(20, "pt"))
p2 <- p + theme(legend.position = "none",
                prism.ticks.length.y = unit(-20, "pt"))

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# change how ticks look
p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  stat_summary(aes(fill = factor(dose)), na.rm = TRUE,
               geom = "col", fun = mean, colour = "black", linewidth = 0.9) + 
  theme_prism() + 
  scale_y_continuous(guide = "prism_minor", 
                     limits = c(0, 30),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 30, 2))

p1 <- p + theme(legend.position = "none")
p2 <- p + theme(legend.position = "none",
                axis.ticks.y = element_line(colour = "blue", 
                                            linewidth = 2, 
                                            lineend = "round"))

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# show that offset axis looks better when you specify the axis limits
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(fill = factor(supp))) + 
  theme_prism() + 
  theme(legend.position = "none")

p1 <- p + scale_y_continuous(guide = "prism_offset")
p2 <- p + scale_y_continuous(limits = c(0, 40), guide = "prism_offset")

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# change appearance of offset axis
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(fill = factor(supp))) + 
  theme_prism() + 
  scale_y_continuous(limits = c(0, 40), guide = "prism_offset")

p1 <- p + theme(legend.position = "none")
p2 <- p + theme(legend.position = "none",
                axis.line.y = element_line(colour = "blue", 
                                           linewidth = 2, 
                                           lineend = "round"))

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# compare prism_minor with prism_offset_minor
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(fill = factor(supp))) + 
  theme_prism() + 
  theme(legend.position = "none")

p1 <- p + scale_y_continuous(guide = "prism_offset")
p2 <- p + scale_y_continuous(guide = "prism_offset_minor")

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(fill = factor(supp))) + 
  theme_prism() + 
  theme(legend.position = "none")

p1 <- p + scale_y_continuous(guide = "prism_offset_minor")
p2 <- p + scale_y_continuous(limits = c(0, 40), 
                             guide = "prism_offset_minor")

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# compare 1 minor tick to 4 minor ticks per major
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(fill = factor(supp))) + 
  theme_prism() + 
  theme(legend.position = "none")

p1 <- p + scale_y_continuous(limits = c(0, 40), 
                             guide = "prism_offset_minor")
p2 <- p + scale_y_continuous(limits = c(0, 40), 
                             minor_breaks = seq(0, 40, 2),
                             guide = "prism_offset_minor")

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# change minor tick length and direction
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(fill = factor(supp))) + 
  theme_prism() + 
  scale_y_continuous(limits = c(0, 40), 
                     minor_breaks = seq(0, 40, 2),
                     guide = "prism_offset_minor")

p1 <- p + theme(legend.position = "none",
                prism.ticks.length.y = unit(20, "pt"))
p2 <- p + theme(legend.position = "none",
                prism.ticks.length.y = unit(-20, "pt"))

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# change minor tick colour, thickness, and lineend
p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(aes(fill = factor(supp))) + 
  theme_prism() + 
  scale_y_continuous(limits = c(0, 40), 
                     minor_breaks = seq(0, 40, 2),
                     guide = "prism_offset_minor")

p1 <- p + theme(legend.position = "none",
                prism.ticks.length.y = unit(20, "pt"))
p2 <- p + theme(legend.position = "none",
                prism.ticks.length.y = unit(20, "pt"),
                axis.ticks.y = element_line(colour = "blue",
                                            linewidth = 2,
                                            lineend = "round"))

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# show bracket axis guide
p1 <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_jitter(aes(shape = factor(dose)), width = 0.2, size = 2) + 
  scale_shape_prism() + 
  theme_prism() + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0, 40), guide = "prism_offset")

p2 <- p1 + scale_x_discrete(guide = "prism_bracket")

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# show bracket axis guide with flipped plot
p1 <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_jitter(aes(shape = factor(dose)), width = 0.2, size = 2) + 
  scale_shape_prism() + 
  theme_prism() + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0, 40), guide = "prism_offset") + 
  scale_x_discrete(guide = "prism_bracket")

p2 <- p1 + coord_flip()

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# control bracket width
p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_jitter(aes(shape = factor(dose)), width = 0.2, size = 2) + 
  scale_shape_prism() + 
  theme_prism() + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0, 40), guide = "prism_offset")

p1 <- p + scale_x_discrete(guide = "prism_bracket")
p2 <- p + scale_x_discrete(guide = guide_prism_bracket(width = 0.2))

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# compare brackets outside or inside
p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_jitter(aes(shape = factor(dose)), width = 0.2, size = 2) + 
  scale_shape_prism() + 
  theme_prism() + 
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(0, 40), guide = "prism_offset")

p1 <- p + scale_x_discrete(guide = "prism_bracket")
p2 <- p + scale_x_discrete(guide = guide_prism_bracket(outside = FALSE))

p1 + p2

## ----fig.height=3.5-----------------------------------------------------------
# adjust text spacing with inside pointing brackets
p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_jitter(aes(shape = factor(dose)), width = 0.2, size = 2) + 
  scale_shape_prism() + 
  theme_prism() + 
  scale_y_continuous(limits = c(0, 40), guide = "prism_offset") + 
  scale_x_discrete(guide = guide_prism_bracket(outside = FALSE))

p1 <- p + theme(legend.position = "none")
p2 <- p + theme(legend.position = "none",
                axis.text.x = element_text(margin = margin(t = 10)))

p1 + p2

## ----fig.asp=0.9, fig.width=5-------------------------------------------------
# define a base plot
base <- ggplot(mpg, aes(x = displ, y = cty)) +
  geom_point(aes(colour = class))

base

## ----fig.asp=0.9, fig.width=5-------------------------------------------------
# apply theme_prism and turn clipping off for the border
p <- base + theme_prism(border = TRUE) +
  guides(colour = guide_legend(position = "inside")) +
  theme(legend.position.inside = c(0.8, 0.75)) +
  coord_cartesian(clip = "off")
p

## ----fig.asp=0.9, fig.width=5-------------------------------------------------
# add axis guides
p <- p + guides(x = "prism_minor", y = "prism_minor")
p

## ----fig.asp=0.9, fig.width=5-------------------------------------------------
# add tick annotations
p_annot <- p + annotation_ticks(sides = "tr", type = "both", linewidth = 1,
                                outside = TRUE,
                                tick.length = unit(14/2, "pt"),
                                minor.length = unit(14/4, "pt"))
p_annot

## ----fig.asp=0.9, fig.width=5-------------------------------------------------
# tick annotations will mirror adjustments to the actual axis ticks
p_annot <- p_annot + scale_x_continuous(minor_breaks = seq(1, 7, 0.2))
p_annot

## -----------------------------------------------------------------------------
# multiply one of the len values by 100
tg <- ToothGrowth
tg[2, "len"] <- tg[2, "len"] * 100

## ----fig.width=4, fig.height=5------------------------------------------------
ggplot(tg, aes(x = factor(dose), y = len)) + 
  geom_jitter(aes(shape = factor(dose)), width = 0.2, size = 2) + 
  scale_shape_prism() + 
  theme_prism() + 
  theme(legend.position = "none")

## ----fig.width=4, fig.height=3------------------------------------------------
p_bottom <- ggplot(tg, aes(x = factor(dose), y = len)) + 
  geom_jitter(aes(shape = factor(dose)), width = 0.2, size = 2) + 
  scale_shape_prism() + 
  coord_cartesian(ylim = c(0, 60)) + 
  guides(x = "prism_bracket", y = "prism_offset_minor") + 
  theme_prism() + 
  theme(legend.position = "none")

p_bottom

## ----fig.width=4, fig.height=1.5----------------------------------------------
p_top <- ggplot(tg, aes(x = factor(dose), y = len)) + 
  geom_jitter(aes(shape = factor(dose)), width = 0.2, size = 2) + 
  scale_shape_prism() + 
  coord_cartesian(ylim = c(1140, 1160)) +
  scale_y_continuous(breaks = c(1140, 1160)) +
  guides(y = "prism_offset_minor")

theme_outlier <- function(palette = "black_and_white",
                          base_size = 14,
                          base_family = "sans",
                          base_fontface = "bold",
                          base_line_size = base_size/14,
                          base_rect_size = base_size/14,
                          axis_text_angle = 0,
                          border = FALSE) {
  theme_prism(palette = palette,
              base_size = base_size,
              base_family = base_family,
              base_fontface = base_fontface,
              base_line_size = base_line_size,
              base_rect_size = base_rect_size,
              axis_text_angle = axis_text_angle,
              border = border) %+replace% 
    theme(axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          legend.position = "none")
}

p_top <- p_top + theme_outlier()
p_top

## ----fig.width=4, fig.height=5------------------------------------------------
p_top / p_bottom + 
  plot_layout(heights = c(1, 4)) & 
  theme(axis.text.y = element_text(colour = "red"))

