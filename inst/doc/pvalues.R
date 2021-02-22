## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggprism)
library(patchwork)
library(magrittr)

## -----------------------------------------------------------------------------
str(sleep)

## ---- fig.width=4, fig.height=3.5---------------------------------------------
# create a jitter plot of the sleep data set
# and indicate the means
p <- ggplot(sleep, aes(x = group, y = extra)) +
  geom_jitter(aes(shape = group), width = 0.1) + 
  stat_summary(geom = "crossbar", fun = mean, colour = "red", width = 0.2) + 
  theme_prism() + 
  theme(legend.position = "none")
p

## -----------------------------------------------------------------------------
# perform a t-test and obtain the p-value
result <- t.test(extra ~ group, data = sleep)$p.value
result <- signif(result, digits = 3)
result

## -----------------------------------------------------------------------------
df_p_val <- data.frame(
  group1 = "1",
  group2 = "2",
  label = result,
  y.position = 6
)

## ---- fig.height=3.5----------------------------------------------------------
# add p-value brackets
p1 <- p + add_pvalue(df_p_val,
                     xmin = "group1",
                     xmax = "group2",
                     label = "label",
                     y.position = "y.position") 

# change column names to something silly
colnames(df_p_val) <- c("apple", "banana", "some_label", "some_y_position")

# add p-value brackets again
p2 <- p + add_pvalue(df_p_val,
                     xmin = "apple",
                     xmax = "banana",
                     label = "some_label",
                     y.position = "some_y_position")

p1 + p2

## -----------------------------------------------------------------------------
# return column names back to default
colnames(df_p_val) <- c("group1", "group2", "label", "y.position")

## ---- fig.width=7, fig.height=7-----------------------------------------------
# change bracket and label aesthetics
p1 <- p + add_pvalue(df_p_val,
                     colour = "red", # label
                     label.size = 8, # label
                     fontface = "bold", # label
                     fontfamily = "serif", # label
                     angle = 45, # label
                     hjust = 1, # label
                     vjust = 2, # label
                     bracket.colour = "blue", # bracket
                     bracket.size = 1, # bracket
                     linetype = "dashed", # bracket
                     lineend = "round") # bracket

# use glue expression for label
p2 <- p + add_pvalue(df_p_val, label = "p = {label}")

# make bracket tips longer and use coord_flip
p3 <- p + add_pvalue(df_p_val, tip.length = 0.15, coord.flip = TRUE) + 
  coord_flip()

# change bracket tips independently
# (make one side disappear and the other longer)
p4 <- p + add_pvalue(df_p_val, tip.length = c(0.2, 0))

(p1 + p2) / (p3 + p4)

## ---- fig.height=3.5----------------------------------------------------------
# position label above "group1"
p1 <- p + add_pvalue(df_p_val, label = "p = {label}", 
                     remove.bracket = TRUE, x = 1)

# position label between x = 1 and x = 2
p2 <- p + add_pvalue(df_p_val, label = "p = {label}", 
                     remove.bracket = TRUE, x = 1.5)

p1 + p2

## -----------------------------------------------------------------------------
str(ToothGrowth)

## ---- fig.width=4, fig.height=3.5---------------------------------------------
# create a box plot of the ToothGrowth data set
p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) +
  geom_boxplot(aes(fill = dose), colour = "black") + 
  theme_prism() + 
  theme(legend.position = "none")
p

## -----------------------------------------------------------------------------
# compare means again reference
result1 <- t.test(len ~ dose, 
                  data = subset(ToothGrowth, dose %in% c(0.5, 1.0)))$p.value
result2 <- t.test(len ~ dose, 
                  data = subset(ToothGrowth, dose %in% c(0.5, 2.0)))$p.value

# Benjamini-Hochberg correction for multiple testing
result <- p.adjust(c(result1, result2), method = "BH")

## -----------------------------------------------------------------------------
# don't need group2 column (i.e. xmax)
# instead just specify x position in the same way as y.position
df_p_val <- data.frame(
  group1 = c(0.5, 0.5),
  x = c(2, 3),
  label = signif(result, digits = 3),
  y.position = c(35, 35)
)

## ---- fig.height=3.5----------------------------------------------------------
p1 <- p + add_pvalue(df_p_val, 
               xmin = "group1", 
               x = "x", 
               label = "label",
               y.position = "y.position")

p2 <- p + add_pvalue(df_p_val, 
               xmin = "group1", 
               x = "x", 
               label = "p = {label}",
               y.position = "y.position",
               label.size = 3.2,
               fontface = "bold")

p1 + p2

## ---- fig.width=4, fig.height=3.5---------------------------------------------
df_p_val <- rstatix::t_test(ToothGrowth, len ~ dose, ref.group = "0.5") %>% 
  rstatix::add_xy_position()

p + add_pvalue(df_p_val, 
               label = "p = {p.adj}",
               remove.bracket = TRUE)

## ---- fig.width=4, fig.height=3.5---------------------------------------------
df_p_val <- rstatix::t_test(ToothGrowth, len ~ supp) %>% 
  rstatix::add_x_position()

p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  stat_summary(geom = "col", fun = mean) + 
  stat_summary(geom = "errorbar", 
               fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.3) + 
  theme_prism() + 
  coord_cartesian(ylim = c(0, 35)) + 
  scale_y_continuous(breaks = seq(0, 35, 5), expand = c(0, 0))

# normal plot
p + add_pvalue(df_p_val, y.position = 30)

## ---- fig.height=3.5----------------------------------------------------------
df_p_val <- rstatix::t_test(ToothGrowth, len ~ dose, ref.group = "0.5") %>% 
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  stat_summary(geom = "col", fun = mean) + 
  stat_summary(geom = "errorbar", 
               fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.3) + 
  theme_prism() + 
  coord_cartesian(ylim = c(0, 40)) + 
  scale_y_continuous(breaks = seq(0, 40, 5), expand = c(0, 0))

# with brackets
p1 <- p + add_pvalue(df_p_val, label = "p.adj.signif")

# without brackets
p2 <- p + add_pvalue(df_p_val, label = "p.adj.signif", remove.bracket = TRUE)

p1 + p2

## ---- fig.width=4, fig.height=3.5---------------------------------------------
df_p_val <- rstatix::t_test(ToothGrowth, len ~ dose, ref.group = "all") %>% 
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  stat_summary(geom = "col", fun = mean) + 
  stat_summary(geom = "errorbar", 
               fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.3) + 
  theme_prism() + 
  coord_cartesian(ylim = c(0, 40)) + 
  scale_y_continuous(breaks = seq(0, 40, 5), expand = c(0, 0))

p + add_pvalue(df_p_val, label = "p.adj.signif")

## ---- fig.width=4, fig.height=3.5---------------------------------------------
df_p_val <- ToothGrowth %>% 
  rstatix::group_by(factor(dose)) %>% 
  rstatix::t_test(len ~ 1, mu = 26) %>% 
  rstatix::adjust_pvalue(p.col = "p", method = "holm") %>% 
  rstatix::add_significance(p.col = "p.adj")

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  stat_summary(geom = "col", fun = mean) + 
  stat_summary(geom = "errorbar", 
               fun = mean,
               fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               width = 0.3) + 
  theme_prism() + 
  coord_cartesian(ylim = c(0, 40)) + 
  scale_y_continuous(breaks = seq(0, 40, 5), expand = c(0, 0))

# remember xmin and x are referring to the column dames in df_p_val
p + add_pvalue(df_p_val, 
               xmin = "group1", 
               x = "factor(dose)", 
               y = 37,
               label = "p.adj.signif")

## ---- fig.width=4, fig.height=3.5---------------------------------------------
df_p_val <- rstatix::t_test(ToothGrowth, len ~ dose)

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_violin(trim = FALSE) + 
  geom_boxplot(width = 0.2) +
  theme_prism() + 
  coord_cartesian(ylim = c(0, 45)) + 
  scale_y_continuous(breaks = seq(0, 45, 5), expand = c(0, 0))

p + add_pvalue(df_p_val, 
               y.position = c(44, 41, 44),
               bracket.shorten = c(0.025, 0, 0.025))

## ---- fig.width=5, fig.height=3.5---------------------------------------------
df_p_val <- ToothGrowth %>% 
  rstatix::group_by(supp) %>% 
  rstatix::t_test(len ~ dose) %>% 
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_boxplot(aes(fill = supp)) + 
  theme_prism()

# remember colour and step.group.by are referring to a column name in df_p_val
p + add_pvalue(df_p_val, 
               label = "p = {p.adj}",
               colour = "supp",
               fontface = "bold",
               step.group.by = "supp",
               step.increase = 0.1,
               tip.length = 0,
               bracket.colour = "black",
               show.legend = FALSE)

## ---- fig.width=5, fig.height=3.5---------------------------------------------
df_p_val <- ToothGrowth %>%
  rstatix::group_by(dose) %>%
  rstatix::t_test(len ~ supp) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>% 
  rstatix::add_xy_position(x = "dose", dodge = 0.8) # important for positioning!

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_boxplot(aes(fill = supp)) + 
  theme_prism() + 
  coord_cartesian(ylim = c(0, 40))

p + add_pvalue(df_p_val, 
               xmin = "xmin", 
               xmax = "xmax",
               label = "p = {p.adj}",
               tip.length = 0)

## ---- fig.width=5, fig.height=4-----------------------------------------------
df_p_val1 <- ToothGrowth %>%
  rstatix::group_by(dose) %>%
  rstatix::t_test(len ~ supp) %>%
  rstatix::adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  rstatix::add_significance(p.col = "p.adj") %>% 
  rstatix::add_xy_position(x = "dose", dodge = 0.8) # important for positioning!

df_p_val2 <- rstatix::t_test(ToothGrowth, len ~ dose, 
                             p.adjust.method = "bonferroni") %>% 
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_boxplot(aes(fill = supp)) + 
  theme_prism() + 
  coord_cartesian(ylim = c(0, 45))

p + add_pvalue(df_p_val1, 
               xmin = "xmin", 
               xmax = "xmax",
               label = "p = {p.adj}",
               tip.length = 0) + 
  add_pvalue(df_p_val2,
             label = "p = {p.adj}",
             tip.length = 0.01,
             bracket.nudge.y = 2,
             step.increase = 0.015)

## ---- fig.height=3.5----------------------------------------------------------
df_p_val <- ToothGrowth %>% 
  rstatix::group_by(dose) %>% 
  rstatix::t_test(len ~ supp) %>% 
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(supp), y = len)) + 
  geom_boxplot(width = 0.2) + 
  facet_wrap(
    ~ dose, scales = "free", 
    labeller = labeller(dose = function(x) paste("dose =", x))
) + 
  theme_prism()

p + add_pvalue(df_p_val)

## ---- fig.height=3.5----------------------------------------------------------
df_p_val <- ToothGrowth %>% 
  rstatix::group_by(supp) %>% 
  rstatix::t_test(len ~ dose) %>% 
  rstatix::add_xy_position()

p <- ggplot(ToothGrowth, aes(x = factor(dose), y = len)) + 
  geom_boxplot(width = 0.4) + 
  facet_wrap(~ supp, scales = "free") + 
  theme_prism()

p + add_pvalue(df_p_val)

## ---- fig.height=7------------------------------------------------------------
# add a grouping variable to ToothGrowth
tg <- ToothGrowth
tg$dose <- factor(tg$dose)
tg$grp <- factor(rep(c("grp1", "grp2"), 30))

# construct the p-value table by hand
df_p_val <- data.frame(
  group1 = c("OJ", "OJ"),
  group2 = c("VC", "VC"),
  p.adj = c(0.0449, 0.00265),
  y.position = c(22, 27),
  grp = c("grp1", "grp2"),
  dose = c("0.5", "1")
)

p <- ggplot(tg, aes(x = factor(supp), y = len)) + 
  geom_boxplot(width = 0.4) + 
  facet_wrap(grp ~ dose, scales = "free") + 
  theme_prism()

p + add_pvalue(df_p_val, bracket.nudge.y = 3)

