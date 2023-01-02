#Lecture 1 different vizualizations
## reused from Klaus Wilke's book

library(tidyverse)
library(ggplot2)
library(cowplot)
library(viridis)
library(RColorBrewer)
library(colorspace)
library(ggforce)

# theme
theme_plot_icon <- function(bg_color = "#F5F8EA", line_color = "#243400",
                            line_size = .5, font_size = 14) {
  theme(
    axis.text.x       = element_blank(),
    axis.text.y       = element_blank(),
    axis.title.x      = element_blank(),
    axis.title.y      = element_blank(),
    #axis.line.x       = element_blank(),
    #axis.line.y       = element_blank(),
    #axis.ticks        = element_blank(),
    axis.line.x       = element_line(size = line_size, color = line_color),
    axis.line.y       = element_line(size = line_size, color = line_color),
    axis.ticks        = element_line(size = line_size, color = line_color),
    axis.ticks.length = grid::unit(4, "pt"),
    legend.position   = "none",
    plot.margin       = margin(
      font_size*8/14, font_size, font_size*10/14, font_size
    ),
    plot.title        = element_text(
      hjust = 0.5,
      #family = dviz_font_family_bold,
      family = dviz_font_family_condensed,
      color = line_color,
      size = font_size,
      margin = margin(0, 0, font_size*6/14, 0)
    ),
    plot.background   = element_rect(fill = bg_color)
  )
}
theme_plot_icon_hgrid <- function(bg_color = "#F5F8EA", line_color = "#243400",
                                  line_size = .5, font_size = 14) {
  theme_plot_icon(bg_color, line_color, line_size, font_size) %+replace% theme(
    # make grid lines
    #panel.grid.major.y   = element_line(colour = paste0(line_color, "30"),
    #                                    size = 0.5),
    # remove x axis
    axis.ticks.x        = element_blank(),
    axis.line.x         = element_blank()
  )
}
theme_plot_icon_vgrid <- function(bg_color = "#F5F8EA", line_color = "#243400",
                                  line_size = .5, font_size = 14) {
  theme_plot_icon(bg_color, line_color, line_size, font_size) %+replace% theme(
    # make grid lines
    #panel.grid.major.x   = element_line(colour = paste0(line_color, "30"),
    #                                    size = 0.5),
    # remove y axis
    axis.ticks.y        = element_blank(),
    axis.line.y         = element_blank()
  )
}
theme_plot_icon_blank <- function(bg_color = "#F5F8EA", line_color = "#243400",
                                  line_size = .5, font_size = 14) {
  theme_plot_icon(bg_color, line_color, line_size, font_size) %+replace% theme(
    axis.ticks          = element_blank(),
    axis.line.x         = element_blank(),
    axis.line.y         = element_blank(),
    axis.ticks.length    = grid::unit(0, "pt")
  )
}
# data sets
set.seed(5142)
n <- 15
x <- rnorm(n)
y <- .4*x + .6*rnorm(n)
df_scatter_xy <- data.frame(x, y)
df_one_dist <- data.frame(x = c(rnorm(1000, 1., 1.6), rnorm(300, 4, .4)))
df_one_normal <- data.frame(x = rnorm(20))
df_fractions <- data.frame(y = c(.3, .39, .48, .6, .25, .13, .22, .24, .45, .48, .3, .16),
                           x = factor(rep(1:4, 3)),
                           type = rep(c("A", "B", "C"), each = 4))
set.seed(2474)
n <- 8
x <- rnorm(n)
y <- .4*x + .6*rnorm(n)
z <- .5*x + .3*rnorm(n)
z <- (z - min(z) + 0.1)^2
df_scatter_xyz <- data.frame(x, y, z)
set.seed(5012)
df_multi_amounts <- mutate(df_fractions,
                           y = c(1.0, 1.1, 1.4, 1.2)[x]*y)
n <- 70
df_multi_dist <- data.frame(y = c(rnorm(n, 1, .8), rnorm(n, 2, .7), rnorm(n, 0, .5)),
                            type = rep(c("A", "B", "C"), each = n),
                            number = rep(c(2, 1, 3), each = n))
df_props = data.frame(value = c(55, 30, 15),
                      group = c("A", "B", "C"))
df_multi_props <- data.frame(
  var1 = rep(c("C", "B", "A"), 3),
  var2 = rep(c("A", "B", "C"), each = 3),
  count = c(4, 1, 2, 12, 9, 5, 4, 5, 4)
) %>% group_by(var2) %>%
  mutate(group_count = sum(count))
df_multi_props2 <- data.frame(
  var1 = rep(c("B", "A"), 9),
  var2 = rep(c("E", "E", "D", "D", "C", "C"), 3),
  var3 = rep(c("H", "G", "F"), each = 6),
  count = c(5, 8, 0, 0, 0, 0, 0, 3, 2, 7, 0, 0, 4, 0, 4, 2, 7, 4)
)
df_sets <- gather_set_data(df_multi_props2, 1:3)
df_one_line <- data.frame(
  x = 1:5,
  y = c(3.1, 3.3, 4.0, 3.8, 4.4)
)
set.seed(9681)
n1 <- 1500/5
n2 <- 800/5
x1 <- rnorm(n1, 0, .7)
y1 <- 2 * x1 + rnorm(n1, 0, .8)
x2 <- rnorm(n2, 0, 0.4)
y2 <- 1.5 * x2 + rnorm(n2, .5, .8)
df_dense_scatter <- na.omit(
  data.frame(
    x = scales::censor(c(x1, x2 + 2.2), c(-2, 4)),
    y = scales::censor(c(y1, y2 + 1.5), c(-3.5, 4.5))
  )
)
y1 <- 2 * x1 + rnorm(n1, 0, 1.6)
y2 <- 1.5 * x2 + rnorm(n2, .5, 1.6)
df_dense_scatter_sample <- na.omit(
  data.frame(
    x = scales::censor(c(x1, x2 + 2.2), c(-2, 4)),
    y = scales::censor(c(y1, y2 + 1.5), c(-3.5, 4.5))
  )
) %>% sample_n(50)
df_connected_scatter <- data.frame(
  x = c(1.9, 1.5, 2.2, 3, 3.3, 2.7, 1.7, 1),
  y = c(0.3, -1, -2.0, -0.9, .6, 1.8, 2, 0.7),
  t = 1:8
)
df_paired <- data.frame(
  y = c(6, 5.3, 3.8, 2.8, 2,
        4.3, 6.1, 5.1, 3.3, 2.4),
  x = rep(c("A", "B"), each = 5),
  group = rep(1:5, 2)
)
df_uncertain <- data.frame(
  type = c("A", "B", "C"),
  x = c(1.5, 2.2, 3.4),
  y = c(3.2, 5.1, 3.9),
  dx = c(.25, .3, .35),
  dy = c(.5, .4, .6)
)
# palettes
npal <- 5
# earth-brown (Amounts)
pal_earth_brown <- sequential_hcl(n = npal, h1 = 71, c1 = 80, c2 = 10, l1 = 18, l2 = 97, p1 = 1.5)
# brown-green (Proportions)
pal_brown_green <- sequential_hcl(n = npal, h1 = 86, c1 = 80, c2 = 10, l1 = 18, l2 = 97, p1 = 1.5)
# green-brown (Geospatial data)
pal_green_brown <- sequential_hcl(n = npal, h1 = -265, c1 = 80, c2 = 10, l1 = 18, l2 = 97, p1 = 1.5)
# burgundy-red
pal_red_brown <- sequential_hcl(n = npal, h1 = 28, c1 = 80, c2 = 10, l1 = 18, l2 = 97, p1 = 1.5)
# brown-red (Uncertainty)
pal_brown_red <- sequential_hcl(n = npal, h1 = 41, c1 = 80, c2 = 10, l1 = 18, l2 = 97, p1 = 1.5)
# ocean-blue (Distributions)
pal_ocean_blue <- sequential_hcl(n = npal, h1 = 241, c1 = 80, c2 = 10, l1 = 18, l2 = 97, p1 = 1.5)
# steel-blue (x-y relationships)
pal_steel_blue <- sequential_hcl(n = npal, h1 = 257, c1 = 80, c2 = 10, l1 = 18, l2 = 97, p1 = 1.5)
pal_steel_blue_inv <- sequential_hcl(n = npal, h1 = 257-180, c1 = 80, c2 = 10, l1 = 18, l2 = 97, p1 = 1.5)


#Amounts
palette <- pal_earth_brown

p1 <- ggplot(df_props, aes(x = group, y = value)) +
  geom_col(
    position="identity", color = palette[npal],
    fill = palette[3], width = 0.8
  ) +
  scale_y_continuous(limits = c(0, 66), expand = c(0, 0)) +
  scale_fill_manual(values = palette[2:4]) +
  labs(title = "Bars") +
  theme_plot_icon_hgrid(palette[npal], palette[1])
p2 <- ggplot(df_props, aes(x = fct_rev(group), y = value)) +
  geom_col(position="identity", color = palette[npal], fill = palette[3],
           width = .8) +
  scale_y_continuous(limits = c(0, 66), expand = c(0, 0)) +
  scale_fill_manual(values = palette[2:4]) +
  coord_flip() +
  labs(title = "Bars") +
  theme_plot_icon_vgrid(palette[npal], palette[1])
p3 <- ggplot(filter(df_multi_amounts, x!=4), aes(x, y,
                                                 fill=factor(type, levels = c("A", "C", "B")))) +
  geom_col(position="dodge", color = palette[npal],
           width = .7) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, .7)) +
  scale_fill_manual(values = palette[2:4]) +
  labs(title = "Grouped Bars") +
  theme_plot_icon_hgrid(palette[npal], palette[1])
p4 <- ggplot(filter(df_multi_amounts, x!=4), aes(x, y,
                                                 fill=factor(type, levels = c("B", "C", "A")))) +
  geom_col(position="dodge", color = palette[npal],
           width = .7) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, .7)) +
  scale_fill_manual(values = rev(palette[2:4])) +
  coord_flip() +
  labs(title = "Grouped Bars") +
  theme_plot_icon_vgrid(palette[npal], palette[1])
p5 <- ggplot(df_multi_amounts, aes(x, y, fill=factor(type, levels = c("B", "C", "A")))) +
  geom_col(position="stack", color = palette[npal]) +
  scale_y_continuous(limits = c(0, 1.55),
                     expand = c(0, 0)) +
  scale_fill_manual(values = rev(palette[2:4])) +
  labs(title = "Stacked Bars") +
  theme_plot_icon_hgrid(palette[npal], palette[1])
p6 <- p5 + coord_flip() + theme_plot_icon_vgrid(palette[npal], palette[1])
p7 <- ggplot(df_props, aes(x = fct_rev(group), y = value)) +
  geom_point(color = palette[2], size = 2) +
  scale_y_continuous(limits = c(0, 66), expand = c(0, 0)) +
  coord_flip() +
  labs(title = "Dots") +
  theme_plot_icon_vgrid(palette[npal], palette[1])
p8 <- ggplot(filter(df_multi_amounts, x != 1), aes(x, y = factor(type, levels = c("A", "C", "B")), fill = y)) +
  geom_tile(color = palette[5], size = 1.5) +
  scale_fill_continuous_sequential(
    h1 = 71, c1 = 80, c2 = 10, l1 = 18, l2 = 97, p1 = 1.5,
    begin = 0.2, end = 0.75,
    rev = FALSE
  ) +
  labs(title = "Heatmap") +
  theme_plot_icon_blank(palette[npal], palette[1])
plot_grid(p1, p2, p7, ncol = 4, scale = .9)
