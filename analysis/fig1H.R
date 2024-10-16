library(tidyverse)
library(viridis)
library(ggcolors)

null_theme <- theme(axis.text = element_text(color = "black", size = 15),
                    axis.title = element_text(color = "transparent"),
                    axis.ticks.length = unit(-0.1, "cm"),
                    legend.text = element_text(color = "transparent"),
                    legend.title = element_text(color = "transparent"),
                    strip.text = element_text(color = "transparent"),
                    strip.background = element_rect(color = "transparent", fill = "transparent"),
                    panel.background = element_rect(fill='transparent'),
                    plot.background = element_rect(fill='transparent', color=NA),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.spacing = unit(1., "cm"))



baseline <- read.csv("./data/fig1H.csv")

dev.new(width = 7., height = 7., unit ="cm")

ggplot(baseline %>% filter(alpha.n == 0.01, alpha.p == 0.1)) +
  geom_line(aes(x = p, y = q, color = "red"),
            linewidth = 1) +
  geom_line(aes(x = p, y = epsilon, color = "blue"),
            linewidth = 1) +
  geom_line(aes(x = p, y = q + epsilon, color = "magenta"),
            linewidth = 1) +
  facet_wrap(~alpha.p) +
  scale_fill_viridis() +
  theme_classic() +
  theme(aspect.ratio = 1) +
  scale_x_continuous(limits = c(0.25, 1.), breaks = c(0.25, 0.5, 0.75, 1.0)) +
  iceberg_dark_color_with_name() +
  null_theme

ggsave("./figs/fig1H-1.jpg", dpi = 300)

dev.new(width = 4, height = 4, unit ="cm")

ggplot(baseline %>% filter(alpha.n == 0.01, alpha.p == 0.1)) +
  geom_line(aes(x = p, y = q, color = "red"),
            linewidth = 1) +
  geom_line(aes(x = p, y = epsilon, color = "blue"),
            linewidth = 1) +
  geom_line(aes(x = p, y = response, color = "magenta"),
            linewidth = 1) +
  facet_wrap(~alpha.p) +
  scale_fill_viridis() +
  theme_classic() +
  theme(aspect.ratio = 1) +
  scale_x_continuous(breaks = c(0., 0.25, 0.5, 0.75, 1.0)) +
  iceberg_dark_color_with_name() +
  null_theme

ggsave("./figs/fig1H-2.jpg", dpi = 300)
