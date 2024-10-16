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
                    panel.spacing = unit(1, "cm"),
                    plot.background = element_rect(fill='transparent', color=NA),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())


dev.new(width = 5, height = 7., unit ="cm")

qhcm_data <- read.csv("./data/fig1F_QHCM.csv") %>%
  mutate(time = seq_len(nrow(.))

vqm_data <- read.csv("./data/fig1F_VQM") %>%
  mutate(time = seq_len(nrow(.))) %>%
  mutate(epsilon = 0, r = q)

ggplot(rbind(qhcm_data, vqm_data), aes(x = time)) +
  geom_vline(xintercept = 200, linetype = "dashed", linewidth = 1) +
  geom_line(aes(y = q, color = "red"), size = 1) +
  geom_line(aes(y = epsilon, color = "blue"), size = 1) +
  geom_line(aes(y = r, color = "magenta"), size = 1) +
  theme_classic() +
  theme(aspect.ratio = 0.75) +
  null_theme +
  iceberg_dark_color_with_name() +
  facet_wrap(~model, nrow = 2)

ggsave("./figs/fig1F.jpg", dpi = 300)
