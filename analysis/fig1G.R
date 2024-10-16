library(tidyverse)
library(viridis)


null_theme <- theme(axis.text = element_text(color = "black", size = 15),
                    axis.title = element_text(color = "transparent"),
                    axis.ticks.length = unit(-0.1, "cm"),
                    legend.text = element_text(color = "black", 10.),
                    legend.title = element_text(color = "transparent"),
                    strip.text = element_text(color = "transparent"),
                    strip.background = element_rect(color = "transparent", fill = "transparent"),
                    panel.background = element_rect(fill='transparent'),
                    plot.background = element_rect(fill='transparent', color=NA),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())


dev.new(width = 7., height = 7., unit ="cm")

burstdata <- read.csv("./data/fig1G.csv")

ggplot(burstdata %>% filter(alpha.n == 0.1)) +
  geom_tile(aes(x = p, y = alpha.p, fill = burst, color = burst),
            linewidth = 3) +
  scale_fill_viridis() +
  scale_color_viridis() +
  theme_classic() +
  theme(aspect.ratio = 1) +
  null_theme

ggsave("./figs/fig1G.jpg", dpi = 300)
