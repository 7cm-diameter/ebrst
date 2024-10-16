library(tidyverse)
library(ggcolors)
library(utexr)

#########################
# Define ggplot's theme #
#########################

null_theme <- theme(axis.text = element_text(color = "black", size = 15),
                    axis.title = element_text(color = "transparent"),
                    axis.ticks.length = unit(-0.1, "cm"),
                    legend.text = element_text(color = "transparent"),
                    legend.title = element_text(color = "transparent"),
                    strip.text = element_text(color = "transparent"),
                    strip.background = element_rect(color = "transparent", fill = "transparent"),
                    panel.background = element_rect(fill='transparent'),
                    panel.spacing=unit(1, "cm"),
                    plot.background = element_rect(fill='transparent', color=NA),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

####################################
# Read rawdata and define event id #
####################################

rawdata <- read.csv("./data/merged_data.csv")
LEVER <- 6
REWARDPHASE <- 200
EXTINCTIONPHASE <- -REWARDPHASE

######################
# Preprocess rawdata #
######################

example <- rawdata %>%
  filter(subject == "WTY02G", date == 220218) %>%
  # filter(condition == "VR1-ext") %>%
  filter(event %in% c(6, 200)) %>%
  mutate(bin = round(time))

response_count <- seq_len(max(example$bin)) %>%
  lapply(., function(t) {
    n <- example %>% filter(bin == t) %>% nrow
    data.frame(time = t, response = n)
  }) %>%
  do.call(rbind, .)

####################
# Visualize result #
####################

dev.new(width = 7., height = 7., unit ="cm")

ggplot() +
  geom_point(data = example %>% filter(event == 6),
             aes(x = time, y = -0.5),
             shape = 108, size = 6, alpha = 0.5) +
  geom_point(data = example %>% filter(event == 200),
             aes(x = time, y = 3.),
             shape = 108, size = 6, color = "#0BB9C2", alpha = 0.5) +
  stat_summary(data = response_count %>% mutate(time = bining(time, 10)),
               fun = "mean", geom = "line",
               aes(x = time, y = response)) +
  theme_classic() +
  facet_wrap(~subject+date) +
  theme(aspect.ratio = 0.5) +
  null_theme

ggsave("./figs/fig2C.jpg", dpi = 300)
