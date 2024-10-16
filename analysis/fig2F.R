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

around_extinction <- rawdata %>%
 split(., list(.$subject, .$date), drop = T) %>%
  lapply(., function(d) {
    d %>%
      align_with(., "event", EXTINCTIONPHASE, "time", -60, 60) %>%
      filter(event == LEVER)
  }) %>%
  do.call(rbind, .)

response_per_sec_around <- around_extinction %>%
  mutate(time = bining(time, 5)) %>%
  group_by(subject, condition, time, phase) %>%
  summarise(response = 1 / mean(IRT))

####################
# Visualize result #
####################

dev.new(width = 7., height = 7., unit ="cm")

ggplot(response_per_sec_around) +
  stat_summary(fun = "mean", geom = "line",
               aes(x = time, y = response, color = as.factor(phase), group = interaction(subject )),
               linewidth = .5, alpha = 0.25) +
  stat_summary(fun.data = "mean_se", geom = "ribbon",
               aes(x = time, y = response, fill = as.factor(phase)),
               alpha = 0.5) +
  stat_summary(fun = "mean", geom = "line",
               aes(x = time, y = response, color = as.factor(phase), group = 1),
               linewidth = 1.5) +
  stat_summary(fun = "mean", geom = "point",
               aes(x = time, y = response, color = as.factor(phase), group = 1),
               size = 1.) +
  facet_wrap(~condition) +
  theme_classic() +
  null_theme +
  theme(aspect.ratio = 0.5)

ggsave("./figs/fig2F.jpg", dpi = 300)
