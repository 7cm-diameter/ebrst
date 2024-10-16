library(tidyverse)
library(ggcolors)
library(utexr)
library(lme4)
library(emmeans)
library(lmerTest)

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
                    panel.spacing = unit(1, "cm"),
                    panel.background = element_rect(fill='transparent'),
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

pulse_data <- rawdata %>%
  mutate(local.response.rate = 1 / IRT) %>%
  split(., list(.$subject, .$date), drop = T) %>%
  lapply(., function(d) {
    threshold <- d %>%
      filter(phase == REWARDPHASE) %>%
      (function(d) quantile(d$local.response.rate, 0.95, na.rm = T))
    d %>%
      mutate(pulse = as.numeric(local.response.rate >= threshold))
  }) %>%
  do.call(rbind, .)

around_extinction <- pulse_data %>%
  split(., list(.$subject, .$date), drop = T) %>%
  lapply(., function(d) {
    d %>%
      align_with(., "event", EXTINCTIONPHASE, "time", -60, 60) %>%
      filter(event == LEVER)
  }) %>%
  do.call(rbind, .) %>%
  mutate(time = bining(time, 2)) %>%
  group_by(subject, condition, time, phase) %>%
  summarise(count = sum(pulse))

####################
# Visualize result #
####################

dev.new(width = 4., height = 4., unit ="cm")

ggplot(around_extinction) +
  stat_summary(fun.data = "mean_se", geom = "errorbar",
               aes(x = time, y = count),
               linewidth = 0.5, width = 0.5) +
  stat_summary(fun = "mean", geom = "point",
               aes(x = time, y = count, color = as.factor(phase), group = 1),
               size = 1) +
  stat_summary(fun = "mean", geom = "line",
               aes(x = time, y = count, color = as.factor(phase), group = 1),
               linewidth = 0.5, alpha = 0.5) +
  stat_summary(fun = "mean", geom = "point",
               aes(x = time, y = count, color = as.factor(phase), group = interaction(subject)),
               size = 0.25, alpha = 0.25) +
  facet_wrap(~condition) +
  coord_cartesian(ylim=c(0, 15)) +
  theme_classic() +
  null_theme +
  theme(aspect.ratio = 0.5, legend.position = "none")

ggsave("./figs/fig2L.jpg", dpi = 300)

########################
# Statistical analysis #
########################

# NO STAT
