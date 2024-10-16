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
                    plot.background = element_rect(fill='transparent', color=NA),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

####################################
# Read rawdata and define event id #
####################################

fitted_data <- list.files("./data", pattern = "subses_fitted", full.names = T) %>%
  lapply(., read.csv) %>%
  do.call(rbind, .)
LEVER <- 6
REWARDPHASE <- 200
EXTINCTIONPHASE <- -REWARDPHASE

######################
# Preprocess rawdata #
######################

pulse_ratio <- fitted_data %>%
  split(., list(.$subject, .$condition), drop = T) %>%
  lapply(., function(d) {
    reward_pulse <- d %>%
      filter(phase == REWARDPHASE) %>%
      summarise(empirical = sum(pulse) / (nrow(.)), prediction = mean(p_qhcm))
    extinction_pulse <- d %>%
      filter(phase == EXTINCTIONPHASE) %>%
      summarise(empirical = sum(pulse) / (nrow(.)), prediction = mean(p_qhcm))
    d %>%
      head(., 1) %>%
      select(subject, condition) %>%
      mutate(empirical = extinction_pulse$empirical - reward_pulse$empirical,
             prediction = extinction_pulse$prediction - reward_pulse$prediction)
  }) %>%
  do.call(rbind, .)

####################
# Visualize result #
####################

dev.new(width = 5., height = 5., unit ="cm")

ggplot(pulse_ratio) +
  geom_abline(slope = 1, intercept = 0, linewidth = 1) +
  geom_point(aes(x = empirical, y = prediction, color = condition, group = interaction(subject)),
             size = 3) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  null_theme

ggsave("./figs/fig3B.jpg", dpi = 300)

########################
# Statistical analysis #
########################

m <- lm(empirical~prediction, pulse_ratio)
sink("stats/fig3B-lm.txt")
cor.test(pulse_ratio$empirical, pulse_ratio$prediction)
summary(m)
sink()
