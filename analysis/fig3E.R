library(tidyverse)
library(ggcolors)
library(utexr)
library(lme4)
library(emmeans)
library(lmerTest)

#########################################
# Define ggplot's theme and figure size #
#########################################

null_theme <- theme(axis.text = element_text(color = "black", size = 12.5),
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

params_data <- list.files("./data", pattern = "subses_params", full.names = T) %>%
  lapply(., read.csv) %>%
  do.call(rbind, .)

fitted_data <- list.files("./data", pattern = "subses_fitted", full.names = T) %>%
  lapply(., read.csv) %>%
  do.call(rbind, .)

LEVER <- 6
REWARDPHASE <- 200
EXTINCTIONPHASE <- -REWARDPHASE

######################
# Preprocess rawdata #
######################

learning_ratio_data <- params_data %>%
  split(., list(.$subject, .$session), drop = T) %>%
  lapply(., function(d) {
    d %>%
      head(., 1) %>%
      mutate(learning.ratio = log10(d[d$parameter == "αqp", ]$mean / d[d$parameter == "αqn", ]$mean))
  }) %>%
  do.call(rbind, .)

learning_ratio_pulse_data <- learning_ratio_data %>%
  split(., list(.$subject, .$session), drop = T) %>%
  lapply(., function(d) {
    sub <- unique(d$subject)
    ses <- unique(d$session)
    pulse <- fitted_data %>%
      filter(subject == sub, session == ses) %>%
      (function(d) {
        reward_pulse <- d %>%
          filter(phase == REWARDPHASE) %>%
          (function(d) sum(d$pulse) / nrow(d))
        extinction_pulse <- d %>%
          filter(phase == EXTINCTIONPHASE) %>%
          (function(d) sum(d$pulse) / nrow(d))
        extinction_pulse - reward_pulse
      })
    d %>%
      mutate(pulse.ratio = pulse)
  }) %>%
  do.call(rbind, .)

####################
# Visualize result #
####################

dev.new(width = 5., height = 5., unit ="cm")

ggplot(learning_ratio_pulse_data) +
  geom_smooth(aes(x = learning.ratio, y = pulse.ratio),
              method = "lm", color = "black", linetype = "dashed") +
  geom_point(aes(x = learning.ratio, y = pulse.ratio, color = condition)) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  null_theme

ggsave("./figs/fig3E.jpg", dpi = 300)

########################
# Statistical analysis #
########################

# m <- lmer(pulse.ratio~log10(learning.ratio)+(1|subject)+(1|session), learning_ratio_pulse_data)
m <- lm(pulse.ratio~learning.ratio, learning_ratio_pulse_data)
sink("stats/fig3E-lm.txt")
summary(m)
cor.test(learning_ratio_pulse_data$pulse.ratio, learning_ratio_pulse_data$learning.ratio)
sink()
