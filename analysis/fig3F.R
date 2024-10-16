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

fitted_data <- list.files("./data", pattern = "subses_fitted", full.names = T) %>%
  lapply(., read.csv) %>%
  do.call(rbind, .)
LEVER <- 6
REWARDPHASE <- 200
EXTINCTIONPHASE <- -REWARDPHASE

######################
# Preprocess rawdata #
######################

response_per_sec <- fitted_data %>%
  filter(phase == REWARDPHASE) %>%
  split(., list(.$subject, .$condition, .$session), drop = T) %>%
  lapply(., function(d) {
    sub <- unique(d$subject)
    ses <- unique(d$session)
    w <- params_data %>%
      filter(subject == sub, session == ses) %>%
      (function(d) d[d$parameters == "w", "mean"])
    d %>%
      mutate(empirical = mean(1 / IRT), prediction = mean((1 - w) * q_qhcm + w * v_qhcm)) %>%
      head(., 1)
  }) %>%
  do.call(rbind, .)

####################
# Visualize result #
####################

dev.new(width = 5., height = 5., unit ="cm")

ggplot(response_per_sec) +
  geom_smooth(aes(x = empirical, y = prediction),
              method = "lm", color = "black", linetype = "dashed") +
  geom_point(aes(x = empirical, y = prediction, color = condition)) +
  theme_classic() +
  theme(aspect.ratio = 1) +
  null_theme

ggsave("./figs/fig3F.jpg", dpi = 300)

########################
# Statistical analysis #
########################

m <- lm(empirical~prediction, response_per_sec)
sink("stats/fig3F-lm.txt")
summary(m)
cor.test(response_per_sec$empirical, response_per_sec$prediction)
sink()
