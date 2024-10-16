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
                    panel.background = element_rect(fill='transparent'),
                    plot.background = element_rect(fill='transparent', color=NA),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())

####################################
# Read rawdata and define event id #
####################################

waic_data <- list.files("./data", pattern = "subses_waic", full.names = T) %>%
  lapply(., read.csv) %>%
  do.call(rbind, .)

######################
# Preprocess rawdata #
######################

delta_waic_data <- waic_data %>%
  split(., list(.$subject, .$session), drop = T) %>%
  lapply(., function(d) {
    q_hcm <- d %>% filter(model == "Q-HCM") %>% (function(d) d$waic)
    vqm <- d %>% filter(model == "VQM") %>% (function(d) d$waic)
    d %>%
      select(subject, session, condition) %>%
      head(., 1) %>%
      mutate(delta_waic = vqm - q_hcm)
  }) %>%
  do.call(rbind, .)

####################
# Visualize result #
####################

dev.new(width = 5., height = 5., unit ="cm")

ggplot(delta_waic_data) +
  geom_point(aes(x = condition, y = delta_waic, color = condition)) +
  stat_summary(fun = "mean", geom = "bar",
               aes(x = condition, y = delta_waic, fill = condition),
               alpha = 0.5, linewidth = 1, width = 0.5) +
  stat_summary(fun.data = "mean_se", geom = "errorbar",
               aes(x = condition, y = delta_waic),
               color = "black", linewidth = .75, width = 0.25) +
  theme_classic() +
  theme(aspect.ratio = 2.) +
  scale_x_discrete(labels = c("0.25", "0.33", "0.50", "1.00"), limits = rev) +
  null_theme

ggsave("./figs/fig3A.jpg", dpi = 300)

########################
# Statistical analysis #
########################

# NO STAT
