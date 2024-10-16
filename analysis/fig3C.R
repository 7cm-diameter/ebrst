library(tidyverse)
library(ggcolors)
library(utexr)
library(lme4)
library(emmeans)
library(lmerTest)

#########################################
# Define ggplot's theme and figure size #
#########################################

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
  split(., list(.$subject, .$condition, .$session), drop = T) %>%
  lapply(., function(d) {
    reward_pulse <- d %>%
      filter(phase == REWARDPHASE) %>%
      summarise(empirical = sum(pulse) / (nrow(.)), prediction = mean(p_qhcm))
    extinction_pulse <- d %>%
      filter(phase == EXTINCTIONPHASE) %>%
      summarise(empirical = sum(pulse) / (nrow(.)), prediction = mean(p_qhcm))
    d %>%
      head(., 1) %>%
      select(subject, condition, session) %>%
      mutate(empirical = extinction_pulse$empirical - reward_pulse$empirical,
             prediction = extinction_pulse$prediction - reward_pulse$prediction)
  }) %>%
  do.call(rbind, .)

####################
# Visualize result #
####################

dev.new(width = 5., height = 5., unit ="cm")

ggplot(pulse_ratio) +
  stat_summary(fun = "mean", geom = "line",
               aes(x = condition, y = prediction, group = interaction(subject)),
               alpha = 0.25) +
  stat_summary(fun = "mean", geom = "point",
               aes(x = condition, y = prediction,  group = interaction(subject), color = condition)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar",
               aes(x = condition, y = prediction),
               width = 0.2, linewidth = .75) +
  stat_summary(fun = "mean", geom = "bar",
               aes(x = condition, y = prediction, fill = condition),
               width = 0.5, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  scale_x_discrete(labels = c("0.25", "0.33", "0.50", "1.00"), limits = rev) +
  theme_classic() +
  theme(aspect.ratio = 2,
        legend.position = "none") +
  null_theme

ggsave("./figs/fig3C.jpg", dpi = 300)

########################
# Statistical analysis #
########################

m <- lmer(prediction~condition+(1|subject)+(1|session), pulse_ratio)
anova(m) %>% as.data.frame %>% write.csv(., "stats/fig3C-anova.csv")
emmeans(m, ~condition) %>%
  pairs %>%
  as.data.frame %>%
  write.csv(., "stats/fig3C-multicomp.csv", row.names = F)
