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

pulse_ratio <- pulse_data %>%
  filter(event == LEVER) %>%
  split(., list(.$subject, .$date, .$condition), drop = T) %>%
  lapply(., function(d) {
    reward_pulse <- d %>%
      filter(phase == REWARDPHASE) %>%
      (function(d) sum(d$pulse) / nrow(d))
    extinction_pulse <- d %>%
      filter(phase == EXTINCTIONPHASE) %>%
      (function(d) sum(d$pulse) / nrow(d))
    d %>%
      head(., 1) %>%
      select(subject, condition, date) %>%
      mutate(ratio = extinction_pulse - reward_pulse)
  }) %>%
  do.call(rbind, .)

####################
# Visualize result #
####################

dev.new(width = 4., height = 4., unit ="cm")

ggplot(pulse_ratio) +
  stat_summary(fun = "mean", geom = "line",
               aes(x = condition, y = ratio, group = subject),
               alpha = 0.25) +
  stat_summary(fun = "mean", geom = "point",
               aes(x = condition, y = ratio, group = subject, color = condition)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar",
               aes(x = condition, y = ratio),
               width = 0.1) +
  stat_summary(fun = "mean", geom = "bar",
               aes(x = condition, y = ratio, fill = condition),
               width = .5, alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  scale_x_discrete(labels = c("0.25", "0.33", "0.50", "1.00"), limits = rev) +
  theme_classic() +
  null_theme +
  theme(aspect.ratio = 2.,
        legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))

ggsave("./figs/fig2J.jpg", dpi = 300)

########################
# Statistical analysis #
########################

m <- lmer(ratio~condition+(1|subject)+(1|date), pulse_ratio)
anova(m) %>% as.data.frame %>% write.csv(., "stats/fig2G-anova.csv")
emmeans(m, ~condition) %>%
  pairs %>%
  summary %>%
  as.data.frame %>%
  write.csv(., "stats/fig2G-multicomp.csv", row.names = F)
