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

response_per_sec <- rawdata %>%
  filter(event == LEVER, phase == REWARDPHASE) %>%
  group_by(subject, condition, date) %>%
  summarise(response = 1 / mean(IRT))

####################
# Visualize result #
####################

dev.new(width = 4., height = 4., unit ="cm")

ggplot(response_per_sec) +
  stat_summary(fun = "mean", geom = "line",
               aes(x = condition, y = response, group = subject),
               linewidth = 0.5, alpha = 0.25) +
  stat_summary(fun = "mean", geom = "point",
               aes(x = condition, y = response, group = subject, color = condition),
               size = 1) +
  stat_summary(fun.data = "mean_se", geom = "errorbar",
               aes(x = condition, y = response, color = condition),
               width = 0.2, linewidth = 1) +
  stat_summary(fun = "mean", geom = "bar",
               aes(x = condition, y = response, fill = condition),
               width = .5, linewidth = 1, color = "transparent", alpha = 0.5) +
  theme_classic() +
  scale_x_discrete(labels = c("0.25", "0.33", "0.5", "1.0"), limits = rev) +
  scale_y_continuous(limits = c(0., 1.2), breaks = c(0, round(1.1 * seq_len(4) / 4, digits = 2))) +
  null_theme +
  theme(legend.position = "none")

ggsave("./figs/fig2D.jpg", dpi = 300)

########################
# Statistical analysis #
########################

m <- lmer(response~condition + (1|subject) + (1|date), response_per_sec)
anova(m) %>% as.data.frame %>% write.csv(., "stats/fig2A-anova.csv")
emmeans(m, ~condition) %>%
  pairs %>%
  as.data.frame %>%
  write.csv(., "stats/fig2A-multicomp.csv", row.names = F)
