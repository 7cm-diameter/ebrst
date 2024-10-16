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
                    panel.spacing = unit(1, "cm"),
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

around_extinction_timewindow <- c(5, 15, 30, 60) %>%
  lapply(., function(t) {
    around_extinction %>%
      filter(time >= -t, time <= t) %>%
      mutate(range = t)
  }) %>%
  do.call(rbind, .)

extinction_increment <- around_extinction_timewindow %>%
  split(., list(.$subject, .$condition, .$date, .$range), drop = T) %>%
  lapply(., function(d) {
    d %>%
      head(., 1) %>%
      select(subject, condition, range, date) %>%
      mutate(reward = d %>% filter(phase == REWARDPHASE) %>% (function(d) 1 / mean(d$IRT)),
             extinction = d %>% filter(phase == EXTINCTIONPHASE) %>% (function(d) 1 / mean(d$IRT)))
  }) %>%
  do.call(rbind, .) %>%
  group_by(subject, condition, range, date) %>%
  mutate(increase = extinction / reward)

####################
# Visualize result #
####################

dev.new(width = 7., height = 7., unit ="cm")

ggplot(extinction_increment) +
  stat_summary(fun.data = "mean_se", geom = "errorbar",
               aes(x = condition, y = increase),
               width = 0.2, linewidth = 1) +
  stat_summary(fun = "mean", geom = "bar",
               aes(x = condition, y = increase, fill = condition),
               width = .5, alpha = 0.5) +
  stat_summary(fun = "mean", geom = "point",
               aes(x = condition, y = increase, color = condition, group = subject)) +
  stat_summary(fun = "mean", geom = "line",
               aes(x = condition, y = increase, group = subject),
               alpha = 0.25) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~range) +
  scale_x_discrete(labels = c("0.25", "0.33", "0.50", "1.00"), limits = rev) +
  theme_classic() +
  null_theme +
  theme(aspect.ratio = 1.5) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))

ggsave("./figs/fig2H.jpg", dpi = 300)

########################
# Statistical analysis #
########################

m <- lmer(increase~condition*as.factor(range) + (1|subject) + (1|date), extinction_increment)
anova(m) %>% as.data.frame %>% write.csv(., "stats/fig2E-anova.csv")
anova(m)
emmeans(m, ~condition*as.factor(range)) %>%
  pairs(., simple = "condition") %>%
  as.data.frame %>%
  write.csv(., "stats/fig2E-multicomp.csv", row.names = F)
