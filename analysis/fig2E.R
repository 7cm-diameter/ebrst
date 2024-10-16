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

align_with_onsets <- rawdata %>%
  split(., list(.$subject, .$date), drop = T) %>%
  lapply(., function(d) {
    onset_extinction <- d %>%
      align_with(., "event", EXTINCTIONPHASE, "time", 0, 60) %>%
      filter(event == LEVER) %>%
      mutate(onset = EXTINCTIONPHASE, phase == EXTINCTIONPHASE) %>%
      select(subject, date, condition, serial, time, IRT, phase, onset)
    onset_reward <- d %>%
      align_with(., "event", REWARDPHASE, "time", 0, 60) %>%
      filter(event == LEVER, phase == REWARDPHASE) %>%
      mutate(onset = REWARDPHASE) %>%
      select(subject, date, condition, serial, time, IRT, phase, onset)
    rbind(onset_reward, onset_extinction)
  }) %>%
  do.call(rbind, .)

response_per_sec_onsets <- align_with_onsets %>%
  mutate(time = bining(time, 5)) %>%
  group_by(subject, condition, onset, time) %>%
  summarise(response = 1 / mean(IRT), IRT = mean(IRT))

####################
# Visualize result #
####################

dev.new(width = 7., height = 7., unit ="cm")

ggplot(response_per_sec_onsets) +
  stat_summary(fun = "mean", geom = "line",
               aes(x = time, y = response, color = as.factor(onset), group = interaction(subject, onset)),
               linewidth = .5, alpha = 0.25) +
  stat_summary(fun.data = "mean_se", geom = "ribbon",
               aes(x = time, y = response, fill = as.factor(onset)),
               alpha = 0.25) +
  stat_summary(fun = "mean", geom = "line",
               aes(x = time, y = response, color = as.factor(onset)),
               linewidth = 2) +
  facet_wrap(~condition) +
  theme_classic() +
  null_theme +
  theme(aspect.ratio = 0.5)

ggsave("./figs/fig2E.jpg", dpi = 300)

########################
# Statistical analysis #
########################

# NO STAT
