library(tidyverse)
library(ggcolors)
library(utexr)
library(lme4)
library(emmeans)
library(lmerTest)

#########################
# Define ggplot's theme #
#########################

null_theme <- theme(axis.text = element_text(color = "black", size = 15.),
                    axis.title = element_text(color = "transparent"),
                    axis.ticks.length = unit(-0.1, "cm"),
                    legend.text = element_text(color = "transparent"),
                    legend.title = element_text(color = "transparent"),
                    strip.text = element_text(color = "transparent"),
                    strip.background = element_rect(color = "transparent", fill = "transparent"),
                    panel.background = element_rect(fill='transparent'),
                    panel.spacing=unit(1, "cm"),
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
  do.call(rbind, .) %>%
  group_by(subject, date, condition, phase, range) %>%
  summarise(response = 1 / mean(IRT))

####################
# Visualize result #
####################

dev.new(width = 7., height = 7., unit ="cm")

ggplot(around_extinction_timewindow) +
  stat_summary(fun.data = "mean_se", geom = "errorbar",
               aes(x = condition, y = response, group = as.factor(phase)),
               width = 0.2, color = "black") +
  stat_summary(fun = "mean", geom = "line",
               aes(x = condition, y = response,
               color = as.factor(phase), group = interaction(subject, phase)),
               alpha = 0.25) +
  stat_summary(fun = "mean", geom = "point",
               aes(x = condition, y = response,
               color = as.factor(phase), group = interaction(subject, phase)),
               alpha = 0.25) +
  stat_summary(fun = "mean", geom = "point",
               aes(x = condition, y = response, color = as.factor(phase)),
               size = 2.5) +
  facet_wrap(~range) +
  theme_classic() +
  scale_x_discrete(labels = c("0.25", "0.33", "0.50", "1.00"), limits = rev) +
  null_theme +
  theme(aspect.ratio = 1.5) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))

ggsave("./figs/fig2G.jpg", dpi = 300)

########################
# Statistical analysis #
########################

m <- lmer(response~condition*phase*as.factor(range)+(1|subject), around_extinction_timewindow)
anova(m)
m <- lmer(response~condition*phase*as.factor(range)+(1|subject), around_extinction_timewindow)
anova(m) %>% as.data.frame %>% write.csv(., "stats/fig2D-anova.csv")
emmeans(m, ~condition*phase*as.factor(range)) %>%
  pairs(., simple = "phase") %>%
  as.data.frame %>%
  write.csv(., "stats/fig2D-multicomp.csv", row.names = F)
