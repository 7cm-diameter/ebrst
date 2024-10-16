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
      mutate(learning.ratio = d[d$parameter == "αqp", ]$mean / d[d$parameter == "αqn", ]$mean)
  }) %>%
  do.call(rbind, .)

####################
# Visualize result #
####################

dev.new(width = 5., height = 5., unit ="cm")

ggplot(learning_ratio_data) +
  stat_summary(fun = "mean", geom = "line",
               aes(x = condition, y = log10(learning.ratio), group = interaction(subject)),
               alpha = 0.25) +
  stat_summary(fun = "mean", geom = "point",
               aes(x = condition, y = log10(learning.ratio),  group = interaction(subject), color = condition),
               alpha = 1.) +
  stat_summary(fun.data = "mean_se", geom = "errorbar",
               aes(x = condition, y = log10(learning.ratio)),
               width = 0.2, linewidth = .75) +
  stat_summary(fun = "mean", geom = "bar",
               aes(x = condition, y = log10(learning.ratio), fill = condition),
               width = 0.5, alpha = 0.5) +
  geom_hline(yintercept = log10(1), linetype = "dashed", linewidth = 1) +
  scale_x_discrete(labels = c("0.25", "0.33", "0.50", "1.00"), limits = rev) +
  theme_classic() +
  theme(aspect.ratio = 2,
        legend.position = "none") +
  null_theme

ggsave("./figs/fig3D.jpg", dpi = 300)
########################
# Statistical analysis #
########################

m <- lmer(learning.ratio~condition+(1|subject)+(1|session), learning_ratio_data)
anova(m) %>% as.data.frame %>% write.csv(., "stats/fig3D-anova.csv")
emmeans(m, ~condition) %>%
  pairs %>%
  as.data.frame %>%
  write.csv(., "stats/fig3D-multicomp.csv", row.names = F)
