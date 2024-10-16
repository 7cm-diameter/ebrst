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

IPIs_data <- pulse_data %>%
  filter(pulse == 1) %>%
  group_by(subject, date, condition) %>%
  mutate(IPI = time - lag(time)) %>%
  data.frame

survivor_individual <- IPIs_data %>%
  mutate(IPI = bining(IPI, 1)) %>%
  filter(!is.na(IPI)) %>%
  split(., list(.$subject, .$condition, .$phase), drop = T) %>%
  lapply(., function(d) {
    IPI_count <- table(d$IPI)
    exisits_IPI <- unique(d$IPI)
    IPI_bins <- seq(0, max(d$IPI), 1)
    nbins <- length(IPI_bins)
    subject <- d$subject %>% unique
    condition <- d$condition %>% unique
    phase <- d$phase %>% unique
    based <- data.frame(subject = rep(subject, nbins),
                        condition = rep(condition, nbins),
                        phase = reorder(phase, nbins),
                        IPI = IPI_bins,
                        count = rep(0, nbins))
    exisits_IPI %>%
      lapply(., function(i) {
        based[which(based$IPI == i), ]$count <<- d %>% filter(IPI == i) %>% nrow
      })
    based
  }) %>%
  do.call(rbind, .) %>%
  group_by(subject, condition, phase) %>%
  mutate(survivor = cumsum(count) / sum(count))

survivor_merged <- IPIs_data %>%
  mutate(IPI = bining(IPI, 1) + 1) %>%
  filter(!is.na(IPI)) %>%
  split(., list(.$condition, .$phase), drop = T) %>%
  lapply(., function(d) {
    IPI_count <- table(d$IPI)
    exisits_IPI <- unique(d$IPI)
    IPI_bins <- seq(0, max(d$IPI), 1)
    nbins <- length(IPI_bins)
    condition <- d$condition %>% unique
    phase <- d$phase %>% unique
    based <- data.frame(condition = rep(condition, nbins),
                        phase = reorder(phase, nbins),
                        IPI = IPI_bins,
                        count = rep(0, nbins))
    exisits_IPI %>%
      lapply(., function(i) {
        based[which(based$IPI == i), ]$count <<- d %>% filter(IPI == i) %>% nrow
      })
    based
  }) %>%
  do.call(rbind, .) %>%
  group_by(condition, phase) %>%
  mutate(survivor = cumsum(count) / sum(count))

####################
# Visualize result #
####################

dev.new(width = 6., height = 4., unit ="cm")

ggplot(survivor_individual) +
  geom_line(aes(x = log10(IPI), y = log10(1 - survivor), color = as.factor(phase), group = interaction(subject, phase)),
            linewidth = .25,alpha = 0.25) +
  geom_line(data = survivor_merged,
            aes(x = log10(IPI), y = log10(1 - survivor), color = as.factor(phase)),
            linewidth = .75) +
  coord_cartesian(xlim=c(0, 3)) +
  facet_wrap(~condition) +
  theme_classic() +
  theme(aspect.ratio = 0.5, legend.position = "none") +
  null_theme

ggsave("./figs/fig2K.jpg", dpi = 300)

########################
# Statistical analysis #
########################
