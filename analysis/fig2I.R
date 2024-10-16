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

example <- rawdata %>%
  filter(subject == "WTY03N", date == 220426) %>%
  mutate(local.response.rate = 1 / IRT)

IRT_count_data <- example %>%
  filter(phase == REWARDPHASE, event == LEVER) %>%
  mutate(IRT = bining(IRT, 0.01)) %>%
  (function(d) {
    IRT_count <- table(d$IRT)
    exisits_IRT <- unique(d$IRT)

    IRT_bins <- seq(0, max(d$IRT), 0.01)
    nbins <- length(IRT_bins)
    subject <- d$subject %>% unique
    date <- d$date %>% unique
    condition <- d$condition %>% unique

    based <- data.frame(subject = rep(subject, nbins),
                        date = rep(date, nbins),
                        condition = rep(condition, nbins),
                        IRT = IRT_bins,
                        count = rep(0, nbins))

    exisits_IRT %>%
      lapply(., function(i) {
        based[which(based$IRT == i), ]$count <<- d %>% filter(IRT == i) %>% nrow
      })

    based
  }) %>%
  mutate(local.response.rate = 1 / IRT)

local_response_rates <- IRT_count_data[order(IRT_count_data$local.response.rate),]

cumulative_count_lrr <- local_response_rates %>%
  mutate(svr = cumsum(count) / sum(count))

threshold <- cumulative_count_lrr %>%
  filter(subject == "WTY03N", date == 220426) %>%
  (function(d) quantile(d$local.response.rate, 0.95))

####################
# Visualize result #
####################

dev.new(width = 4., height = 3.5, unit ="cm")

ggplot(example %>% filter(event == LEVER)) +
  geom_point(aes(x = time, y = IRT, color = as.factor(phase)),
             alpha = 0.5, size = .5) +
  ylim(0, 10) +
  theme_classic() +
  theme(aspect.ratio = 1 / 2) +
  xlim(0, 1000) +
  null_theme +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, vjust = .75, hjust = 0.75))

ggsave("./figs/fig2I-1.jpg", dpi = 300)

ggplot(example %>% filter(event == LEVER)) +
  geom_point(aes(x = time, y = local.response.rate, color = as.factor(phase)),
             alpha = 0.5, size = .5) +
  geom_hline(yintercept = threshold, linetype = "dashed") +
  xlim(0, 1000) +
  theme_classic() +
  theme(aspect.ratio = 1 / 2) +
  null_theme +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, vjust = .75, hjust = 0.75))

ggsave("./figs/fig2I-2.jpg", dpi = 300)

ggplot(cumulative_count_lrr) +
  geom_line(aes(x = local.response.rate, y = svr),
            linewidth = 1) +
  geom_hline(yintercept = 0.95, linetype = "dotted") +
  geom_vline(xintercept = threshold, linetype = "dashed") +
  xlim(0, 15) +
  theme_classic() +
  null_theme +
  theme(aspect.ratio = 2)

ggsave("./figs/fig2I-3.jpg", dpi = 300)

ggplot(example %>% filter(event == LEVER)) +
  geom_point(aes(x = time, y = (local.response.rate >= threshold), color = as.factor(phase)),
             alpha = 0.5, size = .5) +
  theme_classic() +
  xlim(0, 1000) +
  theme(aspect.ratio = 1 / 2) +
  null_theme +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(angle = 45, vjust = -1)) +
  theme(axis.text.x = element_text(angle = 30, vjust = .75, hjust = 0.75))

ggsave("./figs/fig2I-4.jpg", dpi = 300)

########################
# Statistical analysis #
########################

# NO STAT
