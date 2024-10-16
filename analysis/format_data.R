library(tidyverse)
library(comprexr)


LEVER <- 6
REWARD <- 12
REWARDPHASE <- 200
EXTINCTIONPHASE <- -REWARDPHASE

merged_data <- list.files("./data", pattern = "WT", full.names = T) %>%
  lapply(., function(path) {
    d <- path %>%
      read.csv %>%
      filter(event %in% c(LEVER, REWARD, REWARDPHASE, EXTINCTIONPHASE)) %>%
      add_metadata_to_df(., path) %>%
      (function(d) {
        start <- d %>%
          filter(event == REWARDPHASE) %>%
          head(1) %>%
          select(time)
        d %>%
          filter(time >= start$time)
      }) %>%
      mutate(time = time - min(time))
    nrft <- d %>% filter(event == REWARD) %>% nrow
    if (nrft >= 150) {
      return(d)
    } else {
      return(data.frame())
    }
  }) %>%
  do.call(rbind, .)

merged_data_with_phase_ids <- merged_data %>%
  split(., list(.$subject, .$date), drop = T) %>%
  lapply(., function(d) {
    phase_ids <- d %>% filter(event %in% c(REWARDPHASE, EXTINCTIONPHASE)) %>% (function(d) d$event)
    phase_length <- c(which(d$event %in% c(REWARDPHASE, EXTINCTIONPHASE)), nrow(d) + 1) %>% diff
    d <- d %>% mutate(phase = rep(phase_ids, phase_length), IRT = NA)
    d[which(d$event == LEVER), ]$IRT <- d[which(d$event == LEVER), ]$time %>% (function(x) diff(c(0, x)))
    d
  }) %>%
  do.call(rbind, .)

write.csv(merged_data_with_phase_ids, "./data/merged_data.csv", row.names = FALSE)
