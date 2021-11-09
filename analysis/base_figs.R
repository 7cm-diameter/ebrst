library(tidyverse)
source("./analysis/util.R")

# read data files and merge them
DATA_DIR <- "./data"

merged_file <- join_path(DATA_DIR, "merged_data.csv")

datafiles <- list.files(DATA_DIR, full.names = T)

files_to_be_read <- datafiles %>%
  remove_from_vec(., merged_file) %>%
  extract_unmerged_data

merged_data <- merge_data(files_to_be_read,
                          output_name = merged_file)

if (length(files_to_be_read)) {
  write.csv(merged_data, merged_file, row.names = F)
}

# constants
LEVER <- 9
LICK <- 10
REWARD <- 12


# response rate
response_per_min <- merged_data %>%
  split(., list(.$subject, .$condition, .$date), drop = T) %>%
  lapply(., function(d) {
    scale <- 60
    nlever <- d %>% filter(event == LEVER) %>% nrow
    nlick <- d %>% filter(event == LICK) %>% nrow
    session_duration <- max(d$time) - min(d$time)
    data.frame(subject = unique(d$subject),
               date = unique(d$date),
               condition = unique(d$condition),
               lever = scale * nlever / session_duration,
               lick = scale * nlick / session_duration)
}) %>%
  do.call(rbind, .)

ggplot(data = response_per_min) +
  geom_line(aes(x = as.integer(date), y = lever)) +
  facet_wrap(~subject)

ggplot(data = response_per_min) +
  geom_line(aes(x = as.integer(date), y = lick)) +
  facet_wrap(~subject)

ggplot(data = response_per_min) +
  geom_point(aes(x = lever, y = lick))


# lever IRT
lever_IRTs <- merged_data %>%
  split(., list(.$subject, .$condition, .$date), drop = T) %>%
  lapply(., function(d) {
    IRTs <- d %>% filter(event == LEVER) %>% (function(d) diff(d$time))
    num_IRTs <- length(IRTs)
    data.frame(subject = rep(unique(d$subject), num_IRTs),
               date = rep(unique(d$date), num_IRTs),
               condition = rep(unique(d$condition), num_IRTs),
               IRTs = IRTs)
}) %>%
  do.call(rbind, .)

ggplot(data = lever_IRTs) +
  geom_histogram(aes(x = IRTs), binwidth = 0.1) +
  xlim(0, 10) +
  facet_wrap(~subject)


# lick IRT
lick_IRTs <- merged_data %>%
  split(., list(.$subject, .$condition, .$date), drop = T) %>%
  lapply(., function(d) {
    IRTs <- d %>% filter(event == LICK) %>% (function(d) diff(d$time))
    num_IRTs <- length(IRTs)
    data.frame(subject = rep(unique(d$subject), num_IRTs),
               date = rep(unique(d$date), num_IRTs),
               condition = rep(unique(d$condition), num_IRTs),
               IRTs = IRTs)
}) %>%
  do.call(rbind, .)

ggplot(data = lick_IRTs) +
  geom_histogram(aes(x = IRTs), binwidth = 0.01) +
  xlim(0, 0.5) +
  facet_wrap(~subject)


# lever duration
lever_duration <- merged_data %>%
  split(., list(.$subject, .$condition, .$date), drop = T) %>%
  lapply(., function(d) {
    lever_on <- d %>% filter(event == LEVER) %>% (function(d) d$time)
    lever_off <- d %>% filter(event == -LEVER) %>% (function(d) d$time)
    duration <- lever_on - lever_off
    num_duration <- length(duration)
    data.frame(subject = rep(unique(d$subject), num_duration),
               date = rep(unique(d$date), num_duration),
               condition = rep(unique(d$condition), num_duration),
               duration = duration)
}) %>%
  do.call(rbind, .)

ggplot(data = lever_duration) +
  geom_histogram(aes(x = duration), binwidth = 0.1) +
  xlim(0, 10) +
  facet_wrap(~subject)


# lick duration
lick_duration <- merged_data %>%
  split(., list(.$subject, .$condition, .$date), drop = T) %>%
  lapply(., function(d) {
    lick_on <- d %>% filter(event == LICK) %>% (function(d) d$time)
    lick_off <- d %>% filter(event == -LICK) %>% (function(d) d$time)
    if (length(lick_on) == length(lick_off)) {
      duration <- lick_on - lick_off
    } else {
      duration <- lick_on - lick_off[-1]
    }
    num_duration <- length(duration)
    data.frame(subject = rep(unique(d$subject), num_duration),
               date = rep(unique(d$date), num_duration),
               condition = rep(unique(d$condition), num_duration),
               duration = duration)
}) %>%
  do.call(rbind, .)


ggplot(data = lick_duration) +
  geom_histogram(aes(x = duration), binwidth = 0.005) +
  xlim(0, 0.2) +
  facet_wrap(~subject)
