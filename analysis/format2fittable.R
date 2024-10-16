library(tidyverse)
library(ggcolors)
library(utexr)


LEVER <- 6
REWARD <- 12
REWARDPHASE <- 200
EXTINCTIONPHASE <- -REWARDPHASE

rawdata <- read.csv("./data/merged_data.csv") %>%
  filter(!(event == 6 && IRT < 0.01)) %>%
  split(., list(.$subject, .$date), drop = T) %>%
  lapply(., function(d) {
    d <- d %>% mutate(IRT = NA)
    d[which(d$event == LEVER), ]$IRT <- d[which(d$event == LEVER), ]$time %>% (function(x) diff(c(0, x)))
    d
  }) %>%
  do.call(rbind, .)

pulse_data <- rawdata %>%
  mutate(local.response.rate = 1 / IRT) %>%
  split(., list(.$subject, .$date), drop = T) %>%
  lapply(., function(d) {
    threshold <- d %>%
      filter(phase == REWARDPHASE, event == LEVER) %>%
      (function(d) quantile(d$local.response.rate, 0.95))
    d %>%
      mutate(pulse = as.numeric(local.response.rate >= threshold))
  })

throughout <- pulse_data %>%
  lapply(., function(d) {
    d %>%
      mutate(reward = as.numeric(diff(c(event, LEVER)) == 6)) %>%
      filter(event == LEVER)
  }) %>%
  do.call(rbind, .) %>%
  split(., .$subject, drop = T) %>%
  lapply(., function(d) {
    dates <- d$date %>% unique %>% sort
    sessions <- seq_len(length(dates))
    d2s <- dict(dates, sessions)
    d <- d %>%
      mutate(session = map(date, d2s) %>% unlist) %>%
      mutate(block = 1)
    d <- d[order(d$session), ]
    d %>%
      select(subject, session, condition, block, phase, time, IRT, pulse, reward)
  }) %>%
  do.call(rbind, .)

throughout %>%
  split(., list(.$subject, .$session), drop = T) %>%
  lapply(., function(d) {
    subject <- d$subject %>% unique
    session <- d$session %>% unique %>% as.character
    path <- paste0("./data/", subject, "-", session, "_subses.csv")
    write.csv(d, path, row.names = FALSE)
  })

throughout %>%
  split(., list(.$subject), drop = T) %>%
  lapply(., function(d) {
    subject <- d$subject %>% unique
    path <- paste0("./data/", subject,  "_sub.csv")
    write.csv(d, path, row.names = FALSE)
  })


write.csv(throughout, "./data/fittable_all_merged.csv", row.names = FALSE)

# 
# 
# around_extinction <- rawdata %>%
#   split(., list(.$subject, .$date), drop = T) %>%
#   lapply(., function(d) {
#     d %>%
#       align_with(., "event", EXTINCTIONPHASE, "time", -60, 60) %>%
#       filter(event %in% c(LEVER, REWARD)) %>%
#       mutate(reward = as.numeric(diff(c(event, LEVER)) == 6)) %>%
#       filter(event == LEVER)
#   }) %>%
#   do.call(rbind, .) %>%
#   split(., .$subject, drop = T) %>%
#   lapply(., function(d) {
#     dates <- d$date %>% unique %>% sort
#     sessions <- seq_len(length(dates))
#     d2s <- dict(dates, sessions)
#     i <- 0
#     d <- d %>%
#       mutate(session = map(date, d2s) %>% unlist) %>%
#       split(., list(.$serial, .$session), drop = T) %>%
#       lapply(., function(d) {
#         i <<- i + 1
#         d %>%
#           mutate(block = i)
#       }) %>%
#       do.call(rbind, .)
#     d <- d[order(d$session), ]
#     d %>%
#       select(subject, session, condition, block, time, IRT, reward)
#   }) %>%
#   do.call(rbind, .)
# 
# around_extinction %>%
#   split(., list(.$subject, .$session), drop = T) %>%
#   lapply(., function(d) {
#     subject <- d$subject %>% unique
#     date <- d$session %>% unique
#     path <- paste0("./data/", subject, "-", date, "_around_extinction.csv")
#     write.csv(d, path, row.names = FALSE)
#   })
# 
# write.csv(around_extinction, "./data/fittable_around_extionction_merged.csv", row.names = FALSE)
# 
# throughout <- rawdata %>%
#   split(., list(.$subject, .$date), drop = T) %>%
#   lapply(., function(d) {
#     d %>%
#       mutate(reward = as.numeric(diff(c(event, LEVER)) == 6)) %>%
#       filter(event == LEVER)
#   }) %>%
#   do.call(rbind, .) %>%
#   split(., .$subject, drop = T) %>%
#   lapply(., function(d) {
#     dates <- d$date %>% unique %>% sort
#     sessions <- seq_len(length(dates))
#     d2s <- dict(dates, sessions)
#     d <- d %>%
#       mutate(session = map(date, d2s) %>% unlist) %>%
#       mutate(block = 1)
#     d <- d[order(d$session), ]
#     d %>%
#       select(subject, session, condition, block, phase, time, IRT, reward)
#   }) %>%
#   do.call(rbind, .)
# 
# throughout %>%
#   split(., list(.$subject, .$session), drop = T) %>%
#   lapply(., function(d) {
#     subject <- d$subject %>% unique
#     date <- d$session %>% unique %>% as.character
#     path <- paste0("./data/", subject, "-", date, "_all.csv")
#     write.csv(d, path, row.names = FALSE)
#   })
# 
# 
# write.csv(throughout, "./data/fittable_all_merged.csv", row.names = FALSE)
