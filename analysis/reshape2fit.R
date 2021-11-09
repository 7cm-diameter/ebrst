library(tidyverse)

DATA_DIR <- "./data/pilot/"
filepath <- paste0(DATA_DIR, "merged_data.csv")

merged_data <- read.csv(filepath) %>%
  split(., list(.$subject), drop = T) %>%
  lapply(., function(d) {
    d %>%
      mutate(session = rep(1:3, as.vector(table(d$date)))) %>%
      select(!date) %>%
      split(., list(.$session), drop = T) %>%
      lapply(., function(d) {
        d %>%
          mutate(time = time - min(time)) %>% # align on session start
          filter(event == 9 | event == 12 | event == 200)
      }) %>%
      do.call(rbind, .)
}) %>%
  do.call(rbind, .)

# fittable <- merged_data %>%
#   split(., list(.$subject, .$session), drop = T) %>%
#   lapply(., function(d) {
#     where_lever <- d %>% filter(event == 9)
#     where_reward <- d %>% filter(event == 12)
#     rewarded <- where_lever %>%
#       filter(time %in% where_reward$time) %>%
#       mutate(response = 1, reward = 1)
#     unrewarded <- where_lever %>%
#       filter(!(time %in% where_reward$time)) %>%
#       mutate(response = 1, reward = 0)
#     d_ <- rbind(rewarded, unrewarded) %>%
#       (function(d) d[order(d$time), ]) %>%
#       select(!event)
#     bins <- seq(0.00, max(d_$time), 0.01)
#     n <- length(bins)
#     no_resp <- data.frame(subject = d$subject %>% unique %>% rep(., n),
#                           condition = d$condition %>% unique %>% rep(., n),
#                           time = bins,
#                           session = d$session %>% unique %>% rep(., n),
#                           response = rep(0, n),
#                           reward = rep(0, n))
#     no_resp %>%
#       filter(!(time %in% d_$time)) %>%
#       rbind(., d_) %>%
#       (function(d) d[order(d$time), ])
# }) %>%
#   do.call(rbind, .)
# 
# write.csv(fittable, paste0(DATA_DIR, "fittable.csv"), row.names = F)

fittable <- merged_data %>%
  split(., list(.$subject, .$session), drop = T) %>%
  lapply(., function(d) {
    IRTs <- d %>%
      filter(event == 9) %>%
      mutate(IRT = diff(c(0, time)))
    when_events <- d %>%
      filter(event == 200 | event == 12) %>%
      mutate(IRT = 0)
    d_ <- rbind(IRTs, when_events) %>%
      (function(d) d[order(d$time), ])
    rewards <- seq_len(nrow(d_) - 1) %>%
      lapply(., function(i) {
        reward <- 0.
        if (d$event[i + 1] == 12) {
          reward <- 1.
        }
        reward
      }) %>% unlist
    d_$reward <- c(rewards, 0)
    d_
}) %>%
  do.call(rbind, .)

write.csv(fittable, paste0(DATA_DIR, "fittable.csv"), row.names = F)
