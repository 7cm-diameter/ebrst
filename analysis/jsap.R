library(tidyverse)
library(zoo)
library(ggcolors)
library(viridis)
library(comprexr)
library(gt)
options(browser = "google-chrome-stable")


fonts_config <- theme(axis.text = element_text(color = "transparent"),
                      axis.title = element_text(color = "transparent"),
                      legend.text = element_text(color = "transparent"),
                      legend.title = element_text(color = "transparent"),
                      strip.text = element_text(color = "transparent"),
                      strip.background = element_rect(color = "transparent", fill = "transparent"))


# read csv and format data
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
        aligned_timed <- d$time - min(d$time) # align on session start
        relative_time <- aligned_timed / max(aligned_timed)
        d %>% mutate(time = aligned_timed, relative_time = relative_time)
      }) %>%
      do.call(rbind, .)
}) %>%
  do.call(rbind, .)

# response rate dynamics during session
response_per_sec <- merged_data %>%
  split(., list(.$subject, .$session), drop = T) %>%
  lapply(., function(d) {
    d %>%
      filter(event == 9) %>%
      mutate(rps = 1 / diff(c(0, time)))
}) %>%
  do.call(rbind, .)

ggplot() +
  geom_point(data = response_per_sec,
             aes(x = relative_time, y = rps),
             alpha = 0.5, size = 0.5) +
  geom_line(data = response_per_sec,
             aes(x = relative_time, y = rollmean(rps, 11, na.pad = T)),
             ) +
  geom_vline(data = merged_data %>% filter(event == 200),
             aes(xintercept = relative_time),
             color = "red") +
  geom_vline(data = merged_data %>% filter(event == -200),
             aes(xintercept = relative_time),
             color = "red", linetype = "dashed") +
  coord_cartesian(ylim = c(0, 7)) +
  facet_grid(~subject~session, scale = "free")

# align on extinction and reward onsets
aligned_on_event <- merged_data %>%
  split(., list(.$subject, .$session), drop = T) %>%
  lapply(., function(d) {
    dirt <- d %>%
      filter(event == 9) %>%
      mutate(IRT = diff(c(0, time)), sIRT = sample(IRT))
    when_events <- d %>%
      filter(event == 200 | event == 12) %>%
      mutate(IRT = 0, sIRT = 0)
    d_ <- rbind(dirt, when_events) %>%
      (function(d) d[order(d$time), ])
    align_ext <- d_ %>%
      align_with(., "event", 200, "time", -30, 60) %>%
      filter(event == 9) %>%
      mutate(align.on = "extinction")
    align_rew <- d_ %>%
      align_with(., "event", 12, "time", -30, 60) %>%
      filter(event == 9) %>%
      mutate(align.on = "reward")
    rbind(align_rew, align_ext)
  }) %>%
  do.call(rbind, .)

ggplot(data = aligned_on_event) +
  geom_smooth(aes(x = time, y = 1 / IRT, color = align.on),
              fill = "#1a3048") +
  geom_smooth(aes(x = time, y = 1 / sIRT, color = align.on),
              fill = "#1a3048", linetype = "dotted") +
  coord_cartesian(ylim = c(0, 5)) +
  facet_wrap(~subject) +
  xlab("Time from onsets of extinction or reward") +
  ylab("Response per second") +
  thanatos_dark_color_discrete() +
  # thanatos_dark_color_with_name() +
  # theme_thanatos_dark(aspect.ratio = 1)
  theme_bw() +
  theme(aspect.ratio = 1) +
  fonts_config

ggsave("./fig7-1.jpg")

# comapre empirical data and estimated data
estimated_data <- read.csv("./data/pilot/estimated_data.csv")

# align on extinction and reward onsets
aligned_on_event_est <- estimated_data %>%
  split(., list(.$subject, .$session), drop = T) %>%
  lapply(., function(d) {
    align_ext <- d %>%
      align_with(., "event", 200, "time", -30, 60) %>%
      filter(event == 9) %>%
      mutate(align.on = "extinction")
    align_rew <- d %>%
      align_with(., "event", 12, "time", -30, 60) %>%
      filter(event == 9) %>%
      mutate(align.on = "reward")
    rbind(align_rew, align_ext)
  }) %>%
  do.call(rbind, .)

aligned_on_event_est %>% str

ggplot(data = aligned_on_event_est) +
  geom_smooth(aes(x = time, y = 1 / hql_theta, color = align.on),
              fill = "#1a3048", linetype = "dotted") +
  facet_wrap(~subject) +
  xlab("Time from onsets of extinction / reward") +
  ylab("Response per second")

# burst strength
brststr <- read.csv("./data/pilot/brststr.csv")
brststr$burst[brststr$burst == 0] <- NaN
brststr

fig1 <- ggplot(data = brststr) +
  geom_tile(aes(x = alpha_p, y = alpha_n, fill = burst)) +
  facet_grid(~p~w) +
  xlab("") + ylab("") +
  scale_fill_viridis() +
  theme_classic() +
  theme(aspect.ratio = 1) +
  fonts_config
fig1

ggsave("./fig1.jpg", fig1)

# model selection
fitted_result <- read.csv(paste0(DATA_DIR, "fitted_result.csv"))

fitted_result %>% str
fitted_result %>%
  filter(model == "heirarchical") %>%
  (function(d) d$alpha_p / d$alpha_m) %>%
  mean

selected_model <- fitted_result %>%
  split(., list(.$subject, .$session)) %>%
  lapply(., function(d) {
    delta_AIC <- d$AIC[1] - d$AIC[2]
    delta_AIC
    if (delta_AIC < 0) {
      selected_model <- "HQ-learning"
    } else {
      selected_model <- "VQ-learning"
    }
    data.frame(subject = unique(d$subject),
               session = unique(d$session),
               condition = unique(d$condition),
               delta_AIC = delta_AIC,
               selected = selected_model)
}) %>%
  do.call(rbind, .) %>%
  (function(d) d[order(d$subject), ])

tibble(selected_model) %>%
  gt(groupname_col = "subject") %>%
  cols_align(align = "center") %>%
  tab_header(title = "個体・セッションごとのHQ-learningとVQ-learningのフィッティング結果")

# example of analytical approximation
analytical_approximation <- read.csv(paste0(DATA_DIR, "analytical_approximation.csv"))

analytical_approximation$t <- seq_len(nrow(analytical_approximation))

fig2 <- ggplot(analytical_approximation) +
  geom_line(aes(x = t, y = q, color = "red"),
            alpha = 0.7, size = 1) +
  geom_line(aes(x = t, y = epsilon, color = "green"),
            alpha = 0.7, size = 1) +
  geom_line(aes(x = t, y = theta, color = "blue"),
            size = 2) +
  thanatos_light_color_with_name() +
  theme_bw() +
  theme(legend.position = "none", aspect.ratio = 1) +
  fonts_config
ggsave("./fig2.jpg", fig2)
