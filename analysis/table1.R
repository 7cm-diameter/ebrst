library(tidyverse)


read.csv("./data/merged_data.csv") %>%
  select(subject, date, condition) %>%
  distinct_all() %>%
  group_by(subject, condition) %>%
  summarise(count = n()) %>%
  data.frame %>%
  write.csv(., "./data/missings.csv", row.names = F)
