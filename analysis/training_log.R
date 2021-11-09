library(tidyverse)


training_data <- list.files("./data/rawdata/event", full.names = T) %>%
  (function(fs) fs[grep("training", fs)])
