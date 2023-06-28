library(tidyverse)

read_csv("input03", col_names = "sack", show_col_types = FALSE) %>%
  mutate(first = str_sub(sack, end = str_length(sack) / 2)) %>%
  mutate(last = str_sub(sack, start = str_length(sack) / 2 + 1)) %>%
  apply(1, \(x) intersect(strsplit(x["first"], "")[[1]], strsplit(x["last"], "")[[1]])) %>%
  map_int(~ ifelse(any(letters == .), which(letters == .), which(LETTERS == .) + 26L)) %>%
  sum
  
read_csv("input03", col_names = "sack", show_col_types = FALSE) %>%
  mutate(group = (1:n() - 1) %/% 3) %>%
  mutate(rank = (1:n() - 1) %% 3) %>%
  mutate(sack = map(strsplit(sack, ""), unique)) %>%
  pivot_wider(names_from = rank, values_from = sack) %>%
  apply(1, \(x) { t <- table(unlist(x[-1])) ; names(t)[t == 3] }) %>%
  map_int(~ ifelse(any(letters == .), which(letters == .), which(LETTERS == .) + 26L)) %>%
  sum
