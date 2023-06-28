library(tidyverse)

read_csv("input03", col_names = "sack", show_col_types = FALSE) %>%
  mutate(first = str_sub(sack, end = str_length(sack) / 2)) %>%
  mutate(last = str_sub(sack, start = str_length(sack) / 2 + 1)) %>%
  apply(1, \(x) intersect(str_split(x["first"], "", simplify = TRUE),
                          str_split(x["last"], "", simplify = TRUE))) %>%
  map_int(~ which(c(letters, LETTERS) == .)) %>%
  sum
  
read_csv("input03", col_names = "sack", show_col_types = FALSE) %>%
  mutate(sack = map(str_split(sack, ""), unique)) %>%
  mutate(group = (1:n() - 1) %/% 3) %>%
  mutate(rank = (1:n() - 1) %% 3) %>%
  pivot_wider(names_from = rank, values_from = sack) %>%
  apply(1, \(x) { t <- table(unlist(x[-1])) ; names(t)[t == 3] }) %>%
  map_int(~ which(c(letters, LETTERS) == .)) %>%
  sum
