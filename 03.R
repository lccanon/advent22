library(tidyverse)

read_csv("input03", col_names = "sack", show_col_types = FALSE) %>%
  mutate(first = str_sub(sack, end = str_length(sack) / 2)) %>%
  mutate(last = str_sub(sack, start = str_length(sack) / 2 + 1)) %>%
  rowwise() %>%
  mutate(common = intersect(str_split(first, "", simplify = TRUE),
                            str_split(last, "", simplify = TRUE))) %>%
  ungroup() %>%
  mutate(score = match(common, c(letters, LETTERS))) %>%
  summarise(score = sum(score))

read_csv("input03", col_names = "sack", show_col_types = FALSE) %>%
  mutate(sack = map(str_split(sack, ""), unique)) %>%
  mutate(group = (1:n() - 1) %/% 3) %>%
  mutate(rank = (1:n() - 1) %% 3) %>%
  pivot_wider(names_from = rank, values_from = sack) %>%
  rowwise() %>%
  mutate(common = {
    t <- table(unlist(c(`0`, `1`, `2`)))
    names(t)[t == 3]
    }) %>%
  ungroup() %>%
  mutate(score = match(common, c(letters, LETTERS))) %>%
  summarise(score = sum(score))
