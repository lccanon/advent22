library(tidyverse)

read_delim("input02", delim = " ", col_names = c("opponent", "you"),
           show_col_types = FALSE) -> guide

shape <- tribble(
  ~you, ~shape1, ~score2,
  "X", 1, 0,
  "Y", 2, 3,
  "Z", 3, 6
)

scoring <- tribble(
  ~opponent, ~you, ~score1, ~shape2,
  "A", "X", 3, 3,
  "A", "Y", 6, 1,
  "A", "Z", 0, 2,
  "B", "X", 0, 1,
  "B", "Y", 3, 2,
  "B", "Z", 6, 3,
  "C", "X", 6, 2,
  "C", "Y", 0, 3,
  "C", "Z", 3, 1,
)

guide %>%
  inner_join(shape, by = "you") %>%
  inner_join(scoring, by = c("opponent", "you")) %>%
  summarize(score1 = sum(shape1 + score1), score2 = sum(shape2 + score2))
