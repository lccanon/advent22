library(tidyverse)

read_table("input04", col_names = FALSE, show_col_types = FALSE) %>%
  separate("X1", into = c("s1", "e1", "s2", "e2"), sep = "[,-]", convert = TRUE) %>%
  mutate(contained = s1 <= s2 & e2 <= e1 | s1 >= s2 & e2 >= e1) %>%
  mutate(overlap = pmax(s1, s2) <= pmin(e1, e2)) %>%
  summarise(contained = sum(contained), overlap = sum(overlap))
