library(tidyverse)

read_lines("input06") %>%
  str_split("") %>%
  unlist -> line

marker <- 14
for (i in marker:length(line))
  if (unique(line[(i - marker + 1):i]) %>%
      length == marker) {
    print(i)
    break
  }
