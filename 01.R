library(tidyverse)

# Vectorized version

read_file("input01") %>%
  str_split("\\n\\n") %>%
  unlist %>%
  map_int(~ str_split(., "\\n") %>%
            unlist %>%
            as.integer %>%
            sum(na.rm = TRUE)) -> elves

print(max(elves))
print(sum(tail(sort(elves), 3)))

# Iterative version

readLines("input01") %>%
  as.numeric -> cals

elves <- list()
elves[[1]] <- 0
for (i in 1:length(cals)) {
  if (is.na(cals[[i]]))
    elves[[length(elves) + 1]] <- 0
  else
    elves[[length(elves)]] <- cals[[i]] + elves[[length(elves)]]
}
elves <- unlist(elves)

print(max(elves))
print(sum(tail(sort(elves), 3)))
