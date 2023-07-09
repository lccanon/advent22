# Vectorized (base R)

readLines("input01") %>%
  as.numeric -> cals

cals[is.na(cals)] <- 0
rr <- rle(cumsum(cals))
cs <- rr$values[rr$length == 2]
elves <- c(cs[1], diff(cs))

max(elves)
sum(tail(sort(elves), 3))

# Vectorized (tidyverse)

library(tidyverse)

read_file("input01") %>%
  str_split("\\n\\n", simplify = TRUE) %>%
  map_int(~ str_split(., "\\n", simplify = TRUE) %>%
            as.integer %>%
            sum(na.rm = TRUE)) -> elves

max(elves)
sum(tail(sort(elves), 3))

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

max(elves)
sum(tail(sort(elves), 3))
