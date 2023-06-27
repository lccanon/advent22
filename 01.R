library(tidyverse)
library(microbenchmark)

microbenchmark({
  
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
elves <- sort(elves)
print(sum(tail(elves, 3)))

})
