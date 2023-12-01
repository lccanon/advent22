library(tidyverse)

val <- read_file("input14") %>% str_extract_all("\\d+", simplify = TRUE) %>% as.numeric
mat <- matrix(0, nrow = 1000, ncol = max(val) + 1)

input <- read_lines("input14")

for (line in input) {
  segments <- line %>%
    str_split_1(" -> ") %>%
    map(~ str_split_1(., ",") %>% as.numeric) %>%
    do.call(what = rbind)
  for (i in 2:nrow(segments)) {
    orig <- segments[i - 1,]
    curr <- segments[i,]
    mat[cbind(orig[1]:curr[1], orig[2]:curr[2])] <- 1
  }
}

sand <- c(500, 0)
while (sand[2] + 1 != ncol(mat)) {
  if (mat[sand[1], sand[2] + 1] == 0) {
    sand[2] <- sand[2] + 1
  } else if (mat[sand[1] - 1, sand[2] + 1] == 0) {
    sand <- c(sand[1] - 1, sand[2] + 1)
  } else if (mat[sand[1] + 1, sand[2] + 1] == 0) {
    sand <- c(sand[1] + 1, sand[2] + 1)
  } else {
    mat[sand[1],sand[2]] <- 2
    sand <- c(500, 0)
  }
}
print(sum(mat == 2))

lowest <- max(which(apply(mat, 2, function(x) any(x == 1))))
mat[,lowest + 2] <- 1
mat[mat == 2] <- 0

sand <- c(500, 0)
while (any(mat[500 + -1:1,1] == 0)) {
  if (mat[sand[1], sand[2] + 1] == 0) {
    sand[2] <- sand[2] + 1
  } else if (mat[sand[1] - 1, sand[2] + 1] == 0) {
    sand <- c(sand[1] - 1, sand[2] + 1)
  } else if (mat[sand[1] + 1, sand[2] + 1] == 0) {
    sand <- c(sand[1] + 1, sand[2] + 1)
  } else {
    mat[sand[1],sand[2]] <- 2
    sand <- c(500, 0)
  }
}
print(sum(mat == 2) + 1)
