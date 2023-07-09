library(tidyverse)

read_lines("input08") %>%
  str_split("") %>%
  map(as.integer) %>%
  do.call(what = rbind) -> heights

# Iterative version

visible <- 0
for (i in 1:nrow(heights))
  for (j in 1:ncol(heights))
    if (all(heights[i,j] > head(heights[,j], i - 1)) || 
        all(heights[i,j] > head(heights[i,], j - 1)) ||
        all(heights[i,j] > tail(heights[,j], -i)) ||
        all(heights[i,j] > tail(heights[i,], -j)))
      visible <- visible + 1
visible

score <- 0
hh <- cbind(max(heights) + 1,
            rbind(max(heights) + 1, heights, max(heights) + 1),
            max(heights) + 1)
for (i in 2:(nrow(hh) - 1))
  for (j in 2:(ncol(hh) - 1)) {
    left <- which(rev(head(hh[i,], j - 1)) >= hh[i,j])[1]
    right <- which(tail(hh[i,], -j) >= hh[i,j])[1]
    up <- which(rev(head(hh[,j], i - 1)) >= hh[i,j])[1]
    down <- which(tail(hh[,j], -i) >= hh[i,j])[1]
    score <- max(score, min(left, j - 2) *
                   min(right, ncol(hh) - j - 1) *
                   min(up, i - 2) *
                   min(down, nrow(hh) - i - 1))
  }
score

# Vectorized (base R)

vis_row <- \(x) x > c(-1, cummax(head(x, -1)))
vis_row_bi <- \(x) vis_row(x) | rev(vis_row(rev(x)))
visible <- apply(heights, 1, vis_row_bi) %>% t | apply(heights, 2, vis_row_bi)
sum(visible)

score_row <- \(x) sapply(seq_along(x), \(i) min(which(x[i] <= tail(x, -i))[1], ncol(heights) - i, na.rm = TRUE))
score_row_bi <- \(x) score_row(x) * rev(score_row(rev(x)))
score <- apply(heights, 1, score_row_bi) %>% t * apply(heights, 2, score_row_bi)
max(score)
