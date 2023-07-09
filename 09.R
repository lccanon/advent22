library(tidyverse)

motions <- read_delim("input09", delim = " ", col_names = c("dir", "step"))

N <- 1000
L <- 10

grid <- matrix(0, ncol = N, nrow = N)
rope <- tibble(x = rep(N / 2, L), y = rep(N / 2, L)) %>% as.matrix()
dirs <- list("R" = c(0, 1), "L" = c(0, -1), "U" = c(-1, 0), "D" = c(1, 0))

grid[rope[L,"x"],rope[L,"y"]] <- 1
for (i in 1:nrow(motions)) {
  m <- motions[i,]
  for (j in seq_len(m$step)) {
    rope[1,] <- rope[1,] + dirs[[m$dir]]
    for (k in 2:nrow(rope)) {
      if (abs(rope[k,"y"] - rope[k - 1,"y"]) >= 2 && abs(rope[k,"x"] - rope[k - 1,"x"]) >= 2) {
        rope[k,"y"] <- rope[k - 1,"y"] + sign(rope[k,"y"] - rope[k - 1,"y"])
        rope[k,"x"] <- rope[k - 1,"x"] + sign(rope[k,"x"] - rope[k - 1,"x"])
      } else if (abs(rope[k,"y"] - rope[k - 1,"y"]) >= 2) {
        rope[k,"x"] <- rope[k - 1,"x"]
        rope[k,"y"] <- rope[k - 1,"y"] + sign(rope[k,"y"] - rope[k - 1,"y"])
      } else if (abs(rope[k,"x"] - rope[k - 1,"x"]) >= 2) {
        rope[k,"x"] <- rope[k - 1,"x"] + sign(rope[k,"x"] - rope[k - 1,"x"])
        rope[k,"y"] <- rope[k - 1,"y"]
      }
    }
    grid[rope[L,"x"],rope[L,"y"]] <- 1
  }
}
sum(grid)

# Alternative solutions from reddit

motions <- read_delim("input09", delim = " ", col_names = c("dir", "step"))
L <- 10

dirs <- c("R" = 1, "L" = -1, "U" = -1i, "D" = 1i)
apply(motions, 1, \(x) rep(dirs[x["dir"]], x["step"])) %>%
  unlist %>%
  cumsum -> head.pos
pos <- matrix(head.pos, ncol = L, nrow = length(head.pos))
for (r in 2:L)
  for (i in 2:nrow(pos)) {
    diff <- pos[i,r - 1] - pos[i - 1,r]
    pos[i,r] <- pos[i - 1,r] + if (abs(diff) >= 2) sign(Re(diff)) + sign(Im(diff)) * 1i else 0
  }
length(unique(pos[,2]))
length(unique(pos[,10]))
