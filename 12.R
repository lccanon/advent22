library(tidyverse)

read_lines("input12") %>%
  str_split("") %>%
  do.call(what = rbind) -> input
input <- rbind("#", cbind("#", input, "#"), "#")

maze <- matrix(match(input, c(letters, "#")), nrow = nrow(input))
maze[input == "S"] <- 1
maze[input == "E"] <- 26

path <- maze * Inf
path[input == "E"] <- 0
prev_infinite <- Inf
DIR <- list(c(0, 1), c(0, -1), c(1, 0), c(-1, 0))
while (sum(is.infinite(path)) < prev_infinite) {
  prev_infinite <- sum(is.infinite(path))
  for (i in 2:(nrow(path) - 1))
    for (j in 2:(ncol(path) - 1))
      for (dir in DIR)
        if (maze[i + dir[1],j + dir[2]] <= maze[i,j] + 1)
          path[i,j] <- min(path[i,j], path[i + dir[1],j + dir[2]] + 1)
}

path[input == "S"]
min(path[maze == 1])

# Alternative not traversing all nodes everytime

maze[input == "#"] <- -Inf

path <- maze * Inf
path[input == "E"] <- 0
nodes <- which(input == "E", arr.ind = TRUE)
DIR <- list(c(0, 1), c(0, -1), c(1, 0), c(-1, 0))
while (length(nodes) != 0) {
  next_nodes <- cbind(row = numeric(0), col = numeric(0))
  for (i in 1:nrow(nodes))
    for (dir in DIR)
      if (maze[t(nodes[i,] + dir)] + 1 >= maze[t(nodes[i,])] &&
          path[t(nodes[i,])] + 1 < path[t(nodes[i,] + dir)]) {
        path[t(nodes[i,] + dir)] <- path[t(nodes[i,])] + 1
        next_nodes <- rbind(next_nodes, nodes[i,] + dir)
      }
  nodes <- unique(next_nodes)
}

path[input == "S"]
min(path[maze == 1])
