library(tidyverse)
library(gmp)

read_lines("input15") %>%
  str_extract_all("-?\\d+") %>%
  map(as.numeric) %>%
  do.call(what = rbind) -> input
input <- cbind(input, dist = abs(input[,1] - input[,3]) + abs(input[,2] - input[,4]))

x_range <- range(input[,c(1,3)])
y_range <- range(input[,c(2,4)])

x <- (x_range[1] - max(input[,"dist"])):(x_range[2] + max(input[,"dist"]))
y <- 2000000
inside <- rep(FALSE, length(x))
for (i in 1:nrow(input))
  inside <- inside | abs(x - input[i,1]) + abs(y - input[i,2]) <= input[i,"dist"]
beacons <- unique(input[,3:4])
print(sum(inside) - sum(beacons[,2] == y))

for (i in 1:nrow(input)) {
  sensor <- input[i,]
  candidates <- rbind(cbind((sensor[1] - sensor["dist"] - 1):sensor[1],
                            sensor[2]:(sensor[2] + sensor["dist"] + 1)),
                      cbind((sensor[1] - sensor["dist"] - 1):sensor[1],
                            sensor[2]:(sensor[2] - sensor["dist"] - 1)),
                      cbind((sensor[1] + sensor["dist"] + 1):sensor[1],
                            sensor[2]:(sensor[2] + sensor["dist"] + 1)),
                      cbind((sensor[1] + sensor["dist"] + 1):sensor[1],
                            sensor[2]:(sensor[2] - sensor["dist"] - 1)))
  candidates <- candidates[pmin(candidates[,1], candidates[,2]) >= 0 &
                             pmax(candidates[,1], candidates[,2]) <= 4000000,]
  # print(c(i, nrow(candidates)))
  inside <- rep(FALSE, nrow(candidates))
  for (j in 1:nrow(input)) {
    inside <- inside | abs(candidates[,1] - input[j,1]) + 
      abs(candidates[,2] - input[j,2]) <= input[j,"dist"]
  }
  if (any(!inside)) {
    idx <- which(!inside)
    print(as.bigq(candidates[idx,1]) * 4000000 + candidates[idx,2])
    break
  }
}
