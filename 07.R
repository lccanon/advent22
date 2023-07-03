library(tidyverse)

insts <- read_lines("input07")

sizes <- list()
path <- "/"
for (inst in tail(insts, -1)) {
  if (str_detect(inst, "^\\$ cd \\.\\.")) {
    path <- head(path, -1)
  } else if (str_detect(inst, "^\\$ cd ")) {
    dir <- str_sub(inst, start = 6)
    path <- c(path, dir)
  } else if (str_detect(inst, "^\\d+")) {
    size <- str_extract(inst, "^\\d+") %>% as.integer
    for (i in 1:length(path))
      sizes[[paste0(path[1:i], collapse = "/")]] <-
        sum(sizes[[paste0(path[1:i], collapse = "/")]], size)
  }
}
sizes <- unlist(sizes)
sum(sizes[sizes <= 100000])
min(sizes[sizes >= sizes["/"] - 40000000])
