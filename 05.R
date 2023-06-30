library(tidyverse)

read_file("input05") %>%
  str_split("\\n\\n", simplify = TRUE) %>%
  str_split("\\n") -> input

input[[1]] %>%
  str_split("") %>%
  do.call(what = rbind) %>%
  t -> mat
mat <- mat[seq(2, nrow(mat), 4),ncol(mat):1]
map(1:nrow(mat), ~tail(mat[.,mat[.,] != " "], -1)) %>%
  `names<-`(mat[,1]) -> cranes

input[[2]] %>%
  head(-1) %>%
  str_extract_all("\\d+") %>%
  map(as.integer) -> insts

cranes2 <- cranes
for (inst in insts) {
  cranes[[inst[3]]] <- c(cranes[[inst[3]]], tail(cranes[[inst[2]]], inst[1]) %>% rev)
  cranes[[inst[2]]] <- head(cranes[[inst[2]]], -inst[1])
  cranes2[[inst[3]]] <- c(cranes2[[inst[3]]], tail(cranes2[[inst[2]]], inst[1]))
  cranes2[[inst[2]]] <- head(cranes2[[inst[2]]], -inst[1])
}
map_chr(cranes, ~ tail(., 1)) %>% paste(collapse = "")
map_chr(cranes2, ~ tail(., 1)) %>% paste(collapse = "")
