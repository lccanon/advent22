library(tidyverse)

read_file("input05") %>%
  str_split("\\n\\n", simplify = TRUE) %>%
  str_split("\\n") -> input

head(input[[1]], -1) %>%
  str_replace_all("[\\[\\]]", " ") %>%
  str_replace_all(" {4}", " .") %>%
  str_replace_all(" {3}", " ") %>%
  str_replace_all("^ | $", "") %>%
  tibble %>%
  separate(col = ".", into = str_extract_all(tail(input[[1]], 1), "\\d",
                                             simplify = TRUE), sep = " ") %>%
  pivot_longer(everything()) %>%
  filter(value != ".") -> in_cranes

cranes <- list()
for (i in sort(unique(in_cranes$name))) {
  cranes[[i]] <- in_cranes %>%
    filter(name == i) %>%
    pull(value) %>%
    rev
}
cranes2 <- cranes

head(input[[2]], -1) %>%
  str_extract_all("\\d+") %>%
  map(as.integer) -> insts

for (inst in insts) {
  for (i in seq_len(inst[1])) {
    cranes[[inst[3]]] <- c(cranes[[inst[3]]], tail(cranes[[inst[2]]], 1))
    cranes[[inst[2]]] <- head(cranes[[inst[2]]], -1)
  }
  cranes2[[inst[3]]] <- c(cranes2[[inst[3]]], tail(cranes2[[inst[2]]], inst[1]))
  cranes2[[inst[2]]] <- head(cranes2[[inst[2]]], -inst[1])
}
map_chr(cranes, ~ tail(., 1)) %>% paste(collapse = "")
map_chr(cranes2, ~ tail(., 1)) %>% paste(collapse = "")
