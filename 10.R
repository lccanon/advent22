library(tidyverse)

# Base version

read_file("input10") %>%
  str_split_1("[ \\n]") %>%
  as.integer -> X
X[is.na(X)] <- 0
X <- cumsum(c(1L, X))

strengths <- X * seq_along(X)
sum(strengths[seq(20, 220, 40)])

X <- head(X, 6 * 40)
pixels <- (abs(0:39 - X) <= 1) %>%
  matrix(ncol = 40, byrow = TRUE)
image(pixels[6:1,] %>% t)
ifelse(pixels, "X", " ") %>%
  apply(1, \(x) paste0(x, collapse = "")) %>%
  matrix(ncol = 1)

# Tidyverse version

read_csv("input10", col_names = c("inst")) %>%
  separate(inst, into = c("inst", "value2"), sep = " ") %>%
  mutate(inst = "0") %>%
  pivot_longer(everything()) %>%
  filter(!is.na(value)) %>%
  pull(value) %>%
  as.integer() -> X
X <- cumsum(c(1L, X))

rbind(1, read_csv("input10", col_names = c("inst")) %>%
        separate(inst, into = c("inst", "value2"), sep = " ") %>%
        mutate(inst = "0") %>%
        pivot_longer(everything()) %>%
        filter(!is.na(value)) %>%
        select(value) %>%
        mutate_all(as.integer)) %>%
  mutate(X = cumsum(value)) %>%
  mutate(index = 1:n()) -> insts
insts %>%
  filter(index %in% seq(20, 220, 40)) %>%
  summarise(strength = sum(X * index))
insts %>%
  head(6 * 40) %>%
  mutate(letter = ifelse(abs((index - 1) %% 40 - X) <= 1, "X", " ")) %>%
  group_by((index - 1) %/% 40) %>%
  summarise(chain = paste0(letter, collapse = "")) %>%
  select(chain)
