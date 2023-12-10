library(tidyverse)

read_lines("input16") %>%
  str_extract_all("[A-Z]{2}|\\d+") -> input

valves <- list()
for (i in input) {
  for (neigh in tail(i, -2)) {
    valves[[length(valves) + 1]] <- c(name = i[1], rate = i[2], neigh = neigh)
  }
}
valves <- bind_rows(valves) %>%
  mutate(rate = as.numeric(rate)) %>%
  arrange(desc(rate)) %>%
  mutate(name = factor(name, levels = unique(name))) %>%
  mutate(neigh = factor(neigh, levels = levels(name)))

routes <- tibble(current = "AA", cum_rate = 0, active = 0, release = 0)
for (i in 1:30) {
  # Compute released pressure and consider all neighbors
  routes %>%
    mutate(release = release + cum_rate) %>%
    left_join(valves, by = c("current" = "name"), relationship = "many-to-many") -> dest
  # Move
  dest %>%
    mutate(current = neigh) %>%
    select(-rate, -neigh) -> moves
  # Activate valve
  dest %>%
    filter(rate != 0) %>% # Jammed
    filter(!bitwAnd(active, 2^as.numeric(current))) %>% # Already active
    mutate(active = active + 2^as.numeric(current)) %>%
    mutate(cum_rate = cum_rate + rate) %>%
    select(-rate, -neigh) -> opening
  rbind(moves, opening) %>%
    group_by(current, active) %>%
    summarise(cum_rate = min(cum_rate), release = max(release), .groups = "drop") -> routes
  if (i == 26)
    routes_single <- routes %>%
    group_by(active) %>%
    summarise(release = max(release))
}
print(max(routes$release))

routes_single %>%
  expand_grid(routes_single, .name_repair = "unique") %>%
  filter(!bitwAnd(active...1, active...3)) %>%
  summarise(release = max(release...2 + release...4)) %>%
  print
