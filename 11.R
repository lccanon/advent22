library(tidyverse)

monkeys <- list()
items <- list()
for (line in read_lines("input11")) {
  if (str_detect(line, "^Monkey")) {
    monkeys[[length(monkeys) + 1]] <- list()
  } else if (str_detect(line, "Starting")) {
    items[[length(monkeys)]] <- str_extract_all(line, "\\d+", simplify = TRUE) %>% as.integer
  } else if (str_detect(line, "Operation")) {
    monkeys[[length(monkeys)]]$operator <- str_extract(line, "[+*]")
    monkeys[[length(monkeys)]]$operand <- str_extract(line, "\\d+") %>% as.integer
  } else if (str_detect(line, "Test")) {
    monkeys[[length(monkeys)]]$div <- str_extract(line, "\\d+") %>% as.integer
  } else if (str_detect(line, "If true")) {
    monkeys[[length(monkeys)]]$true <- str_extract(line, "\\d+") %>% as.integer
  } else if (str_detect(line, "If false")) {
    monkeys[[length(monkeys)]]$false <- str_extract(line, "\\d+") %>% as.integer
  }
}
mod <- prod(map_int(monkeys, ~ .$div))

inspections <- rep(0, length(monkeys))
for (k in seq_len(10000)) {
  for (i in seq_along(monkeys)) {
    inspections[i] <- inspections[i] + length(items[[i]])
    for (j in seq_along(items[[i]])) {
      item <- items[[i]][j]
      if (is.na(monkeys[[i]]$operand)) {
        new_item <- item * item
      } else if (monkeys[[i]]$operator == "+") {
        new_item <- item + monkeys[[i]]$operand
      } else if (monkeys[[i]]$operator == "*") {
        new_item <- item * monkeys[[i]]$operand
      } else
        stop("Impossible")
      # new_item <- new_item %/% 3
      new_item <- new_item %% mod
      next_monkey <- 1 + if (new_item %% monkeys[[i]]$div == 0) monkeys[[i]]$true else monkeys[[i]]$false
      stopifnot(i != next_monkey)
      items[[next_monkey]] <- c(items[[next_monkey]], new_item)
    }
    items[[i]] <- numeric(0)
  }
}
prod(rev(sort(inspections))[1:2])

# Alternative reading

input <- read_lines("input11")
items <- str_extract_all(input[0:7*7 + 2L], "\\d+") %>% map(as.integer)
operators <- str_extract(input[0:7*7 + 3L], "[+*]")
operands <- str_extract(input[0:7*7 + 3L], "\\d+") %>% as.integer
divs <- str_extract(input[0:7*7 + 4L], "\\d+") %>% as.integer
trues <- str_extract(input[0:7*7 + 5L], "\\d+") %>% as.integer
falses <- str_extract(input[0:7*7 + 6L], "\\d+") %>% as.integer

mod <- prod(div)

inspections <- rep(0, length(items))
for (k in seq_len(10000)) {
  for (i in seq_along(items)) {
    inspections[i] <- inspections[i] + length(items[[i]])
    for (j in seq_along(items[[i]])) {
      item <- items[[i]][j]
      if (is.na(operands[i])) {
        new_item <- item * item
      } else if (operators[i] == "+") {
        new_item <- item + operands[i]
      } else if (operators[i] == "*") {
        new_item <- item * operands[i]
      } else
        stop("Impossible")
      # new_item <- new_item %/% 3
      new_item <- new_item %% mod
      next_monkey <- 1 + if (new_item %% divs[i] == 0) trues[i] else falses[i]
      stopifnot(i != next_monkey)
      items[[next_monkey]] <- c(items[[next_monkey]], new_item)
    }
    items[[i]] <- numeric(0)
  }
}
prod(rev(sort(inspections))[1:2])
