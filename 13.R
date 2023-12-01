library(tidyverse)

read_file("input13") %>%
  str_split_1("\\n\\n") -> input

lower_than <- function(first, second) {
  while (TRUE) {
    if (str_length(first) == 0)
      return (TRUE)
    if (str_length(second) == 0)
      return (FALSE)
    first_head <- str_sub(first, end = 1)
    second_head <- str_sub(second, end = 1)
    if (first_head == "[" && second_head == "[" ||
        first_head == "]" && second_head == "]" ||
        first_head == "," && second_head == ",") {
      first <- str_sub(first, 2)
      second <- str_sub(second, 2)
      next
    }
    if (first_head == "," || second_head == "]")
      return (FALSE)
    if (second_head == "," || first_head == "]")
      return (TRUE)
    if (first_head == "[") {
      value <- str_extract(second, "\\d+")
      second <- str_replace(second, value, str_c("[", value, "]"))
      next
    }
    if (second_head == "[") {
      value <- str_extract(first, "\\d+")
      first <- str_replace(first, value, str_c("[", value, "]"))
      next
    }
    value1 <- str_extract(first, "\\d+")
    value2 <- str_extract(second, "\\d+")
    if (as.numeric(value1) < as.numeric(value2))
      return (TRUE)
    if (as.numeric(value1) > as.numeric(value2))
      return (FALSE)
    first <- str_replace(first, value1, "")
    second <- str_replace(second, value1, "")
  }
}

index_sum <- 0
for (i in 1:length(input)) {
  pair <- input[i] %>% str_split_1("\\n")
  if (lower_than(pair[1], pair[2]))
    index_sum <- index_sum + i
}
print(index_sum)

read_file("input13") %>%
  str_replace_all("\\n\\n", "\n") %>%
  str_split_1("\\n") -> input
input <- head(input, -1)

place2 <- 0
place6 <- 0
for (i in input) {
  if (lower_than(i, "[[2]]"))
    place2 <- place2 + 1
  if (lower_than(i, "[[6]]"))
    place6 <- place6 + 1
}
print((place2 + 1) * (place6 + 2))
