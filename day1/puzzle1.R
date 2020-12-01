# puzzle1.R
# find 2 numbers that sum to 2020, multiply them, and that's the answer.

library(tidyverse)

nums <- readr::read_csv("day1/input.txt", col_names = "num") %>%
  arrange(desc(num)) %>%
  pull(num)

diff <- 2020 - nums

in_there <- map_lgl(diff, ~.x %in% nums)
length(which(in_there))
# 2 answers for me!

num2 <- nums[min(which(in_there))]
num1 <- 2020 - num2

ans <- num1 * num2

# check
num1 %in% nums
num2 %in% nums
which(num1 == nums) != which(num2 == nums)

# as a function

day1_part1 <- function(nums, total) {

  diff <- total - nums

  in_there <- map_lgl(diff, ~.x %in% nums)

  num2 <- nums[min(which(in_there))]
  num1 <- total - num2

  # list of numbers for answer:
  c(num1, num2)
}

reduce(day1_part1(nums, 2020), `*`)

### part 2

try_all_3s <- map(1:length(nums), ~day1_part1(nums[-.x], 2020 - nums[[.x]]))

which_worked <- map_lgl(try_all_3s, ~!is.na(.x[1])) %>% which() %>% min()

ans2 <- c(nums[which_worked], try_all_3s[[which_worked]])
reduce(ans2, `*`)
