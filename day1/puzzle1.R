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

puzzle1 <- function(nums) {

  diff <- 2020 - nums

  in_there <- map_lgl(diff, ~.x %in% nums)

  num2 <- nums[min(which(in_there))]
  num1 <- 2020 - num2

  ans <- num1 * num2
  ans
}

puzzle1(nums)
