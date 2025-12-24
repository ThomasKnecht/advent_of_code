# Day 4

# https://adventofcode.com/2025/day/4

# Load Data

all_shelfes <- readLines("puzzle_data/advent_of_code_4.txt")

# Prepare Data
shelfes_numeric <- gsub("@", "1", all_shelfes)
shelfes_numeric <- gsub("\\.", "0", shelfes_numeric)

shelfes_vector <- unlist(
  purrr::map(shelfes_numeric, ~as.numeric(strsplit(.x, "", fixed = TRUE)[[1]]))
)


# Part 1

number_of_adjacent_rolls <- find_positions_with_less_than_four_adjacent_rolls(
  shelfes_vector,
  nchar(shelfes_numeric[1])
)

solution_part_4_1 <- nrow(number_of_adjacent_rolls)

# Part 2


init_vector <- shelfes_vector

# placeholder for the beginning
n_deleted <- 10

n_deleted_vector <- c()

while(n_deleted > 0){
  number_of_adjacent_rolls <- find_positions_with_less_than_four_adjacent_rolls(
    init_vector,
    nchar(shelfes_numeric[1])
  )

  n_deleted <- nrow(number_of_adjacent_rolls)

  n_deleted_vector <- c(n_deleted_vector, n_deleted)

  init_vector[number_of_adjacent_rolls$from] <- 0
}


total_deleted <- sum(n_deleted_vector)

