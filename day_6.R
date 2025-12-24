# Day 6

# https://adventofcode.com/2025/day/6

# Load Data
math_problems <- readLines("puzzle_data/advent_of_code_6.txt")

math_problems_splitted <- strsplit(math_problems, " ", fixed = TRUE)

math_problems_cleaned <- purrr::map(
  math_problems_splitted,
  remove_empty_chr
)

index_vector <- 1:length(math_problems_cleaned[[1]])


# Part 1

math_problem_results <- purrr::map_dbl(
  index_vector,
  ~solve_single_problem_part_1(math_problems_cleaned, .x)
)

solution_part_6_1 <- sum(math_problem_results)

# Part 2

max_n_char_per_column <- purrr::map_dbl(
  index_vector,
  ~get_max_char_per_column(math_problems_cleaned, .x)
)

start <- 1

math_problem_list <- list()

for(i in max_n_char_per_column){

  end <- start + i -1

  problem_numbers <- substr(math_problems, start, end)

  math_problem_list <- c(math_problem_list, list(problem_numbers))

  start <- start + i + 1
}


math_problem_results <- purrr::map2_dbl(
  math_problem_list,
  max_n_char_per_column,
  ~solve_single_problem_part_2(..1, ..2)
)

solution_part_6_2 <- sum(math_problem_results)

