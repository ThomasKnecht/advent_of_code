# Day 5

# https://adventofcode.com/2025/day/5

options(scipen = 0, digits = 15)

# Load Data

database <- readLines("puzzle_data/advent_of_code_5.txt")


# Prepare Data
fresh_ingredient_ranges <- database[grepl("-", database)]

ingredients <- tail(as.numeric(database[!grepl("-", database)]), -1)

all_fresh_id_ranges <- purrr::map_df(fresh_ingredient_ranges, create_min_max) |>
  dplyr::arrange(min)


# Part 1


fresh_ingredients_from_fridge <- purrr::map_int(
  ingredients, ~check_between(.x, all_fresh_id_ranges)
)

solution_part_5_1 <- sum(fresh_ingredients_from_fridge)


# Part 2

combined_fresh_id_ranges <- all_fresh_id_ranges |>
  dplyr::mutate(
    indx = c(
      0,
      cumsum(as.numeric(dplyr::lead(min)) > cummax(as.numeric(max)))[-dplyr::n()]
    )
  ) |>
  dplyr::group_by(indx) |>
  dplyr::summarise(min = min(min), max = max(max)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    max = max + 1
  ) |>
  dplyr::mutate(
    diff = max - min
  )

solution_part_5_2 <- sum(combined_fresh_id_ranges$diff)
