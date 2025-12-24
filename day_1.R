# Day 1

# https://adventofcode.com/2025/day/1

# Load Data
combinations <- c("50",readLines("puzzle_data/advent_of_code_1.txt"))

combination_df <- data.frame(
  combination_char = combinations
)


# Part 1
combination_df_part_1 <- combination_df |>
  dplyr::mutate(
    combination_num = as.numeric(gsub("R|L", "", combination_char))
  ) |>
  dplyr::mutate(
    combination_num = ifelse(
      grepl("L", combination_char),
      as.numeric(paste0("-", combination_num)),
      combination_num
    )
  ) |>
  dplyr::mutate(
    combination_cumsum = cumsum(combination_num)
  ) |>
  dplyr::mutate(
    modulo_100 = combination_cumsum %% 100
  ) |>
  dplyr::mutate(
    zero_flag = ifelse(modulo_100 == 0, 1, 0)
  )


solution_part_1_1 <- sum(combination_df_part_1$zero_flag)

# Part 2
combination_df_part_2 <- combination_df |>
  tail(-1) |>
  dplyr::mutate(
    combination_num = as.numeric(gsub("R|L", "", combination_char))
  ) |>
  dplyr::mutate(
    combination_num_list = purrr::map(combination_num, ~rep(1, .x))
  ) |>
  tidyr::unnest_longer(combination_num_list) |>
  dplyr::mutate(
    combination_num_list = ifelse(
      grepl("L", combination_char),
      as.numeric(paste0("-", combination_num_list)),
      combination_num_list
    )
  ) |>
  dplyr::add_row(
    combination_char = NA,
    combination_num = NA,
    combination_num_list = 50,
    .before = 1
  ) |>
  dplyr::mutate(
    combination_cumsum = cumsum(combination_num_list)
  ) |>
  dplyr::mutate(
    modulo_100 = combination_cumsum %% 100
  ) |>
  dplyr::mutate(
    zero_flag = ifelse(modulo_100 == 0, 1, 0)
  )


solution_part_1_2 <- sum(combination_df_part_2$zero_flag)
