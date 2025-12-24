# Day 2

# https://adventofcode.com/2025/day/2

# Laod Data
id_ranges_chr <- readLines("puzzle_data/advent_of_code_2.txt")

id_ranges <- strsplit(id_ranges_chr, ",")[[1]]

all_ids <- purrr::map_df(id_ranges, split_and_expand)

# Part 1
doubled_pattern_ids <-  all_ids |>
  dplyr::mutate(
    ids_chr = as.character(id)
  ) |>
  dplyr::mutate(
    n_chr = nchar(ids_chr)
  ) |>
  dplyr::mutate(
    even = n_chr %% 2
  ) |>
  dplyr::filter(
    even == 0
  ) |>
  dplyr::mutate(
    n_chr_half = n_chr / 2
  ) |>
  dplyr::mutate(
    first_part = substr(ids_chr, 1, n_chr_half),
    second_part = substr(ids_chr, n_chr_half+1, n_chr)
  ) |>
  dplyr::mutate(
    equal_flag = ifelse(
      first_part == second_part,
      1,
      0
    )
  ) |>
  dplyr::filter(
    equal_flag == 1
  )


solution_part_2_1 <- sum(doubled_pattern_ids$id)


# Part 2

multiple_pattern_ids <- all_ids |>
  dplyr::mutate(
    ids_chr = as.character(id)
  ) |>
  dplyr::mutate(
    id_length = nchar(ids_chr)
  ) |>
  dplyr::mutate(
    pattern = sub("(.+?)\\1+", "\\1", id)
  ) |>
  dplyr::mutate(
    n_pattern = stringr::str_count(id, pattern)
  ) |>
  dplyr::filter(
    n_pattern > 1
  ) |>
  dplyr::mutate(
    test = id_length %% n_pattern
  ) |>
  dplyr::filter(
    test == 0
  )

solution_part_2_2 <- sum(multiple_pattern_ids$id)


