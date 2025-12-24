# Day 7

# https://adventofcode.com/2025/day/7

# Load Data

flowchart <- readLines("puzzle_data//advent_of_code_7.txt")

# Prepare Data

flowchart_reduced <- flowchart[grepl("\\^", flowchart)]

flowchart_numbered <- gsub("\\^", "1", flowchart_reduced)
flowchart_numbered <- gsub("\\.", "0", flowchart_numbered)

flowchart_splittet <- strsplit(flowchart_numbered, "", fixed = TRUE)

flowchart_splittet <- purrr::map(flowchart_splittet, as.numeric)

# Part 1

number_splits_vec <- c()

vec1 <- flowchart_splittet[[1]]

zero_vec <- rep(0, 141)

for(i in 1:length(flowchart_splittet)){

  vec2 <- flowchart_splittet[[i]]

  start <- which(vec1 == 1)

  split <- which(vec2 == 1)

  used_splits <- intersect(unique(start), unique(split))

  number_splits <- length(used_splits)

  number_splits_vec <- c(number_splits_vec, number_splits)

  possible_paths <- c(used_splits-1, used_splits+1)

  new_start <- c(unique(possible_paths), setdiff(start, split))

  vec1 <- zero_vec
  vec1[new_start] <- 1
}

solution_part_7_1 <- sum(vec1)

# Part 2


vec1 <- flowchart_splittet[[1]]

zero_vec <- rep(0, 141)

for(i in 1:length(flowchart_splittet)){

  vec2 <- flowchart_splittet[[i]]

  start <- which(vec1 > 0)

  split <- which(vec2 == 1)

  used_splits <- intersect(unique(start), unique(split))

  no_splits <- setdiff(start, split)

  before_not_to_split <- data.frame(
    index = no_splits,
    value = vec1[no_splits]
  )

  before_to_split <- purrr::map(
    used_splits, ~list(
      index = .x,
      value = vec1[.x]
    )
  )

  split_path <- function(before_entry){
    data.frame(
      index = c(before_entry$index-1, before_entry$index+1),
      value =  rep(before_entry$value, 2)
    )
  }

  after <- purrr::map_df(before_to_split, split_path) |>
    dplyr::summarize(value = sum(value),
                     .by = index)

  new_start <- dplyr::bind_rows(after, before_not_to_split) |>
    dplyr::summarize(value = sum(value), .by = index)

  vec1 <- zero_vec

  for(i in 1:nrow(new_start)){
    vec1[new_start$index[i]] <- new_start$value[i]
  }

}

solution_part_7_2 <- sum(vec1)

