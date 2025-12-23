library(dplyr)
library(tidyr)

combination <- c("50",readLines("~/Documents/advent_of_code_combination.txt"))


# combination <- c(
#   "50",
#   "L68",
#   "L30",
#   "R48",
#   "L5",
#   "R60",
#   "L55",
#   "L1",
#   "L99",
#   "R14",
#   "L82"
# )

combination_df <- data.frame(
  combination_char = combination
)



combination_df_final_2 <- combination_df |>
  mutate(
    combination_num = as.numeric(gsub("R|L", "", combination_char))
  ) |>
  mutate(
    combination_num = ifelse(
      grepl("L", combination_char),
      as.numeric(paste0("-", combination_num)),
      combination_num
    )
  ) |>
  mutate(
    combination_cumsum = cumsum(combination_num)
  ) |>
  mutate(
    modulo_100 = combination_cumsum %% 100
  ) |>
  # mutate(
  #   lag_modulo_100 = lag(modulo_100)
  # ) |>
  mutate(
    zero_flag = ifelse(modulo_100 == 0, 1, 0)
  )
# |>
#   mutate(
#     n_hundreds = floor(combination_cumsum/100)
#   ) |>
#   mutate(
#     n_hundreds_lag = lag(n_hundreds)
#   ) |>
#   mutate(
#     diff_n_hundreds = abs(n_hundreds - n_hundreds_lag)
#   ) |>
#   mutate(
#     final_count = ifelse(
#       zero_flag == 1 & diff_n_hundreds == 0,
#       zero_flag,
#       diff_n_hundreds
#     )
#   ) |>
#   mutate(
#     final_count = ifelse(
#       zero_flag == 1 & diff_n_hundreds  > 1 & combination_cumsum > 0 & combination_num < 0,
#       diff_n_hundreds + zero_flag,
#       final_count
#     )
#   ) |>
#   # mutate(
#   #   final_count = ifelse(
#   #     zero_flag == 1 & diff_n_hundreds  > 1 & combination_cumsum < 0 & combination_num > 0,
#   #     diff_n_hundreds + zero_flag,
#   #     final_count
#   #   )
#   # ) |>
#   # mutate(
#   #   final_count = ifelse(
#   #     lag_modulo_100 == 0 & combination_num > 0 & combination_cumsum < 0,
#   #     final_count - 1,
#   #     final_count
#   #   )
#   # ) |>
#   mutate(
#     final_count = ifelse(
#       lag_modulo_100 == 0 & combination_num < 0,
#       final_count - 1,
#       final_count
#     )
#   ) |>
#   select(
#     combination_cumsum,
#     combination_num,
#     diff_n_hundreds,
#     zero_flag,
#     final_count
#   )














# combination_df_final <- combination_df |>
#   mutate(
#     combination_num = as.numeric(gsub("R|L", "", combination_char))
#   ) |>
#   mutate(
#     combination_modulo = abs(combination_num) %% 100
#   ) |>
#   mutate(
#     combination_modulo = ifelse(
#       grepl("L", combination_char),
#       as.numeric(paste0("-", combination_modulo)),
#       combination_modulo
#     )
#   ) |>
#   mutate(
#     combination_cumsum = cumsum(combination_modulo)
#   ) |>
#   mutate(
#     result_modulo = abs(combination_cumsum) %% 100
#   ) |>
#   mutate(
#     result_modulo = ifelse(
#       combination_cumsum < 0,
#       as.numeric(paste0("-", result_modulo)),
#       result_modulo
#     )
#   ) |>
#   mutate(
#     zero_flag = ifelse(result_modulo == 0, 1, 0)
#   )
#
# result <- sum(combination_df_final$zero_flag)
#

# combination_df_final_2 <- combination_df_final |>
#   mutate(
#     hundreds = abs(combination_cumsum - result_modulo)/100
#   ) |>
#   mutate(
#     lead_hundreds = lead(hundreds)
#   ) |>
#   mutate(
#     diff_hundreds = lead_hundreds - hundreds
#   )

combination_df_final_3 <- combination_df |>
  tail(-1) |>
  mutate(
    combination_num = as.numeric(gsub("R|L", "", combination_char))
  ) |>
  mutate(
    combination_num_list = purrr::map(combination_num, ~rep(1, .x))
  ) |>
  unnest_longer(combination_num_list) |>
  mutate(
    combination_num_list = ifelse(
      grepl("L", combination_char),
      as.numeric(paste0("-", combination_num_list)),
      combination_num_list
    )
  ) |>
  add_row(
    combination_char = NA,
    combination_num = NA,
    combination_num_list = 50,
    .before = 1
  ) |>
  mutate(
    combination_cumsum = cumsum(combination_num_list)
  ) |>
  mutate(
    modulo_100 = combination_cumsum %% 100
  ) |>
  mutate(
    zero_flag = ifelse(modulo_100 == 0, 1, 0)
  )


###############################################################################


# day 2

id_ranges_chr <- readLines("~/Documents/advent_of_code_2.txt")

id_ranges <- strsplit(id_ranges_chr, ",")[[1]]


split_and_expand <- function(id_range){

  range <- strsplit(id_range, "-", fixed = TRUE)[[1]]

  start_length <- nchar(range[1])
  end_length <- nchar(range[2])

  if(start_length == end_length){
    if(start_length %% 2 != 0 && end_length %% 2 != 0){
      return(NULL)
    }
  }


  all_ids <- data.frame(
    id = seq(
      as.numeric(range[1]),
      as.numeric(range[2]),
      1
    )
  ) |>
    dplyr::mutate(
      ids_chr = as.character(id)
    ) |>
    dplyr::mutate(
      n_chr = nchar(ids_chr)
    ) |>
    dplyr::mutate(
      gerade = n_chr %% 2
    ) |>
    dplyr::filter(
      gerade == 0
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

  return(all_ids)
}


ids_1 <- purrr::map_df(id_ranges, split_and_expand)


result_ids_1 <- sum(ids_1$ids)


# Part 2


find_pattern <- function(id_range){

  range <- strsplit(id_range, "-", fixed = TRUE)[[1]]

  ids <- data.frame(
    id = as.character(seq(
      as.numeric(range[1]),
      as.numeric(range[2]),
      1
    ))
  ) |>
    dplyr::mutate(
      id_length = nchar(id)
    ) |>
    dplyr::mutate(
      pattern = sub("(.+?)\\1+", "\\1", id)
    ) |>
    dplyr::mutate(
      n_pattern = stringr::str_count(id, pattern)
    ) |>
    # dplyr::mutate(
    #   pattern_length = nchar(pattern)
    # ) |>
    # dplyr::filter(
    #   id_length != pattern_length
    # ) |>
    dplyr::filter(
      n_pattern > 1
    ) |>
    dplyr::mutate(
      test = id_length %% n_pattern
    ) |>
    dplyr::filter(
      test == 0
    )

  return(ids)
}





ids_2 <- purrr::map_df(id_ranges, find_pattern)

result_ids_2 <- sum(as.numeric(ids_2$id))


test1 <- dplyr::anti_join(ids_1, ids_2, by = c("ids_chr" = "id"))

test2 <- dplyr::anti_join(ids_2, ids_1, by = c("id" = "ids_chr"))


# day 3
###################################################################


# 1
#test <- "2235324222232244322422312234251333343425243363443152244111122632336242225745433452452451332445546443"

battery_banks <- readLines("~/Documents/advent_of_code_3.txt")

extract_joltage <- function(battery_bank){
  single_batteries <- as.numeric(unlist(strsplit(battery_bank, "", fixed = TRUE)))

  max_index <- which.max(single_batteries)

  if(max_index == 100){

    rest <- head(single_batteries, -1)

    max1 <- rest[which.max(rest)]

    max2 <- single_batteries[100]

  } else {

    rest <- tail(single_batteries,100-max_index)

    max1 <- single_batteries[max_index]

    max2 <- rest[which.max(rest)]

  }


  joltage <- as.numeric(paste0(max1, max2))

  return(joltage)
}


joltages <- purrr::map_dbl(battery_banks, extract_joltage)



# 2

battery_banks <- "2235324222232244322422312234251333343425243363443152244111122632336242225745433452452451332445546443"


battery_banks <- readLines("~/Documents/advent_of_code_3.txt")

extract_joltage_12 <- function(battery_bank){

  single_batteries <- as.numeric(unlist(strsplit(battery_bank, "", fixed = TRUE)))


  iteration <- c(12:1)

  values <-c()

  rest <- single_batteries

  for(i in iteration){
    start_index <- which.max(rest)
    max <- max(rest)

    # if(i == 6L){
    #   browser()
    # }


    while(start_index > length(rest)-i+1) {
      max <- max - 1

      start_index <- which(rest == max)[1]

      if(is.na(start_index)){
        start_index <- length(rest)
      }
    }



    if(length(rest)-start_index+1 == i){
      rest <- rest[start_index:length(rest)]

      values <- c(values,  rest)
      break
    } else {
      rest <- rest[(start_index+1):length(rest)]

      values <- c(values, max)
    }
  }

  joltage <- as.numeric(paste0(values, collapse = ""))


  return(joltage)
}

joltages12 <- purrr::map_dbl(battery_banks, extract_joltage_12)

total_joltage <- sum(joltages12)


# Day 4

# 1

all_shelfes <- readLines("~/Documents/advent_of_code_4.txt")


test <- gsub("@", "1", all_shelfes)
test <- gsub("\\.", "0", test)

splitted <- purrr::map(test, ~as.numeric(strsplit(.x, "", fixed = TRUE)[[1]]))

all_in_a_row <- data.frame(
  cell = 1:19044,
  value = unlist(splitted)
)

# Source - https://stackoverflow.com/a
# Posted by flodel
# Retrieved 2025-12-17, License - CC BY-SA 3.0
output <- matrix(unlist(splitted), ncol = 138, byrow = TRUE)

dat1 = list()

dat1$x = 1:138
dat1$y = 1:138
dat1$z = output

test_raster <- raster::raster(dat1)

cells_to_look_at <- which(all_in_a_row$value == 1)

test_adjacent <- as.data.frame(raster::adjacent(test_raster, cells=cells_to_look_at, directions=8) )

adjacent_with_value <- test_adjacent |>
  dplyr::left_join(all_in_a_row, by = c("to" = "cell"))


number_of_adjacent_rolls <- adjacent_with_value |>
  dplyr::group_by(from) |>
  dplyr::summarise(n_adj_rolls = sum(value)) |>
  dplyr::ungroup() |>
  dplyr::filter(
    n_adj_rolls < 4
  )

# 2

all_shelfes <- readLines("~/Documents/advent_of_code_4.txt")


all_shelfes <- gsub("@", "1", all_shelfes)
all_shelfes <- gsub("\\.", "0", all_shelfes)

splitted <- purrr::map(all_shelfes, ~as.numeric(strsplit(.x, "", fixed = TRUE)[[1]]))

init_vector <- unlist(splitted)

n_deleted <- 10

n_deleted_vector <- c()

while(n_deleted > 0){
  shelf_matrix <- matrix(init_vector, ncol = 138, byrow = TRUE)

  all_in_a_row <- data.frame(
    cell = 1:19044,
    value = init_vector
  )

  cells_to_look_at <- which(init_vector == 1)

  shelf_data = list()

  shelf_data$x = 1:138
  shelf_data$y = 1:138
  shelf_data$z = shelf_matrix

  shelf_raster <- raster::raster(shelf_data)

  adjacent_paper_rolls <- as.data.frame(raster::adjacent(shelf_raster, cells=cells_to_look_at, directions=8) )

  adjacent_with_value <- adjacent_paper_rolls |>
    dplyr::left_join(all_in_a_row, by = c("to" = "cell"))

  number_of_adjacent_rolls <- adjacent_with_value |>
    dplyr::group_by(from) |>
    dplyr::summarise(n_adj_rolls = sum(value)) |>
    dplyr::ungroup() |>
    dplyr::filter(
      n_adj_rolls < 4
    )

  n_deleted <- length(number_of_adjacent_rolls$from)

  n_deleted_vector <- c(n_deleted_vector, n_deleted)

  init_vector[number_of_adjacent_rolls$from] <- 0
}


total_deleted <- sum(n_deleted_vector)


# Day 5
################################################################################

# 1

options(scipen = 0, digits = 15)

database <- readLines("~/Documents/advent_of_code_5.txt")

fresh_ingredients <- database[grepl("-", database)]


ingredients <- tail(as.numeric(database[!grepl("-", database)]), -1)

create_all_ids <- function(id_range){

  id_range_limits <- as.numeric(strsplit(id_range, "-", fixed = TRUE)[[1]])

  #full_id_range <- id_range_limits[1]:id_range_limits[2]

  id_range_limits <- data.frame(
    id_range = id_range,
    min = id_range_limits[1],
    max = id_range_limits[2]
  )

  return(id_range_limits)

}


all_fresh_ids <- purrr::map_df(fresh_ingredients, create_all_ids) |>
  dplyr::arrange(min)

brute_force <- function(number, df){

  df <- df |>
    dplyr::mutate(
      to_check = number
    ) |>
    dplyr::mutate(
      check = dplyr::between(to_check, min_new, max_new)
    )

  if(any(df$check)){
    return(1)
  }else{
    return(0)
  }


}

brutre_force_df <- all_fresh_ids |>
  dplyr::select(
    min_new = min,
    max_new = max
  )

out1 <- purrr::map_int(ingredients, ~brute_force(.x, brutre_force_df))

# out_test <- purrr::map_int(ingredients, ~brute_force(.x, test))
#
# test_df <- data.frame(
#   ingredients = ingredients,
#   out1 = out1,
#   out_test = out_test
# ) |>
#   dplyr::mutate(
#     diff = out1 - out_test
#   )


# 2

test <- all_fresh_ids |>
  dplyr::mutate(indx = c(0, cumsum(as.numeric(dplyr::lead(min)) > cummax(as.numeric(max)))[-dplyr::n()])) |>
  dplyr::group_by(indx) |>
  dplyr::summarise(min = min(min), max = max(max)) |>
  dplyr::ungroup() |>
  # dplyr::rename(
  #   min_new = min,
  #   max_new = max
  # ) |>
  dplyr::mutate(
    max = max + 1
  ) |>
  dplyr::mutate(
    diff = max - min
  )

out4 <- sum(test$diff)


# Day 6

math_problems <- readLines("~/Documents/advent_of_code_6.txt")

math_problems_splitted <- strsplit(math_problems, " ", fixed = TRUE)

remove_empty_chr <- function(x){

  x <- x[x != ""]

  return(x)
}

math_problems_cleaned <- purrr::map(math_problems_splitted, remove_empty_chr)



solve_single_problem <- function(index, problem_list){

  problem_numbers <- c(
    as.numeric(problem_list[[1]][index]),
    as.numeric(problem_list[[2]][index]),
    as.numeric(problem_list[[3]][index]),
    as.numeric(problem_list[[4]][index])
  )

  if(problem_list[[5]][index] == "+"){
    out <- sum(problem_numbers)
  }else if(problem_list[[5]][index] == "*"){
    out <- prod(problem_numbers)
  }

  return(out)
}


index_vector <- 1:length(math_problems_cleaned[[1]])


results <- purrr::map_dbl(index_vector, ~solve_single_problem(.x, math_problems_cleaned))


# 2

get_max_char_per_column <- function(index, problem_list){

  column_nchars <- c(
    nchar(problem_list[[1]][index]),
    nchar(problem_list[[2]][index]),
    nchar(problem_list[[3]][index]),
    nchar(problem_list[[4]][index])
  )

  out <- max(column_nchars)

  return(out)
}

max_n_char_per_column <- purrr::map_dbl(index_vector, ~get_max_char_per_column(.x, math_problems_cleaned))



math_problems <- paste0(math_problems, " ")


start <- 1

out <- list()

for(i in max_n_char_per_column){

  end <- start + i -1

  problem_numbers <- substr(math_problems, start, end)

  out <- c(out, list(problem_numbers))

  start <- start + i + 1
}


extract_numbers <- function(index, vector){
  number <- c(
    as.numeric(vector[[1]][index]),
    as.numeric(vector[[2]][index]),
    as.numeric(vector[[3]][index]),
    as.numeric(vector[[4]][index])
  )

  number <- as.numeric(paste0(number[!is.na(number)], collapse = ""))

  return(number)
}

calculate_single_problem <- function(problem_vector, n_char){

  numbers_splittet <- strsplit(problem_vector, "")

  extract_vector <- 1:n_char

  problem_sign <- numbers_splittet[[5]][1]

  numbers <- purrr::map_dbl(
    extract_vector,
    ~extract_numbers(.x, numbers_splittet)
  )


  if(problem_sign == "+"){
    out <- sum(numbers)
  }else if(problem_sign == "*"){
    out <- prod(numbers)
  }

  return(out)
}


calculated_problems <- purrr::map2_dbl(
  out,
  max_n_char_per_column,
  ~calculate_single_problem(..1, ..2)
)

sum(calculated_problems)



# Day 7
################################################################################

# 1

flowchart <- readLines("~/Documents/advent_of_code_7.txt")

flowchart_reduced <- flowchart[grepl("\\^", flowchart)]

flowchart_numbered <- gsub("\\^", "1", flowchart_reduced)
flowchart_numbered <- gsub("\\.", "0", flowchart_numbered)

flowchart_splittet <- strsplit(flowchart_numbered, "", fixed = TRUE)

flowchart_splittet <- purrr::map(flowchart_splittet, as.numeric)

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






vec1 <- flowchart_splittet[[1]]

zero_vec <- rep(0, 141)

for(i in 1:length(flowchart_splittet)){

  vec2 <- flowchart_splittet[[i]]

  start <- which(vec1 > 0)

  split <- which(vec2 == 1)

  used_splits <- intersect(unique(start), unique(split))

  #number_splits <- length(used_splits)

  #umber_splits_vec <- c(number_splits_vec, number_splits)
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


# Day 8
################################################################################

# 1

all_points <- read.csv("~/Documents/advent_of_code_8.csv", header = FALSE)

names(all_points) <- c("X", "Y", "Z")

point_matrix <- as.matrix(all_points)


ind = t(combn(nrow(point_matrix), 2))
# ind = expand.grid(1:nrow(point_matrix), 1:nrow(point_matrix)) |>
#   dplyr::filter(Var1 != Var2) |>
#   as.matrix()


distances = apply(ind, 1, function(z){
  sqrt(sum((point_matrix[z[1],] - point_matrix[z[2], ])^2))
})

result = cbind(data.frame(ind), distances) |>
  dplyr::arrange(distances)

distances <- dist(point_matrix, method = "euclidean") |> as.matrix()
dims <- dim(distances)



tree <- hclust(dist(point_matrix), method = "single")
test <- data.frame(test = cutree(tree, h = 3500)) |>
  dplyr::count(test)


plot(tree)


# Day 9
################################################################################
# 1

#test_tiles <- read.csv("~/Documents/advent_of_code_9.csv", header = FALSE)
test_tiles <- read.csv("~/Documents/advent_of_code_9.csv", header = FALSE)

names(test_tiles) <- c("x", "y")

test_tiles <- test_tiles

point_matrix <- as.matrix(test_tiles)


ind = t(combn(nrow(point_matrix), 2))

area = apply(ind, 1, function(z){
  prod_matrix <- abs(point_matrix[z[1],] - point_matrix[z[2],]) + 1

  abs(prod(prod_matrix))
})

result = cbind(data.frame(ind), area) |>
  dplyr::arrange(area)

# 2
test_tiles_new <- rbind(test_tiles, head(test_tiles, 1))


test_polygon <- test_tiles_new |>
  sf::st_as_sf(coords = c("x", "y")) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("POLYGON")


ind_df <- as.data.frame(ind)

area_vec <- list()

for(i in 1:nrow(ind_df)){

  first <- ind_df$V1[i]
  second <- ind_df$V2[i]

  test <- test_tiles[c(first, second),]

  test_expanded <- tidyr::complete(test, x, y)
  test_expanded <- test_expanded[c(1,2,4,3),]

  if(!any(is.na(test_expanded$x))){
    test_polygon1 <- test_expanded|>
      sf::st_as_sf(coords = c("x", "y")) |>
      dplyr::summarise(geometry = sf::st_combine(geometry)) |>
      sf::st_cast("POLYGON")

    test_within <- sf::st_contains(test_polygon, test_polygon1)

    if(length(test_within[[1]]) != 0){
      #area <- sf::st_area(test_polygon1)

      out_df <- ind_df[i,] |>
        dplyr::mutate(within = TRUE)

      area_vec <- c(area_vec, list(out_df))
    }
  }
}

out_within_df <- do.call(rbind, area_vec)


find_finally_the_biggest <- result |>
  dplyr::left_join(out_within_df, by = c("X1" = "V1", "X2" = "V2")) |>
  dplyr::filter(within)


mapview::mapview(list(test_polygon, test_polygon1),col.regions=c("red","blue"))




# Day 10
################################################################################

# 1


# get all combinations
# press button function

# ind for each selections
# 1:length(buttons)

# t(combn(nrow(point_matrix), n))

# first plus second
# all that are 2 set to 0
