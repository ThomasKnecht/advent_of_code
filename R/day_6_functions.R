remove_empty_chr <- function(x){
  x <- x[x != ""]

  return(x)
}



solve_single_problem_part_1 <- function(problem_list, index){

  numbers <- extract_by_index(index, problem_list) |>
    as.numeric()

  problem_sign <- problem_list[[5]][index]

  result <- sum_or_prod(numbers, problem_sign)

  return(result)
}


solve_single_problem_part_2 <- function(problem_list, n_char){

  numbers_splittet <- strsplit(problem_list, "")

  extract_vector <- 1:n_char

  problem_sign <- numbers_splittet[[5]][1]

  numbers <- purrr::map_dbl(
    extract_vector,
    ~extract_numbers(.x, numbers_splittet)
  )

  result <- sum_or_prod(numbers, problem_sign)

  return(result)
}


get_max_char_per_column <- function(problem_list, index){

  column_nchars <- extract_by_index(index, problem_list) |>
    nchar()

  out <- max(column_nchars)

  return(out)
}



extract_numbers <- function(index, problem_list){
  number <- extract_by_index(index, problem_list) |>
    as.numeric()

  number <- as.numeric(paste0(number[!is.na(number)], collapse = ""))

  return(number)
}


extract_by_index <- function(index, problem_list){
  number <- c(
    problem_list[[1]][index],
    problem_list[[2]][index],
    problem_list[[3]][index],
    problem_list[[4]][index]
  )

  return(number)
}

sum_or_prod <- function(numbers, problem_sign){
  if(problem_sign == "+"){
    out <- sum(numbers)
  }else if(problem_sign == "*"){
    out <- prod(numbers)
  }

  return(out)
}
