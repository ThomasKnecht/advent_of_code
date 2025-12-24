create_min_max <- function(id_range){

  id_range_limits <- as.numeric(strsplit(id_range, "-", fixed = TRUE)[[1]])

  id_range_limits <- data.frame(
    id_range = id_range,
    min = id_range_limits[1],
    max = id_range_limits[2]
  )

  return(id_range_limits)

}


check_between <- function(number, idgredient_ranges){

  idgredient_ranges <- idgredient_ranges |>
    dplyr::mutate(
      to_check = number
    ) |>
    dplyr::mutate(
      check = dplyr::between(to_check, min, max)
    )

  if(any(idgredient_ranges$check)){
    return(1)
  }else{
    return(0)
  }
}
