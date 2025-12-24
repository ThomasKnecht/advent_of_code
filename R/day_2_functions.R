split_and_expand <- function(id_range){

  range <- strsplit(id_range, "-", fixed = TRUE)[[1]]

  all_ids <- data.frame(
    id = seq(
      as.numeric(range[1]),
      as.numeric(range[2]),
      1
    )
  )

  return(all_ids)
}


