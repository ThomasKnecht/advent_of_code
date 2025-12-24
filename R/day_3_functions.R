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


extract_joltage_12 <- function(battery_bank){

  single_batteries <- as.numeric(unlist(strsplit(battery_bank, "", fixed = TRUE)))

  iteration <- c(12:1)

  values <-c()

  rest <- single_batteries

  for(i in iteration){
    start_index <- which.max(rest)
    max <- max(rest)

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
