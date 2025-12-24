# Day 9

# https://adventofcode.com/2025/day/9

# Load Data
red_tiles <- read.csv("puzzle_data/advent_of_code_9.csv", header = FALSE)

names(red_tiles) <- c("x", "y")


# Prepare Data
red_tile_matrix <- as.matrix(red_tiles)

ind = t(combn(nrow(red_tile_matrix), 2))

ind_df <- as.data.frame(ind)

# Part 1
area = apply(ind, 1, function(z){
  prod_matrix <- abs(red_tile_matrix[z[1],] - red_tile_matrix[z[2],]) + 1

  abs(prod(prod_matrix))
})

all_areas = cbind(data.frame(ind), area) |>
  dplyr::arrange(area)


solution_part_9_1 <- max(all_areas$area)

# Part 2

# for some reasons, there is a memory leak... couldn't figure out
# why. But somehow, the full environment is passed to the daemon and not
# only the crated function... Furthermore, the memory is not freed after
# the runners die...

red_tiles_circle <- rbind(red_tiles, head(red_tiles, 1))

red_tiles_polygon <- red_tiles_circle |>
  sf::st_as_sf(coords = c("x", "y")) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("POLYGON")

ind_list <- split(ind_df, 1:nrow(ind_df))

mirai::daemons(14)

fn <- carrier::crate(function(x) {

  matrix_to_expand <- red_tile_matrix[c(x$V1,x$V2),]

  if(length(unique(matrix_to_expand[,1])) == 1 | length(unique(matrix_to_expand[,2])) == 1){
    NULL
  }else{
    rectangle_df <- expand.grid(matrix_to_expand[,1], matrix_to_expand[,2])

    rectangle_df <- rectangle_df[c(1,2,4,3),]
    rectangle_sf <- rectangle_df |>
      sf::st_as_sf(coords = c("Var1", "Var2")) |>
      dplyr::summarise(geometry = sf::st_combine(geometry)) |>
      sf::st_cast("POLYGON")

    rectangle_within <- sf::st_contains(red_tile_polygon, rectangle_sf)

    matrix_to_expand <- NULL
    rectangle_sf <- NULL
    red_tile_polygon <- NULL
    rectangle_df <- NULL
    red_tile_matrix <- NULL

    if(length(rectangle_within[[1]]) != 0){
      rectangle_within <- NULL
      out_df <- tibble::tibble(
        ind_1 = x$V1,
        ind_2 = x$V2,
        within = TRUE
      )
      return(out_df)
    }
  }
  },
  red_tile_matrix = red_tile_matrix,
  red_tile_polygon = red_tiles_polygon
)


out <- mirai::mirai_map(
  ind_list[1:40],
  fn
)[.progress]

mirai::daemons(0)

all_rectangles <- data.table::rbindlist(out)


find_finally_the_biggest <- all_rectangles |>
  dplyr::left_join(all_areas, by = c("ind_1" = "X1", "ind_2" = "X2")) |>
  dplyr::arrange(area)

solution_part_9_2 <- max(find_finally_the_biggest$area)
