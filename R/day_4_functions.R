create_raster <- function(data){

  matrix_dimensions <- dim(data)

  raster_input = list()

  raster_input$x = 1:matrix_dimensions[1]
  raster_input$y = 1:matrix_dimensions[2]
  raster_input$z = data

  shelf_raster <- raster::raster(raster_input)

  return(shelf_raster)
}



find_positions_with_less_than_four_adjacent_rolls <- function(shelf_vector, n_col){
  all_in_a_row <- data.frame(
    cell = 1:length(shelf_vector),
    value = shelf_vector
  )

  # Source - https://stackoverflow.com/a
  # Posted by flodel
  # Retrieved 2025-12-17, License - CC BY-SA 3.0
  shelf_matrix <- matrix(shelf_vector, ncol = n_col, byrow = TRUE)

  shelf_raster <- create_raster(shelf_matrix)

  cells_to_look_at <- which(shelf_vector == 1)

  adjacent_positions <- as.data.frame(
    raster::adjacent(shelf_raster, cells=cells_to_look_at, directions=8)
  )

  adjacent_positions_with_value <- adjacent_positions |>
    dplyr::left_join(all_in_a_row, by = c("to" = "cell"))


  number_of_adjacent_rolls <- adjacent_positions_with_value |>
    dplyr::group_by(from) |>
    dplyr::summarise(n_adj_rolls = sum(value)) |>
    dplyr::ungroup() |>
    dplyr::filter(
      n_adj_rolls < 4
    )

  return(number_of_adjacent_rolls)
}
