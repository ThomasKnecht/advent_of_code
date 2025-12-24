# check_within <- function(ind, red_tile_matrix, red_tile_polygon){
#   matrix_to_expand <- red_tile_matrix[c(ind$V1,ind$V2),]
#
#   if(length(unique(matrix_to_expand[,1])) == 1 | length(unique(matrix_to_expand[,2])) == 1){
#     NULL
#   }else{
#     rectangle_df <- expand.grid(matrix_to_expand[,1], matrix_to_expand[,2])
#
#     rectangle_df <- rectangle_df[c(1,2,4,3),]
#
#     rectangle_sf <- rectangle_df |>
#       sf::st_as_sf(coords = c("Var1", "Var2")) |>
#       dplyr::summarise(geometry = sf::st_combine(geometry)) |>
#       sf::st_cast("POLYGON")
#
#     rectangle_within <- sf::st_contains(red_tile_polygon, rectangle_sf)
#
#     rectangle_sf <- NULL
#     red_tiles_polygon <- NULL
#     rectangle_df <- NULL
#     matrix_to_expand <- NULL
#     red_tile_matrix <- NULL
#
#     if(length(rectangle_within[[1]]) != 0){
#       rectangle_within <- NULL
#       oud_df <- data.frame(
#         ind_1 = ind_1,
#         ind_2 = ind_2,
#         within = TRUE
#       )
#     }
#     return(out_df)
#   }
# }
