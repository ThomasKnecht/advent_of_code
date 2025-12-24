

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
