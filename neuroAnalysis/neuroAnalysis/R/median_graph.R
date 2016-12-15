#' Title
#'
#' @param mat_list
#' @param heuristic
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
median_graph <- function(mat_list, heuristic = median_heuristic, ...){
  sum_mat <- Reduce('+', mat_list)

  val <- heuristic(sum_mat, ...)
  idx <- which(sum_mat[upper.tri(sum_mat)] >= val)

  median_graph <- matrix(0, ncol(mat_list[[1]]), nrow(mat_list[[1]]))
  median_graph[idx] <- 2

  (median_graph + t(median_graph))/2
}

median_heuristic <- function(sum_mat, percentage = 0.8){
  quantile(sum_mat[upper.tri(sum_mat)], probs = percentage)
}
