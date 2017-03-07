#' Median graph
#'
#' @param mat_list list of graphs encoded as sparse matrices
#' @param heuristic heuristic to choose rank of value for median graph
#' @param ... additional parameters for median graph
#'
#' @return a graph encoded as sparse matrix
#' @export
library(changepoint)

median_graph <- function(mat_list, heuristic = bin_seg_heuristic, ...){
  sum_mat <- Reduce('+', mat_list)

  val <- heuristic(sum_mat, ...)
  print(val)
  idx <- which(as.matrix(sum_mat) >= val, arr.ind = T)

  median_graph <- matrix(0, ncol(mat_list[[1]]), nrow(mat_list[[1]]))
  median_graph[idx] <- 1

  Matrix::Matrix(median_graph, sparse = T)
}

#' Median heuristic
#'
#' @param sum_mat The matrix that with values equal to number of instances per edge
#' @param percentage threshold for how large the quantile is
#'
#' @return a value of number of instances to threshold below
#' @export
median_heuristic <- function(sum_mat, percentage = 0.9){
  val <- as.matrix(sum_mat)[upper.tri(sum_mat)]
  val <- val[val != 0]
  stats::quantile(val, probs = percentage)
}


#' Binary segmentation heuristic 
#' 
#' 

bin_seg_heuristic <- function(sum_mat) {
  threshold_vec = 1:max(sum_mat)
  sparsity_vec = rep(0, max(sum_mat))
  for (i in 1:length(threshold_vec)) {
    idx <- which(as.matrix(sum_mat) >= threshold_vec[i])
    sparsity_vec[i] = length(idx)/(choose(nrow(sum_mat),2))
  }
  #use wbs
  wbs_res <- wbs::wbs(sparsity_vec)
  wbs_changepoint_res <- wbs::changepoints(wbs_res)
  wbs_changepoint <- c(1, sort(wbs_changepoint_res$cpt.th[[1]]), length(threshold_seq))
  
  #find the largest gap
  gap_size <- diff(wbs_changepoint)
  gap_idx <- which.max(gap_size)
}
