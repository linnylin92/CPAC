#' Estimate Graph
#'
#' @param mat Input matrix
#' @param heuristic Heuristic to do model selection
#' @param ... Additional parameters for heuristic
#'
#' @return Graph encoded as sparse matrix
#' @export
estimate_graph <- function(mat, heuristic = graph_heuristic_percentage, ...){
  res <- huge::huge(mat)

  idx <- heuristic(res$path, ...)
  res$path[[idx]]
}

#' Graph Heurisitic
#'
#' @param mat_list List of graphs encoded as sparse matrices
#' @param percentage Target sparsity percentage
#'
#' @return Index of the mat_list
#' @export
graph_heuristic_percentage <- function(mat_list, percentage = 0.2){
  sparsity_vec <- lapply(mat_list, function(x){
    round(sum(x)/2)
  })

  which.min(abs(sparsity_vec - percentage))
}
