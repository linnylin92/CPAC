<<<<<<< HEAD
#' Estimate Graph
#'
#' @param mat Input matrix
#' @param heuristic Heuristic to do model selection
#' @param ... Additional parameters for heuristic
#'
#' @return Graph encoded as sparse matrix
#' @export
estimate_graph <- function(mat, heuristic = graph_heuristic_percentage, ...){
  res <- huge::huge(mat, verbose = F)

  idx <- heuristic(res$sparsity, ...)
  res$path[[idx]]
}

#' Graph Heurisitic
#'
#' @param vec vector of sparsity values
#' @param percentage Target sparsity percentage
#'
#' @return Index of the mat_list
#' @export
graph_heuristic_percentage <- function(vec, percentage = 0.1){
  which.min(abs(vec[-1] - percentage))+1
=======
' Estimate Graph
#'
#' @param mat Input matrix
#' @param heuristic Heuristic to do model selection
#' @param ... Additional parameters for heuristic
#'
#' @return Graph encoded as sparse matrix
#' @export
estimate_graph <- function(mat, heuristic = graph_heuristic_percentage, ...){
  res <- huge::huge(mat, verbose = F)

  idx <- heuristic(res$sparsity, ...)
  res$path[[idx]]
}

#' Graph Heurisitic
#'
#' @param vec vector of sparsity values
#' @param percentage Target sparsity percentage
#'
#' @return Index of the mat_list
#' @export
graph_heuristic_percentage <- function(vec, percentage = 0.1){
  which.min(abs(vec[-1] - percentage))+1
>>>>>>> 8418f7e818e635367e5d676f7f8146440e26612b
}