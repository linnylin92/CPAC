#' Graph difference
#'
#' @param mat1 graph encoded as sparse matrix
#' @param mat2 graph encoded as sparse matrix
#'
#' @return The percentage difference and list of edges in one graph but not the other
#' @export
graph_difference <- function(mat1, mat2){
  idx <- which(mat1 != mat2)

  perc_diff <- length(idx)/(length(unique(c(which(mat1 != 0), which(mat2 != 0)))))

  idx_pos <- which(mat1 > mat2, arr.ind = T)
  idx_neg <- which(mat1 < mat2, arr.ind = T)

  x = list(percentage_diff = perc_diff, list1 = .prune_idx(idx_pos),
       list2 = .prune_idx(idx_neg))
}

.prune_idx <- function(mat){
  mat <- mat[mat[,1] > mat[,2],]
}
