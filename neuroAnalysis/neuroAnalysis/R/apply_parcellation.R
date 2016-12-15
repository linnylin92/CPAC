#' Apply parcellation
#'
#' @param arr functional data of subject
#' @param parcel_arr parcellation
#'
#' @return a matrix with number of rows equal to number of time slices and number
#' or columns equal to number of parcellations
#' @export
apply_parcellation <- function(arr, parcel_arr){
  stopifnot(all(dim(arr)[1:3] == dim(parcel_arr)))
  stopifnot(length(dim(arr)) == 4)

  idx <- sort(unique(parcel_arr)); idx <- idx[idx != 0]
  mat <- matrix(0, dim(arr)[4], length(idx))

  for(i in 1:length(idx)){
    loc <- which(parcel_arr == idx[i])
    arr_subset <- t(apply(arr, 4, function(x){x[loc]}))
    non_zero <- apply(arr_subset, 2, function(x){sum(abs(x))})
    mat[,i] <- apply(arr_subset[,non_zero != 0], 1, mean)
  }

  mat
}
