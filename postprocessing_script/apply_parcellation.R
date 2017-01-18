parcellation <- function(dat, parcel) {
  idx <- sort(unique(as.numeric(parcel))); idx <- idx[idx != 0]
  timeseries <- matrix(0, dim(dat)[4], length(idx))
  
  for (i in 1:length(idx)) {
    loc = which(parcel==i)
    dat_subset <- t(apply(dat, 4, function(x){x[loc]}))
    non_zero <- apply(dat_subset, 2, function(x){sum(abs(x))})
    timeseries[,i] <- apply(dat_subset[,non_zero != 0, drop = F], 1, mean)
  }
  timeseries
}
