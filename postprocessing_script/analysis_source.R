vecMatch <- function(x, want) {
  isTRUE(all.equal(x, want))
}

parcellation <- function(dat, parcel) {
  library(oro.nifti)
  timeseries = matrix(0, nrow = 150, ncol = 90)
  nonzero <- apply(dat, c(1,2,3), function( x ){sum(abs(x))})
  indices2 = which(nonzero!= 0, arr.ind=T)
  B = as.list(data.frame(t(indices2)))
  for (i in 1:90) {
    indices1 = which(parcel==i,arr.ind =T)
    A = as.list(data.frame(t(indices1)))
    indices = list();
    for (j in 1:length(A)) {
      #print(i)
      #print(Sys.time())
      if (any(sapply(B,vecMatch,A[[j]])) == TRUE) {
        indices = append(indices, j);
      }
    }
    for (j in 1:150) {
      for (k in indices) {
        timeseries[j,i] = timeseries[j,i]+dat[append(indices[k],j)];
      }
      timeseries[j,i]= timeseries[j,i]/length(indices);
    }
  }
}