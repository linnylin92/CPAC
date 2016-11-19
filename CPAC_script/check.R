#!/usr/bin/env Rscript
# lists all subjects in COBRE with the number of anatomic and functional scans

args = commandArgs(trailingOnly=TRUE)

setwd(args[1])

subjects = list.files(full.names = TRUE)

countmatrix = as.data.frame(matrix(0, ncol = 3, nrow = length(subjects)))
countmatrix[,1] = subjects
for (i in subjects){
    funccount = length(list.files(path = paste0(args[1], "/func")))
    anatcount = length(list.files(path = paste0(args[2], "/anat")))
    countmatrix[i,2] = anatcount
    countmatrix[i,3] = funccount  
}
    save(countmatrix, file = "/home/maksen/CPAC_script.git/countCOBRE.RData")