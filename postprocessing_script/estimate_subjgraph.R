#!/usr/bin/env Rscript
source("apply_parcellation.R")
source("estimate_graph.R")

###### STEP 0: read in data ########
args <- commandArgs(trailingOnly=TRUE)

dat <- oro.nifti::readNIfTI(paste0(args[2], args[1], "func.nii.gz"), reorient = F)@.Data

###### STEP 1: apply the parcellation #######

load("/tigress/HANLIU/mridata/CPAC_image_resource/AAL/AAL_3mm.RData")

dat <- apply_parcellation(dat, AAL_3mm)

###### STEP 2: estimate the graph #########

res <- estimate_graph(dat)

save(res, file=paste0(args[3], args[1], "graph.RData"))