#!/usr/bin/env Rscript

###### STEP 1: read in csv containing labels

args <- commandArgs(trailingOnly=TRUE)

library(Matrix)
source("median_graph.R")
source("graph_difference.R")

MyData <- read.csv(file=args[2], header=TRUE, sep=",")
colnames(MyData)[5]<-"labels"
case_subj = MyData[,1][which(MyData$labels == "Patient")]
control_subj = MyData[,1][which(MyData$labels == "Control")]

# remove subjects not found
current_files <- list.files(args[1])
loc <- sapply(case_subj, function(x){length(grep(x, current_files))})
case_subj <- case_subj[loc != 0]

loc <- sapply(control_subj, function(x){length(grep(x, current_files))})
control_subj <- control_subj[loc != 0]


###### STEP 2: read in all the graphs for each of the subjects

case_graph_list = vector("list", length(case_subj))
control_graph_list = vector("list", length(control_subj))

for (idx in 1:length(case_subj)) {
    load(paste0(args[1],"025-00", case_subj[idx], "/graph.RData"))
	case_graph_list[[idx]]= res;
}

for (idx in 1:length(control_subj)) {
	load(paste0(args[1],"025-00", control_subj[idx], "/graph.RData"));
	control_graph_list[[idx]]= res;
}

###### STEP 4: compute median graph ##########

case_median <- median_graph(case_graph_list)
control_median <- median_graph(control_graph_list)

###### STEP 5: compute the difference #########

graph_difference(control_median, case_median)
save(graph_difference, file=paste0(args[3], "/graph.RData"))
