#!/usr/bin/env Rscript

###### STEP 1: read in csv containing labels

args <- commandArgs(trailingOnly=TRUE)

MyData <- read.csv(file=args[2], header=TRUE, sep=",")
colnames(MyData)[5]<-"labels"
case_subj = MyData[,1][which(MyData$labels == "Patient")]
control_subj = MyData[,1][which(MyData$labels == "Control")]

###### STEP 2: read in all the graphs for each of the subjects

case_graph_list = list()
control_graph_list = list()
for (subject in case_subj) {
    load(paste(args[1],"025-", subject, "/graph.RData"));
	case_graph_list[length(case_graph_list)+1]= res;
}

for (subject in control_subj) {
	load(paste(args[1],"025-", subject, "/graph.RData"));
	control_graph_list[length(control_graph_list)+1]= res;
}

###### STEP 4: compute median graph ##########

case_median <- median_graph(case_graph_list)
control_median <- median_graph(control_graph_list)

###### STEP 5: compute the difference #########

graph_difference(control_median, case_median)
save(graph_difference, file=paste0(args[3], "/graph.RData"))