#########################
# Simulation Study
#########################
source("/tigress/HANLIU/mridata/test_dir/PFC.R")

args <- commandArgs(TRUE)
n <- as.numeric(args[[1]])
p <- as.numeric(args[[2]])
seed <- as.numeric(args[[3]])


lambdas <- seq(0.001,2.5,by=0.015)*sqrt(log(p)/n)
res <- onebitsim(n,p,lambdas,seed)

save(res,file=paste("/tigress/HANLIU/mridata/test_dir/scenario1/n",n,"p",p,"seed",seed,".RData",sep=""))
