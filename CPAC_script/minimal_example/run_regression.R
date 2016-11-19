args <- commandArgs(TRUE)
seed <- as.numeric(args[[1]])

set.seed(seed)
x <- rbind(matrix(rnorm(100), ncol = 2),
   matrix(rnorm(100,5), ncol = 2))

cl <- kmeans(x, 2)
save.image(paste0("results/", seed, ".RData"))
