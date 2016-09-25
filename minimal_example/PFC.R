library(MASS)
############################################
# May 21, 2016 
# Principal Fitted Components 
# Tailored to Sliced Inverse Regression
############################################

############################################
# Simulation PFC
############################################
simpfc <- function(n,p,lambdas,seed){
	
	set.seed(seed)	
	Gamma <- matrix(c(0.5,0.5,0.5,0.5,rep(0,p-4),0.5,-0.5,0.5,-0.5,rep(0,p-4)),nrow=p,ncol=2,byrow=FALSE)
	
# Generate AR-1 type covariance for X
	Sigma <- matrix(0.5,p,p)
	tmpmat <- matrix(0,p,p)
	for(i in 1:(p-1)){
		tmpmat[i,i:p] <- c(0:(length(i:p)-1))
	}
	tmpmat = tmpmat+t(tmpmat)
	Sigma <- Sigma^tmpmat	

	y <- rnorm(n)
	epsilon <- mvrnorm(n=n,mu=rep(0,p),Sigma=Sigma)
	fy <- t(cbind(y,y^2))
	
	X <- t(Gamma%*%fy)+epsilon
	
	truesubspace <- solve(Sigma)%*%Gamma
	temp <- svd(truesubspace)
	truesubspace <- temp$u[,1:2]%*%t(temp$u[,1:2])

	nfold <- 5
	nslice <- 5
	SigmaxSIR <- cov(X)
	SigmaSIR <- calsigmafit(y,X,nslice=5)	
	
	Xfit <- X-apply(X,2,mean)
	Ffit <- t(fy)-apply(t(fy),2,mean)

	Sigmafit <- t(Xfit)%*%Ffit%*%solve(t(Ffit)%*%Ffit)%*%t(Ffit)%*%Xfit/n 
	Sigmax <- t(Xfit)%*%Xfit/(n-1)



mse <- correctzero <- correctnonzero <- specificity <- sensitivity <- sensitivitySIR <- specificitySIR <- NULL
	
	initPi = diag(1,p,p)
	initH = diag(1,p,p)
	initGamma = diag(0,p,p)

	initPiSIR = diag(1,p,p)
	initHSIR = diag(1,p,p)
	initGammaSIR = diag(0,p,p)


# For SIR

for(lambda in lambdas){
print(lambda)	

	res <- ssir(Sigmafit,Sigmax,lambda,K=2,epsilon=1e-03,init=TRUE,initPi=initPi,initH=initH,initGamma=initGamma)

	initPi = res$Pi
	initH = res$H
	initGamma = res$Gamma
	
	Pi <- res$Pi
	temp <- svd(Pi)
        est <- temp$u[,1:2]%*%t(temp$u[,1:2])
	specificity <- c(specificity,sum(abs(diag(Pi))<1e-5 & abs(diag(truesubspace))< 1e-5)/sum(diag(truesubspace)==0))
	sensitivity <- c(sensitivity,sum(abs(diag(Pi))>1e-5 & abs(diag(truesubspace))>1e-5)/sum(diag(truesubspace)!=0))


# SIR 

	res <- ssir(SigmaSIR,SigmaxSIR,lambda,K=2,epsilon=1e-03,init=TRUE,initPi=initPiSIR,initH=initHSIR,initGamma=initGammaSIR)

	initPiSIR = res$Pi
	initHSIR = res$H
	initGammaSIR = res$Gamma
	
	PiSIR <- res$Pi
	specificitySIR <- c(specificitySIR,sum(abs(diag(PiSIR))<1e-5 & abs(diag(truesubspace))< 1e-5)/sum(diag(truesubspace)==0))
	sensitivitySIR <- c(sensitivitySIR,sum(abs(diag(PiSIR))>1e-5 & abs(diag(truesubspace))>1e-5)/sum(diag(truesubspace)!=0))



	
}


	return(list(sensitivity=sensitivity,specificity=specificity,sensitivitySIR=sensitivitySIR,specificitySIR=specificitySIR))

	
	
}



############################################
# Simulation very easy case 
# linear regression
############################################
linearcase <- function(n,p,lambdas,seed){
	
	set.seed(seed)
	beta <- rep(0,p)
	beta[1:3] <- 1/sqrt(3)
	
# Generate AR-1 type covariance for X
	Sigma <- matrix(0.5,p,p)
	tmpmat <- matrix(0,p,p)
	for(i in 1:(p-1)){
		tmpmat[i,i:p] <- c(0:(length(i:p)-1))
	}
	tmpmat = tmpmat+t(tmpmat)
	Sigma <- Sigma^tmpmat	
	eigenSigma <- eigen(Sigma)
	sqrtSigma <- eigenSigma$vectors%*%diag(sqrt(eigenSigma$values))%*%t(eigenSigma$vectors)	
	truesubspace <- cbind(beta)%*%t(cbind(beta))
	temp <- svd(truesubspace)
	truesubspace <- temp$u[,1]%*%t(temp$u[,1])
	
	X <- mvrnorm(n=n,mu=rep(0,p),Sigma=Sigma)
	y <- X[,1]+X[,2]+X[,3]+2*rnorm(n,0,1)


	nfold <- 5
	nslice <- 5
	Sigmax <- cov(X)
	Sigmafit <- calsigmafit(y,X,nslice=5)	

mse <- correctzero <- correctnonzero <- specificity <- sensitivity <- NULL
	
	initPi = diag(1,p,p)
	initH = diag(1,p,p)
	initGamma = diag(0,p,p)
	
for(lambda in lambdas){
print(lambda)	

	res <- ssir(Sigmafit,Sigmax,lambda,K=1,epsilon=1e-03,init=TRUE,initPi=initPi,initH=initH,initGamma=initGamma)

	initPi = res$Pi
	initH = res$H
	initGamma = res$Gamma
	
	Pi <- res$Pi
	temp <- svd(Pi)
        est <- temp$u[,1]%*%t(temp$u[,1])
	specificity <- c(specificity,sum(abs(diag(Pi))<1e-5 & abs(diag(truesubspace))< 1e-5)/sum(diag(truesubspace)==0))
	sensitivity <- c(sensitivity,sum(abs(diag(Pi))>1e-5 & abs(diag(truesubspace))>1e-5)/sum(diag(truesubspace)!=0))
	
}
########## Cv to select lambda
Ks <- 1:3
nfold <- 5
nslice <- 5
lambdas <- seq(0.001,0.8,by=0.005)*sqrt(log(p)/n)
a <- ssir.cv(X,y,Ks,lambdas,nfold,nslice)
calmean <- function(mat){ apply(mat,2,mean)}
calse <- function(mat){ apply(mat,2,sd)/nfold}
temp1 <- lapply(a,calmean)
temp1se <- lapply(a,calse)
cverror <- NULL

for(k in Ks){
    
    tempcv <- min(temp1[[k]])+temp1se[[k]][which(temp1[[k]]==min(temp1[[k]]))][1]
    
    tempcv2 <- temp1[[k]][tail(which(temp1[[k]]<=tempcv),1)]
    
    cverror <- c(cverror,tempcv2)
}



#	temp2 <- unlist(lapply(temp1,min))
chosenK <- which(cverror==min(cverror))
chosenK <- chosenK[1]
print(chosenK)

templambda <- min(temp1[[chosenK]])+temp1se[[chosenK]][which(temp1[[chosenK]]==min(temp1[[chosenK]]))][1]

chosenlambda <- lambdas[tail(which(temp1[[chosenK]]<=templambda),1)]
chosenlambda <- chosenlambda[1]
print(chosenlambda)


######### Fit it using CV selected tuning parameters
Sigmax <- cov(X)
Sigmafit <- calsigmafit(y,X,nslice=5)
res <- ssir(Sigmafit,Sigmax,chosenlambda,chosenK,epsilon=1e-03,maxiter=1000,trace=FALSE)
Pi <- res$Pi
cvspec <- sum(abs(diag(Pi))<1e-5 & abs(diag(truesubspace))< 1e-5)/sum(diag(truesubspace)==0)
cvsens <- sum(abs(diag(Pi))>1e-5 & abs(diag(truesubspace))>1e-5)/sum(diag(truesubspace)!=0)




return(list(sensitivity=sensitivity,specificity=specificity,cvsens=cvsens,cvspec=cvspec))
}


	

############################################
# linearcase Frobenius Norm
############################################
linearcasefro <- function(ns,p,s,seed){
	
	set.seed(seed)
	beta <- rep(0,p)
	beta[1:s] <- 1/sqrt(s)
	
# Generate AR-1 type covariance for X
	Sigma <- matrix(0.5,p,p)
	tmpmat <- matrix(0,p,p)
	for(i in 1:(p-1)){
		tmpmat[i,i:p] <- c(0:(length(i:p)-1))
	}
	tmpmat = tmpmat+t(tmpmat)
	Sigma <- Sigma^tmpmat	
	eigenSigma <- eigen(Sigma)
	sqrtSigma <- eigenSigma$vectors%*%diag(sqrt(eigenSigma$values))%*%t(eigenSigma$vectors)	

mse <- number <- NULL

truesubspace <- cbind(beta)%*%t(cbind(beta))
temp <- svd(truesubspace)
truesubspace <- temp$u[,1]%*%t(temp$u[,1])

for(n in ns){
	print(n)
	lambdas <- seq(0.2,5,by=0.5)*sqrt(log(p)/n)
	Ks <- 1
	X <- mvrnorm(n=n,mu=rep(0,p),Sigma=Sigma)
	y <- X[,1]+X[,2]+X[,3]+2*rnorm(n,0,1)


chosenlambda <- 2*sqrt(log(p)/n)	
chosenK <- 1

# Sample covariance
Sigmafit <- calsigmafit(y,X,nslice=5)
Sigmax <- cov(X)	

res <- ssir(Sigmafit,Sigmax,chosenlambda,chosenK,epsilon=1e-03,maxiter=1000,trace=FALSE)

	Pi <- res$Pi
	temp <- svd(Pi)
    est <- temp$u[,1:chosenK]%*%t(temp$u[,1:chosenK])
	mse <- c(mse,sqrt(sum((truesubspace-est)^2)))
	number <- c(number,n)

}
	
return(list(mse=mse,number=number))	
}





############################################
# Simulation (1) onebitsim
############################################
onebitsim <- function(n,p,lambdas,seed){
	
	set.seed(seed)
	beta <- rep(0,p)
	beta[1:5] <- 1/sqrt(5)
	
# Generate AR-1 type covariance for X
	Sigma <- matrix(0.5,p,p)
	tmpmat <- matrix(0,p,p)
	for(i in 1:(p-1)){
		tmpmat[i,i:p] <- c(0:(length(i:p)-1))
	}
	tmpmat = tmpmat+t(tmpmat)
	Sigma <- Sigma^tmpmat	
	eigenSigma <- eigen(Sigma)
	sqrtSigma <- eigenSigma$vectors%*%diag(sqrt(eigenSigma$values))%*%t(eigenSigma$vectors)	
	truesubspace <- cbind(beta)%*%t(cbind(beta))
	temp <- svd(truesubspace)
	truesubspace <- temp$u[,1]%*%t(temp$u[,1])
	
	X <- mvrnorm(n=n,mu=rep(0,p),Sigma=Sigma)
	y <- sign(X%*%beta+0.5*rnorm(n,0,1))

	nfold <- 5
	nslice <- 5
	Sigmax <- cov(X)
	Sigmafit <- calsigmafit(y,X,nslice=5)	

mse <- correctzero <- correctnonzero <- specificity <- sensitivity <- NULL
	
	initPi = diag(1,p,p)
	initH = diag(1,p,p)
	initGamma = diag(0,p,p)
	
for(lambda in lambdas){
print(lambda)	

	res <- ssir(Sigmafit,Sigmax,lambda,K=1,epsilon=1e-03,init=TRUE,initPi=initPi,initH=initH,initGamma=initGamma)

	initPi = res$Pi
	initH = res$H
	initGamma = res$Gamma
	
	Pi <- res$Pi
	temp <- svd(Pi)
        est <- temp$u[,1]%*%t(temp$u[,1])
	specificity <- c(specificity,sum(abs(diag(Pi))<1e-5 & abs(diag(truesubspace))< 1e-5)/sum(diag(truesubspace)==0))
	sensitivity <- c(sensitivity,sum(abs(diag(Pi))>1e-5 & abs(diag(truesubspace))>1e-5)/sum(diag(truesubspace)!=0))
	
}
########## Cv to select lambda
Ks <- 1:3
nfold <- 5
nslice <- 5
lambdas <- seq(0.001,0.8,by=0.005)*sqrt(log(p)/n)
a <- ssir.cv(X,y,Ks,lambdas,nfold,nslice)
calmean <- function(mat){ apply(mat,2,mean)}
calse <- function(mat){ apply(mat,2,sd)/nfold}
temp1 <- lapply(a,calmean)
temp1se <- lapply(a,calse)
cverror <- NULL

for(k in Ks){
    
    tempcv <- min(temp1[[k]])+temp1se[[k]][which(temp1[[k]]==min(temp1[[k]]))][1]
    
    tempcv2 <- temp1[[k]][tail(which(temp1[[k]]<=tempcv),1)]
    
    cverror <- c(cverror,tempcv2)
}



#	temp2 <- unlist(lapply(temp1,min))
chosenK <- which(cverror==min(cverror))
chosenK <- chosenK[1]
print(chosenK)

templambda <- min(temp1[[chosenK]])+temp1se[[chosenK]][which(temp1[[chosenK]]==min(temp1[[chosenK]]))][1]

chosenlambda <- lambdas[tail(which(temp1[[chosenK]]<=templambda),1)]
chosenlambda <- chosenlambda[1]
print(chosenlambda)


######### Fit it using CV selected tuning parameters
Sigmax <- cov(X)
Sigmafit <- calsigmafit(y,X,nslice=5)
res <- ssir(Sigmafit,Sigmax,chosenlambda,chosenK,epsilon=1e-03,maxiter=1000,trace=FALSE)
Pi <- res$Pi
cvspec <- sum(abs(diag(Pi))<1e-5 & abs(diag(truesubspace))< 1e-5)/sum(diag(truesubspace)==0)
cvsens <- sum(abs(diag(Pi))>1e-5 & abs(diag(truesubspace))>1e-5)/sum(diag(truesubspace)!=0)




return(list(sensitivity=sensitivity,specificity=specificity,cvsens=cvsens,cvspec=cvspec))
}





############################################
# Simulation (1) onebitsim Frobenius Norm
############################################
onebitsimfro <- function(ns,p,s,seed){
	
	set.seed(seed)
	beta <- rep(0,p)
	beta[1:s] <- 1/sqrt(s)
	
# Generate AR-1 type covariance for X
	Sigma <- matrix(0.5,p,p)
	tmpmat <- matrix(0,p,p)
	for(i in 1:(p-1)){
		tmpmat[i,i:p] <- c(0:(length(i:p)-1))
	}
	tmpmat = tmpmat+t(tmpmat)
	Sigma <- Sigma^tmpmat	
	eigenSigma <- eigen(Sigma)
	sqrtSigma <- eigenSigma$vectors%*%diag(sqrt(eigenSigma$values))%*%t(eigenSigma$vectors)	

mse <- number <- NULL

truesubspace <- cbind(beta)%*%t(cbind(beta))
temp <- svd(truesubspace)
truesubspace <- temp$u[,1]%*%t(temp$u[,1])

for(n in ns){
	print(n)
	lambdas <- seq(0.2,5,by=0.5)*sqrt(log(p)/n)
	Ks <- 1
	X <- mvrnorm(n=n,mu=rep(0,p),Sigma=Sigma)
	y <- sign(X%*%beta+0.5*rnorm(n,0,1))


chosenlambda <- 2*sqrt(log(p)/n)	
chosenK <- 1

# Sample covariance
Sigmafit <- calsigmafit(y,X,nslice=2)
Sigmax <- cov(X)	

res <- ssir(Sigmafit,Sigmax,chosenlambda,chosenK,epsilon=1e-03,maxiter=1000,trace=FALSE)

	Pi <- res$Pi
	temp <- svd(Pi)
    est <- temp$u[,1:chosenK]%*%t(temp$u[,1:chosenK])
	mse <- c(mse,sqrt(sum((truesubspace-est)^2)))
	number <- c(number,n)

}
	
return(list(mse=mse,number=number))	
}


############################################
# Simulation (2) sinfracsim
############################################
sinfracsim <- function(n,p,lambdas,seed){
	
	set.seed(seed)
	beta1 <- rep(0,p)
	beta2 <- rep(0,p)
	beta1[1:3] <- 1
	beta2[4:5] <- 1
	
# Generate AR-1 type covariance for X
	Sigma <- matrix(0.5,p,p)
	tmpmat <- matrix(0,p,p)
	for(i in 1:(p-1)){
		tmpmat[i,i:p] <- c(0:(length(i:p)-1))
	}
	tmpmat = tmpmat+t(tmpmat)
	Sigma <- Sigma^tmpmat	
	eigenSigma <- eigen(Sigma)
	sqrtSigma <- eigenSigma$vectors%*%diag(sqrt(eigenSigma$values))%*%t(eigenSigma$vectors)	
	truesubspace <- cbind(beta1,beta2)%*%t(cbind(beta1,beta2))
	


	X <- mvrnorm(n=n,mu=rep(0,p),Sigma=Sigma)
	y <- (X[,1]+X[,2]+X[,3])/(0.5+(X[,4]+X[,5]+1.5)^2)+0.2*rnorm(n,0,1)

	nfold <- 5
	nslice <- 5
	Sigmax <- cov(X)
	Sigmafit <- calsigmafit(y,X,nslice=5)	

mse <- correctzero <- correctnonzero <- specificity <- sensitivity <- NULL
	
	initPi = diag(1,p,p)
	initH = diag(1,p,p)
	initGamma = diag(0,p,p)
	
for(lambda in lambdas){
print(lambda)	
	
	res <- ssir(Sigmafit,Sigmax,lambda,K=2,epsilon=1e-03,init=TRUE,initPi=initPi,initH=initH,initGamma=initGamma)

	initPi = res$Pi
	initH = res$H
	initGamma = res$Gamma
	
	Pi <- res$Pi
	temp <- svd(Pi)
    est <- temp$u[,1:2]%*%t(temp$u[,1:2])
	specificity <- c(specificity,sum(abs(diag(Pi))<1e-5 & abs(diag(truesubspace))< 1e-5)/sum(diag(truesubspace)==0))
	sensitivity <- c(sensitivity,sum(abs(diag(Pi))>1e-5 & abs(diag(truesubspace))>1e-5)/sum(diag(truesubspace)!=0))
	
}
########## Cv to select lambda
Ks <- 1:3
nfold <- 5
nslice <- 5
lambdas <- seq(0.001,0.8,by=0.01)*sqrt(log(p)/n)
a <- ssir.cv(X,y,Ks,lambdas,nfold,nslice)
calmean <- function(mat){ apply(mat,2,mean)}
calse <- function(mat){ apply(mat,2,sd)/nfold}
temp1 <- lapply(a,calmean)
temp1se <- lapply(a,calse)
cverror <- NULL

for(k in Ks){
    
    tempcv <- min(temp1[[k]])+1.5*temp1se[[k]][which(temp1[[k]]==min(temp1[[k]]))][1]
    
    tempcv2 <- temp1[[k]][tail(which(temp1[[k]]<=tempcv),1)]
    
    cverror <- c(cverror,tempcv2)
}



#	temp2 <- unlist(lapply(temp1,min))
chosenK <- which(cverror==min(cverror))
chosenK <- chosenK[1]
print(chosenK)

templambda <- min(temp1[[chosenK]])+1.5*temp1se[[chosenK]][which(temp1[[chosenK]]==min(temp1[[chosenK]]))][1]

chosenlambda <- lambdas[tail(which(temp1[[chosenK]]<=templambda),1)]
chosenlambda <- chosenlambda[1]
print(chosenlambda)


######### Fit it using CV selected tuning parameters
Sigmax <- cov(X)
Sigmafit <- calsigmafit(y,X,nslice=5)
res <- ssir(Sigmafit,Sigmax,chosenlambda,chosenK,epsilon=1e-03,maxiter=1000,trace=FALSE)
Pi <- res$Pi
cvspec <- sum(abs(diag(Pi))<1e-5 & abs(diag(truesubspace))< 1e-5)/sum(diag(truesubspace)==0)
cvsens <- sum(abs(diag(Pi))>1e-5 & abs(diag(truesubspace))>1e-5)/sum(diag(truesubspace)!=0)





return(list(sensitivity=sensitivity,specificity=specificity,cvsens=cvsens,cvspec=cvspec))
}



############################################
# Simulation (2) sinfracsim Frobenius Norm
############################################
sinfracsimfro <- function(ns,p,seed){
	
	set.seed(seed)
	beta1 <- rep(0,p)
	beta2 <- rep(0,p)
	beta1[1:3] <- 1
	beta2[4:5] <- 1
	
# Generate AR-1 type covariance for X
	Sigma <- matrix(0.5,p,p)
	tmpmat <- matrix(0,p,p)
	for(i in 1:(p-1)){
		tmpmat[i,i:p] <- c(0:(length(i:p)-1))
	}
	tmpmat = tmpmat+t(tmpmat)
	Sigma <- Sigma^tmpmat	
	eigenSigma <- eigen(Sigma)
	sqrtSigma <- eigenSigma$vectors%*%diag(sqrt(eigenSigma$values))%*%t(eigenSigma$vectors)	

mse <- number <- NULL

truesubspace <- cbind(beta1,beta2)%*%t(cbind(beta1,beta2))
temp <- svd(truesubspace)
truesubspace <- temp$u[,1:2]%*%t(temp$u[,1:2])

for(n in ns){
	print(n)
	lambdas <- seq(0.2,5,by=0.5)*sqrt(log(p)/n)
	Ks <- 2
	X <- mvrnorm(n=n,mu=rep(0,p),Sigma=Sigma)
	y <- (X[,1]+X[,2]+X[,3])/(0.5+(X[,4]+X[,5]+1.5)^2)+0.2*rnorm(n,0,1)

	nfold <- 5
	nslice <- 5
########## Cv to select lambda
#	a <- ssir.cv(X,y,Ks,lambdas,nfold,nslice)
#a[[1]]<-a[[2]]
#	calmean <- function(mat){ apply(mat,2,mean)}
#	calse <- function(mat){ apply(mat,2,sd)/sqrt(nfold)}
#	temp1 <- lapply(a,calmean)
#	temp1se <- lapply(a,calse)
#	temp2 <- unlist(lapply(temp1,min))
	chosenK <- 2#<- which(temp2==min(temp2))
#	chosenlambda <- lambdas[which(temp1[[chosenK]]==min(temp1[[chosenK]]))]
#	chosenlambda <- chosenlambda[1]
#
chosenlambda <- 2*sqrt(log(p)/n)	
######### Fit it using CV selected tuning parameters 
Sigmax <- cov(X)
Sigmafit <- calsigmafit(y,X,nslice=5)
res <- ssir(Sigmafit,Sigmax,chosenlambda,chosenK,epsilon=1e-03,maxiter=1000,trace=FALSE)

	Pi <- res$Pi
	temp <- svd(Pi)
    est <- temp$u[,1:chosenK]%*%t(temp$u[,1:chosenK])
	mse <- c(mse,sqrt(sum((truesubspace-est)^2)))
	number <- c(number,n)

}
	
return(list(mse=mse,number=number))	
}






############################################
# Cross-validation for choosing K and lambda
# nfold is the number of fold  
# K is a vector of possible values of the rank
# lambda is a vector of possible values of lambda
############################################
ssir.cv <- function(X,y,Ks,lambdas,nfold,nslice){
    p <- ncol(X)
# Index for training vs test set
	fold <- rep(NA,length(y))
	fold <- sample(rep(1:nfold,length(y)/nfold))
	cv.error <- vector("list",length(Ks))
	for(K in Ks){
		cv.error[[K]] <- matrix(NA,nrow=nfold,ncol=length(lambdas))	
	}
	for(j in 1:5){
		print(paste("cross validation for dataset ",j,sep=""))
		tmp <- 1
		ytrain <- y[which(fold!=j)]
		ytest <- y[which(fold==j)]		
		Xtrain <- X[which(fold!=j),]		
		Xtest <- X[which(fold==j),]		
		Sigmax <- cov(Xtrain)
		Sigmafit <- calsigmafit(ytrain,Xtrain,nslice=nslice)

		for(K in Ks){
			initPi = diag(1,p,p)
			initH = diag(1,p,p)
			initGamma = diag(0,p,p)
			
			tmp <- 1
		for(lambda in lambdas){
			res <- ssir(Sigmafit,Sigmax,lambda,K,epsilon=5e-03,maxiter=1000,init=TRUE,initPi=initPi,initH=initH,initGamma=initGamma,trace=FALSE)
			initPi = res$Pi
			initH = res$H
			initGamma = res$Gamma
			yhat <- predictpfc(res,K,ytrain,Xtrain,Xtest)
			# Test if this is integer or continuous
			#if(sum(y-round(y))==0){
			#	yhat <- sign(yhat)
			#	cv.error[[K]][j,tmp] <- 	1-sum(diag(table(yhat,ytest)))/sum(table(yhat,ytest))
			#}
			#else if(sum(y-round(y))!=0){
				cv.error[[K]][j,tmp] <- sum((ytest-yhat)^2)
			#}
			tmp <- tmp + 1
		}
	}
	}
	return(cv.error)
}

############################################
# Prediction using PFC under X|y Normality Assumption 
# See Cook Statistical Science Paper 2007 and 2008
# Insert output of PFC object and desired rank
# Xnew is a matrix
############################################
predictpfc <- function(pfcobject,K,y,X,Xnew){
	
	Pi <- pfcobject$Pi
	temp <- svd(Pi)
	if(max(temp$d)<0.01){
		return(rep(mean(y),nrow(Xnew)))
	}
	temp <- temp$u[,1:K]
	RhatX <- X%*%temp
	
	Xnew <- as.list(data.frame(t(Xnew)))
	
	predicty <- function(x){ 		
		temp2 <- x%*%temp	
		residual <- t(t(RhatX)-as.vector(temp2))
		weights <- exp(-0.5*apply((residual)^2,1,sum))
		weights <- weights/sum(weights)
	 	return(sum(weights*y))
	}
	yhat <- unlist(lapply(Xnew,predicty))
	return(yhat)	
} 

############################################
# A function to calculate Sigmafit for PFC
# If y is categorical, nslice is # of unique values in y
# Standardize for mean
############################################
calsigmafit <- function(y,X,nslice,standardize=TRUE){

n <- length(y)

if(standardize==TRUE){ 	
	X <- scale(X,T,F)
	}

# Test if this is integer or continuous
if(sum(y-round(y))==0){
	nslice <- length(unique(y))
	quany <- sort(unique(y))
}

# For continuous response y
else if(sum(y-round(y))!=0){
quany <- quantile(y,seq(1/nslice,1,length.out=nslice))
}

indexy <- vector("list",nslice)

# Assign indices into indexy
	indexy[[1]] <- which(y<=quany[1])
for(k in 2:nslice){
	indexy[[k]] <- which(y >quany[k-1] & y<=quany[k])
}

nindexy <- lapply(indexy,length)

# f matrix 
f <- matrix(0,n,nslice)
for(k1 in 1:(nslice-1)){
	for(k2 in 1:nslice){
		if(k1==k2){
		f[indexy[[k1]],k2] <- 1 - nindexy[[k2]]/n
		}
		if(k1!=k2){
		f[indexy[[k1]],k2] <- -nindexy[[k2]]/n			
		}
	}
}
for(k in 1:nslice){
	f[indexy[[nslice]],k] <- -nindexy[[k]]/n
}

bigF <- f%*%solve(t(f)%*%f)%*%t(f)
Sigmafit <- t(X)%*%bigF%*%X/(n)
return(Sigmafit)
}



#####################################
# L-ADMM for sparse Sliced Inverse Regression
#####################################
ssir <- function(covxy,covx,lambda,K,nu=1,epsilon=1e-3,maxiter=1000,trace=FALSE,init=FALSE,initPi=NULL,initH=NULL,initGamma=NULL){
	
	p <- nrow(covx)
	eigencovx <- eigen(covx)
	sqcovx <- eigencovx$vectors%*%sqrt(diag(pmax(eigencovx$values,0)))%*%t(eigencovx$vectors)	
	
	tau <- 4*nu*eigencovx$values[1]^2	
	criteria <- 1e10
	i <- 1
	
	
	# Initialize parameters
	H <- Pi <- oldPi <-  diag(1,p,p)
	Gamma <- matrix(0,p,p)
    
    if(init==TRUE){
        H <- initH
        Pi <- initPi
        Gamma <- initGamma
    }
	
	# While loop for the iterations
    	while(criteria > epsilon && i <= maxiter){
        Pi <- updatePi(covx,sqcovx,covxy,H,Gamma,nu,lambda,Pi,tau)

		H <- updateH(sqcovx,Gamma,nu,Pi,K)
		Gamma <- Gamma + sqcovx%*%Pi%*%sqcovx-H	
        criteria <- sqrt(sum((Pi-oldPi)^2))
        oldPi <- Pi
        i <- i+1
        if(trace==TRUE)
        {
 		    print(i)
			print(criteria)
        }
		
	}
	
	return(list(Pi=Pi,H=H,Gamma=Gamma,iteration=i,convergence=criteria))

}



######################################################
# Update Pi
######################################################
updatePi <- function(covx,sqcovx,covxy,H,Gamma,nu,lambda,Pi,tau){
	
    A <- Pi + 1/tau*covxy-nu/tau*covx%*%Pi%*%covx+nu/tau*sqcovx%*%(H-Gamma)%*%sqcovx
    B <- lambda/tau
    return(Soft(A,B))
}


######################################################
# Update H
######################################################
updateH <- function(sqcovx,Gamma,nu,Pi,K){

	temp <- Gamma + sqcovx%*%Pi%*%sqcovx
	temp <- (temp+t(temp))/2
	svdtemp <- eigen(temp)
	d <- svdtemp$values
	p <- length(d)
	
	if(sum(pmin(1,pmax(d,0)))<=K){
        dfinal <- pmin(1,pmax(d,0))
        return(svdtemp$vectors%*%diag(dfinal)%*%t(svdtemp$vectors))
	}
	
	fr <- function(x){
		sum(pmin(1,pmax(d-x,0)))
	}
# Vincent Vu Fantope Projection
	knots <- unique(c((d-1),d))
	knots <- sort(knots,decreasing=TRUE)
	temp <- which(sapply(knots,fr)<=K)
	lentemp <- tail(temp,1)
	a=knots[lentemp]
	b=knots[lentemp+1]
	fa <- sum(pmin(pmax(d-a,0),1))
	fb <- sum(pmin(pmax(d-b,0),1))
	theta <- a+ (b-a)*(K-fa)/(fb-fa)
	dfinal <- pmin(1,pmax(d-theta,0))
	res <- svdtemp$vectors%*%diag(dfinal)%*%t(svdtemp$vectors)
	return(res)
}	



######################################################
# Soft-thresholding Operator
######################################################
Soft <- function(a,b){
    if(b<0) stop("Can soft-threshold by a nonnegative quantity only.")
    return(sign(a)*pmax(0,abs(a)-b))
}

	





