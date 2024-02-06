# Function to perform lars with cross-validation and obtain predictions and residuals
# Inputs:
#   - str: lars object obtained from the lars package.
#   - matrix.train: Training matrix with predictors.
#   - matrix.test: Test matrix with predictors.
#   - y.train: Response vector for training.
# Output:
#   - A list containing predicted values, residuals, and beta coefficients.
lars.cp <- function(str, matrix.train, matrix.test, y.train){
  I1 <- (str$Cp==min(str$Cp))
  s1 <- c(1:length(I1))[I1]
  
  xmat.train <- matrix.train
  ymat.train <- y.train
  
  yp0 <- predict(str,xmat.train,s1)$fit
  resid <- ymat.train-yp0
  xmat.pred <- matrix.test
  yp1 <- predict(str, xmat.pred, s1)$fit
  npred <- length(yp1)
  resp <- sample(resid, npred, replace=T)
  
  list(ypredict0=yp1, resid=resp, beta=str$beta[I1,])
}

# Function to perform leaps with cross-validation and obtain predictions and residuals
# Inputs:
#   - str: leaps object obtained from the leaps package.
#   - matrix.train: Training matrix with predictors.
#   - matrix.test: Test matrix with predictors.
#   - y.train: Response vector for training.
# Output:
#   - A list containing predicted values, residuals, and beta coefficients.
leaps.cp <- function(str, matrix.train, matrix.test,y.train){
  I1 <- (str$Cp==min(str$Cp))
  which1 <- str$which[I1,]
  
  xmat.train <- (matrix.train)[,which1]
  ymat.train <- y.train
  
  ls.train <- lsfit(xmat.train, ymat.train)
  coef0 <- ls.train$coef
  which2 <- c(1:length(which1))[which1]
  coef1 <- which1
  coef1[which1] <- coef0
  
  resid <- ls.train$resid
  xmat.pred <- (matrix.test)[,which1]
  yp1 <- xmat.pred%*%coef0[-1]+coef0[1]
  npred <- length(yp1)
  resp <- sample(resid, npred, replace=T)
  
  list(ypredict0=yp1, resid=resp, beta=coef1)
}

# Function to perform bootstrapping using either leaps or lars
# Inputs:
#   - mat.train: Training matrix with predictors.
#   - y: Response vector for training.
#   - xstring: String indicating which method to use ('leaps' or 'lars').
#   - brep: Number of bootstrap replicates.
# Output:
#   - A list containing bagged beta coefficients, original beta coefficients, and the bootstrap matrix.
bootstrap.ch <- function(mat.train, y, xstring="leaps", brep=10){
  if(xstring=="lars"){
    library(lars)
    func0 <- lars
    reduce.function <- lars.cp
  }
  
  if(xstring=="leaps"){
    library(leaps)
    func0 <- leaps
    reduce.function <- leaps.cp
  }
  
  ypredmat <- NULL
  residmat <- NULL
  betamat <- NULL
  n1 <- length(mat.train[,1])
  
  out0 <- reduce.function(func0(mat.train, y), mat.train,y)
  beta0 <- c(out0$beta)
  I1 <- rep(T, length(beta0))
  beta00 <- beta0

  betamat <- rbind(betamat, beta00)
 
  for(i in 1:brep){
    if((i/2)==floor(i/2)){
      print(c(i, brep))
    }
  
    v1 <- sort(sample(n1, n1, replace=T))

    m0 <- mat.train
    m1 <- (m0[,I1])[v1,]
    y1 <- (y)[v1]

    out1 <- reduce.function(func0(m1, y1), m1, y1)

    beta1 <- out1$beta
    betamat <- rbind(betamat, beta1)
  }

  bagged.beta <- apply(betamat, 2, mean)
  boxplot(betamat)
  
  list(bagged.beta=bagged.beta, orig.beta=beta00, mat=betamat)
}

# Function to perform bootstrapping with prediction intervals
# Inputs:
#   - mat.train: Training matrix with predictors.
#   - mat.test: Test matrix with predictors.
#   - y.train: Response vector for training.
#   - y.test: Response vector for testing.
#   - xstring: String indicating which method to use ('leaps' or 'lars').
#   - brep: Number of bootstrap replicates.
#   - pred.int: Logical indicating whether to calculate prediction intervals. Default is TRUE.
#   - alpha: Significance level for prediction intervals. Default is 0.05.
# Output:
#   - A list containing bagged predictions, original predictions, type of method used, bagged beta coefficients, original beta coefficients, and prediction interval status.
bootstrap.ch.conf <- function(mat.train,mat.test,y.train,y.test,xstring="lars",brep=10000,pred.int=T,alpha=.05){
  if(xstring=="lars"){
    library(lars)
    func0 <- lars
    reduce.function <- lars.cp
  }
  if(xstring=="leaps"){
    library(leaps)
    func0 <- leaps
    reduce.function <- leaps.cp
  }
  ypredmat <- NULL
  residmat <- NULL
  betamat <- NULL
  n1 <- length(mat.train[,1])
  
  out0 <- reduce.function(func0(mat.train, y.train), mat.train, mat.test, y.train)
  ypred0 <- c(out0$ypredict0)
  resid0 <- c(out0$resid)
  beta0 <- c(out0$beta)
  ypredmat <- rbind(ypredmat, ypred0)
  residmat <- rbind(residmat, resid0)
  betamat <- rbind(betamat, beta0)
  
  for(i in 1:brep){
    if((i/200)==floor(i/200)){
      print(c(i,brep))
    }
  
    v1 <- sort(sample(n1, n1, replace=T))
    
    m1 <- (mat.train)[v1,]
    y1 <- (y.train)[v1]
    
    out1 <- reduce.function(func0(m1, y1), m1, mat.test, y1)
    
    ypred1 <- c(out1$ypredict0)
    resid1 <- c(out1$resid)
    beta1 <- c(out1$beta)
    ypredmat <- rbind(ypredmat, ypred1)
    residmat <- rbind(residmat, resid1)
    betamat <- rbind(betamat, beta1)
  }
  
  bagged.pred <- apply(ypredmat, 2, median)
  bagged.beta <- apply(betamat, 2, median)
  quant.boot <- function(x){quantile(x, c(alpha/2, 1-alpha/2))}
  
  if(pred.int){
    n1 <- length(residmat[,1])
    v9 <- sample(n1, n1, replace=F)
    main1 <- paste("Prediction interval", xstring,"alpha=", alpha)
    qboot <- apply(ypredmat+residmat[v9,], 2, quant.boot)
  }
  
  else{
    main1 = paste("Confidence interval,", xstring, "alpha=", alpha)
    qboot <- apply(ypredmat, 2, quant.boot)
  }
  
  y0 <- y.test
  c1 <- cor(y0, bagged.pred)
  c2 <- cor(y0, ypred0)
  plot(rep(bagged.pred, 5),c(y0, ypred0, bagged.pred, qboot[1,], qboot[2,]), xlab="Bagged prediction", ylab="Data and intervals", type="n", 
  main=paste(main1,"\n corbag=", floor(c1*1e8)/1e8, "cor0=", floor(c2*1e8)/1e8))
  points(bagged.pred, y0)
  lines(bagged.pred, bagged.pred)
  
  o1 <- order(bagged.pred)
  lines(bagged.pred[o1], ypred0[o1], col=2)
  lines(bagged.pred[o1], smooth(qboot[1,o1]), col=3)
  lines(bagged.pred[o1], smooth(qboot[2,o1]), col=3)
  
  list(bpred=bagged.pred, ypred0=ypred0, type=xstring, bagged.beta=bagged.beta, orig.beta=beta0, pred.int=pred.int)
}

# Function to perform bootstrapping using leaps or lars for variable selection
# Inputs:
#   - mat.train: Training matrix with predictors.
#   - y: Response vector for training.
#   - xstring: String indicating which method to use ('leaps' or 'lars').
#   - brep: Number of bootstrap replicates.
# Output:
#   - A list containing bagged beta coefficients, original beta coefficients, and the bootstrap matrix.
bootstrap.ch.2 <- function(mat.train, y, xstring="leaps", brep=1000){
  if(xstring=="lars"){
    library(lars)
    func0 <- lars
    reduce.function <- lars.cp
  }
  
  if(xstring=="leaps"){
    library(leaps)
    func0 <- leaps
    reduce.function <- leaps.cp
  }
  
  ypredmat <- NULL
  residmat <- NULL
  betamat <- NULL
  n1 <- length(mat.train[,1])
  
  out0 <- reduce.function(func0(mat.train, y),mat.train, y)
  beta0 <- c(out0$beta)
  I1 <- abs(beta0)>0
  beta00 <- beta0[I1]

  betamat <- rbind(betamat, beta00)
  
  for(i in 1:brep){
    if((i/200)==floor(i/200)){
      print(c(i, brep))
    }
  
    v1 <- sort(sample(n1, n1, replace=T))

    m0 <- mat.train
    m1 <- (m0[,I1])[v1,]
    y1 <- (y)[v1]

    out1 <- reduce.function(func0(m1, y1),m1, y1)

    beta1 <- out1$beta
    betamat <- rbind(betamat, beta1)
  }
  
  bagged.beta <- apply(betamat, 2, mean)
  boxplot(betamat)
  
  list(bagged.beta=bagged.beta, orig.beta=beta00, mat=betamat)
 }
  
# Function to perform bootstrap and compute the bootstrap mean and variance
# Inputs:
#   - vec0: Input vector for bootstrapping.
#   - statfunc: Function to compute the statistic of interest.
#   - nboot: Number of bootstrap replicates.
# Output:
#   - A list containing the original statistic, bootstrap mean, and bootstrap variance.
my.bootstrap.exp <- function(vec0,statfunc,nboot=100){
  n0 <- length(vec0)
  stat0 <- statfunc(vec0)
  bootvec <- NULL

  for( i in 1:nboot){
    vecb <- sample(vec0, replace=T)
    statb <- statfunc(vecb)
    bootvec <- c(bootvec, statb)
  }

  list(stat0=stat0, bootmean=mean(bootvec), bootvar=var(bootvec))
}
