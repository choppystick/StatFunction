#computes a 1-way MANOVA for a mat dat0 with identicators column idol.
#function iterates over each group and calculates the mean for each dependent variable. Then uses these means to calculate the sum of squares within groups (SSW)
#and the group mean deviations from the overall mean to calculate the sum of squares between groups (SSB).
#Finally, the function calculates the total sum of squares (SST) and Wilks' lambda statistic (lambda).

Manova.1way <- function(dat0,idcol){
  mat0 <- convert.data(dat0, idcol)
  id1 <- mat0[,idcol]
  ud1 <- unique(id1)

  #mean calculation and SS calculation
  mean.mat <- NULL
  nvec <- NULL
  SSW <- 0

  for(i in seq_along(ud1)){
      I1 <- (id1==ud1[i])
      m0 <- mat0[I1, -idcol]
      mu0 <- apply(m0, 2, mean)

      musubt <- function(vec){ vec-mu0 }

      m1 <- apply(m0, 1, musubt)
      SSW <- SSW+m1%*%t(m1)
      mean.mat <- rbind(mean.mat, mu0)
      nvec <- c(nvec, length(m1[1,]))
  }

  mutot <- (nvec%*%mean.mat)/sum(nvec)
  musubt <- function(vec){vec-mutot}

  b0 <- apply(mean.mat,1,musubt)
  #print(dim(b0))
  #print(length(nvec))

  for(i in seq_along(nvec)){
      b0[,i] <- b0[,i]*sqrt(nvec[i])
  }

  SSB <- b0%*%t(b0)
  T0 <- apply(mat0[,-idcol], 1, musubt)
  SST <- (T0)%*%t(T0)

  list(lambda=mdet(SSW)/mdet(SSW+SSB), SSW=SSW, SSB=SSB, SST=SST, mumat=mean.mat)
}

#Extension of Manova.1way. Performs a permutation test to obtain a p-value for an observed Wilk's ambda statistic.
Manova.1way.perm <- function(mat, idcol, nperm=10000){
  dum <- Manova.1way(mat, idcol)
  lambda0 <- dum$lambda
  m1 <- mat[,-idcol]
  idvec <- mat[,idcol]
  n1 <- length(m1[,1])
  lambdavec <- NULL
  for(i in 1:nperm){
      nsamp <- sample(n1)
      mp <- m1[nsamp,]
      matp <- cbind(idvec,mp)
      lambdavec <- c(lambdavec, Manova.1way(matp, 1)$lambda)
  }
  pval <- sum(lambdavec<lambda0)/nperm
  dum$p <- pval
  #dum$lambdavec<-lambdavec
  return(dum)
}

#function takes in a dataframe and converts the data into a matrix and the identificator columns into integers.
#Used for cleaning up the categorical grouping columns into numerical grouping columns
convert.data <- function(dat, idcol){
  n1 <- length(idcol)
  #print(idcol)
  dat00 <- as.matrix(dat[,-idcol])
  n0 <- length(dat[,1])
  n2 <- length(dat[1,])
  dat0 <- matrix(rep(0,n0*n2),n0,n2)
  zmatnumid <- 1:n2[-idcol]
  dat0[,zmatnumid] <- dat00

  for(i in 1:n1){
      idv <- dat[,idcol[i]]
      un1 <- unique(idv)
      n2 <- length(un1)
      vec0 <- rep(0, length(idv))
      for(j in 1:n2){
          Id0 <- (idv==un1[j])
          vec0[Id0] <- j
      }
      dat0[,idcol[i]] <- vec0
      }

  return(dat0)
}

#Performs multiway MANOVA. It is assumed that there are no more than 9 levels for any factor. 
#It is also assumed this is higher than 2 way and every cell has more than 1 observation.
#rid = response id column. To remove any response variable.
Manova.multiway <- function(dat0, idcol, rid=0){
    if(abs(rid[1])>.2){
      mat0 <- convert.data(dat0, c(idcol, rid))
    }
    else{
      mat0 <- convert.data(dat0, idcol)
    }
  
    nway <- length(idcol)
    idvec0 <- 0
  
    for(i in 1:nway){
      idvec0 <- idvec0+mat0[,idcol[i]]*10^(i-1)
    }
  
    mat1 <- cbind(idvec0,mat0[,-c(idcol,rid)])
    dum1 <- Manova.1way(mat1, 1)
    SSE <- dum1$SSW
    SSB <- dum1$SSB
    SST <- dum1$SST
    mean.mat <- dum1$mumat
    residmat <- dum1$resid
  
    list(lambda=mdet(SSE)/(mdet(SSE+SSB)), SSE=dum1$SSW, SSB=dum1$SSB, SST=dum1$SST, mean.mat=mean.mat, mumatA=dum1$mumatA, residmat=residmat)
}

Manova.multiway.portmanteau <- function(dat0, idcol, int.str) {
    n1 <- length(int.str)
    out.str <- rep(list(), (n1 + 1))
    SSElist <- rep(list(), (n1 + 1))
    SSBlist <- rep(list(), (n1 + 1))
    SSTlist <- rep(list(), (n1 + 1))
    mulist <- rep(list(), (n1 + 1))
    muAlist <- rep(list(), (n1 + 1))
    residlist <- rep(list(), (n1 + 1))
    lambdavec <- NULL
    clambdavec <- NULL
    
    # SSE and full interaction
    dum1 <- Manova.multiway(dat0, idcol)
    out.str[[1]] <- dum1
    lambdavec <- NULL
    SSElist[[1]] <- dum1$SSE
    SSBlist[[1]] <- dum1$SSB
    SSTlist[[1]] <- dum1$SST
    mulist[[1]] <- dum1$mean.mat
    residlist[[1]] <- dum1$residmat
    muAlist[[1]] <- dum1$mumatA
    SSBtot <- 0
    dat00 <- convert.data(dat0, idcol)
    
    for (j in 1:n1) {
        idcoltemp <- idcol[int.str[[j]]]
        throw.away <- (idcol[-int.str[[j]]])
        dum2 <- Manova.multiway(dat00, idcoltemp, rid = throw.away)
        lambdavec <- c(lambdavec, mdet(dum1$SSE) / mdet(dum1$SSE + dum2$SSB))
        clambdavec <- c(clambdavec, det(dum1$SSE) / det(dum1$SSE + dum2$SSB))
        out.str[[(j + 1)]] <- list(int = int.str[[j]], raw = dum2)
        SSElist[[(j + 1)]] <- dum2$SSE
        SSBlist[[(j + 1)]] <- dum2$SSB
        SSTlist[[(j + 1)]] <- dum2$SST
        SSBtot <- SSBtot + dum2$SSB
        mulist[[(j + 1)]] <- dum2$mean.mat
        residlist[[(j + 1)]] <- dum2$residmat
        muAlist[[(j + 1)]] <- dum2$mumatA
    }
    
    lambda.int <- mdet(dum1$SSE) / mdet(dum1$SST - SSBtot)
    clambda.int <- det(dum1$SSE) / det(dum1$SST - SSBtot)
    out <- list(Lambda = c(lambdavec, lambda.int),
                 cLambda = c(clambdavec, clambda.int),
                 outinf = out.str,
                 SSE = SSElist,
                 SSB = SSBlist,
                 SST = SSTlist,
                 mu = mulist,
                 muAlist = muAlist,
                 resid = residlist)
    return(out)
}

Manova.multiway.perm <- function(mat, idcol, nperm = 10000) {
    # function(dat0, idcol, int.str)
    # NOTE IDCOL SHOULD BE IN ORDER THEY APPEAR IN MATRIX
    n00 <- length(idcol)
    int.str <- rep(list(), n00)
    for (i in 1:n00) {
        int.str[[i]] <- i
    }
    dum <- Manova.multiway.portmanteau(mat, idcol, int.str)
    lambda0 <- dum$Lambda
    clambda0 <- dum$cLambda
    m1 <- mat[, -idcol]
    idvec <- mat[, idcol]
    n0 <- length(idvec[1, ])
    n1 <- length(m1[, 1])
    lambdamat <- NULL
    clambdamat <- NULL

    print(lambda0)
    print(clambda0)
    for (i in 1:nperm) {
        nsamp <- sample(n1)
        mp <- m1[nsamp, ]
        matp <- cbind(idvec, mp)
        dumP <- Manova.multiway.portmanteau(matp, c(1:n0), int.str)
        lambdamat <- rbind(lambdamat, dumP$Lambda)
        clambdamat <- rbind(clambdamat, dumP$cLambda)
        if ((i / 500) == floor(i / 500)) {
            print(c(i))
        }
    }
    lambcomp <- function(vec) { 1 * (vec < lambda0) }
    dumcomp <- t(apply(lambdamat, 1, lambcomp))
    pvec <- apply(dumcomp, 2, sum) / nperm
    clambcomp <- function(vec) { 1 * (vec < clambda0) }
    cdumcomp <- t(apply(clambdamat, 1, clambcomp))
    cpvec <- apply(cdumcomp, 2, sum) / nperm

    dum$p <- pvec
    dum$cp <- cpvec
    dum
}

#Perform a full MANOVA with minimal dependencies with other functions.
Manova <- function(dat0, idcol, int.str) {
    n1 <- length(int.str)
    out.str <- rep(list(), (n1 + 1))
    SSElist <- rep(list(), (n1 + 1))
    SSBlist <- rep(list(), (n1 + 1))
    SSTlist <- rep(list(), (n1 + 1))
    mulist <- rep(list(), (n1 + 1))
    muAlist <- rep(list(), (n1 + 1))
    residlist <- rep(list(), (n1 + 1))
    lambdavec <- NULL
    clambdavec <- NULL

    # SSE and full interaction
    dum1 <- Manova.multiway(dat0, idcol)
    out.str[[1]] <- dum1
    lambdavec <- NULL
    SSElist[[1]] <- dum1$SSE
    SSBlist[[1]] <- dum1$SSB
    SSTlist[[1]] <- dum1$SST
    mulist[[1]] <- dum1$mean.mat
    residlist[[1]] <- dum1$residmat
    muAlist[[1]] <- dum1$mumatA
    SSBtot1 <- 0
    SSBtot2 <- 0
    SSBtot3 <- 0
    dat00 <- convert.data(dat0, idcol)

    # the assumption is that the interactions in int.str go from lowest to highest
    # So all previous SSB must be subtracted from SST
    for (j in 1:n1) {
        idcoltemp <- idcol[int.str[[j]]]
        throw.away <- (idcol[-int.str[[j]]])
        dum2 <- Manova.multiway(dat00, idcoltemp, rid = throw.away)
        if (length(int.str[[j]]) == 1) {
            lambdavec <- c(lambdavec, mdet(dum1$SSE) / mdet(dum1$SSE + dum2$SSB))
            clambdavec <- c(clambdavec, det(dum1$SSE) / det(dum1$SSE + dum2$SSB))
            out.str[[(j + 1)]] <- list(int = int.str[[j]], raw = dum2)
            SSElist[[(j + 1)]] <- (dum2$SSE)
            SSBlist[[(j + 1)]] <- (dum2$SSB)
            SSTlist[[(j + 1)]] <- dum2$SST
            SSBtot1 <- SSBtot1 + dum2$SSB
            mulist[[(j + 1)]] <- dum2$mean.mat
            residlist[[(j + 1)]] <- dum2$residmat
            muAlist[[(j + 1)]] <- dum2$mumatA
        } else {
            if (length(int.str[[j]]) == 2) {
                vec0 <- int.str[[j]]
                jvec <- vec0 + 1
                SSB <- dum2$SST - SSBlist[[jvec[1]]] - SSBlist[[jvec[2]]] - dum2$SSE
                lambdavec <- c(lambdavec, mdet(dum1$SSE) / mdet(dum1$SSE + SSB))
                clambdavec <- c(clambdavec, det(dum1$SSE) / det(dum1$SSE + SSB))
                out.str[[(j + 1)]] <- list(int = int.str[[j]], raw = dum2)
                SSElist[[(j + 1)]] <- (dum2$SSE)
                SSBlist[[(j + 1)]] <- SSB
                SSTlist[[(j + 1)]] <- dum2$SST
                SSBtot2 <- SSBtot2 + SSB
                mulist[[(j + 1)]] <- dum2$mean.mat
                residlist[[(j + 1)]] <- dum2$residmat
                muAlist[[(j + 1)]] <- dum2$mumatA
            } else {
                if (length(int.str[[j]]) == 3) {
                    jvec <- int.str[[j]] + 1 + length(idcol) * (length(idcol) - 1) / 2
                    SSB <- dum2$SST - dum2$SSE
                    for (k in 1:3) {
                        SSB <- SSB - SSBlist[[jvec[k]]]
                    }
                    lambdavec <- c(lambdavec, mdet(dum1$SSE) / mdet(dum1$SSE + SSB))
                    clambdavec <- c(clambdavec, det(dum1$SSE) / det(dum1$SSE + SSB))
                    out.str[[(j + 1)]] <- list(int = int.str[[j]], raw = dum2)
                    SSElist[[(j + 1)]] <- (dum2$SSE)
                    SSBlist[[(j + 1)]] <- SSB
                    SSTlist[[(j + 1)]] <- dum2$SST
                    SSBtot3 <- SSBtot3 + SSB
                    mulist[[(j + 1)]] <- dum2$mean.mat
                    residlist[[(j + 1)]] <- dum2$residmat
                    muAlist[[(j + 1)]] <- dum2$mumatA
                }
            }
        }
    }

    SSBtottot <- SSBtot1 + SSBtot2 + SSBtot3
    lambda.int <- mdet(dum1$SSE) / mdet(dum1$SST - SSBtottot)
    clambda.int <- det(dum1$SSE) / det(dum1$SST - SSBtottot)
    out <- list(Lambda = c(lambdavec, lambda.int),
                cLambda = c(clambdavec, clambda.int),
                outinf = out.str,
                SSE = SSElist,
                SSB = SSBlist,
                SST = SSTlist,
                mulist = mulist,
                muAlist = muAlist,
                residlist = residlist)
    out
}

#Perform permutation tests with the function Manova.
Manova.perm <- function(mat, idcol, int.str, nperm = 10000) {
    # function(dat0, idcol, int.str)
    # NOTE IDCOL SHOULD BE IN ORDER THEY APPEAR IN MATRIX
    dum <- Manova(mat, idcol, int.str)
    lambda0 <- dum$Lambda
    clambda0 <- dum$cLambda
    m1 <- mat[, -idcol]
    idvec <- mat[, idcol]
    n0 <- length(idvec[1,])
    n1 <- length(m1[, 1])
    lambdamat <- NULL
    clambdamat <- NULL

    print(lambda0)
    print(clambda0)

    for (i in 1:nperm) {
        nsamp <- sample(n1)
        mp <- m1[nsamp, ]
        matp <- cbind(idvec, mp)
        dumP <- Manova(matp, c(1:n0), int.str)
        lambdamat <- rbind(lambdamat, dumP$Lambda)
        clambdamat <- rbind(clambdamat, dumP$cLambda)
        if ((i / 500) == floor(i / 500)) {
            print(c(i))
        }
    }

    lambcomp <- function(vec) {
        1 * (vec < lambda0)
    }

    dumcomp <- t(apply(lambdamat, 1, lambcomp))
    pvec <- apply(dumcomp, 2, sum) / nperm

    clambcomp <- function(vec) {
        1 * (vec < clambda0)
    }

    cdumcomp <- t(apply(clambdamat, 1, clambcomp))
    cpvec <- apply(cdumcomp, 2, sum) / nperm

    dum$p <- pvec
    dum$cp <- cpvec

    return(dum)
}


