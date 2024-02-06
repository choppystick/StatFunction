# Perform the Durbin test. Durbin test is a non parametric statistical test used to assess whether a given dataset 
# follows a specified distribution. Compares the empirical cumulative distribution function (ECDF) of the
# observed data to the cumulative distribution function (CDF) of the specified distribution.
#
# Input:
#   - x: Input data vector.
#   - nboot: Number of bootstrap samples (default is 10000).
#
# Output:
#   - pval: p-value of the Durbin test.
Durbin.test <- function(x, nboot=10000){
  mu0 <- mean(x)
  s0 <- sd(x)
  df0 <- pnorm(sort(x), mu0, s0)
  stat0 <- ks.test(x, df0)$statistic
  statvec <- NULL
  
  for(i in 1:10000){
    xb <- rnorm(length(x), mu0, s0)
    mub <- mean(xb)
    sb <- sd(xb)
    dfb <- pnorm(sort(x), mub, sb)
    statvec <- c(statvec, ks.test(xb, dfb)$statistic)
  }
  
  pval <- sum(statvec>stat0)/nboot
  pval
}

# Performs False Discovery Rate (FDR) correction on a list of p-values.
# 
# Input:
#   - v1: A vector of p-values.
#   - Q: The desired false discovery rate (FDR) level.
#   - ind: A boolean indicating whether to return indices of significant p-values or FDR correction values.
# 
# Output:
#   If ind is FALSE:
#   - A plot showing the ordered p-values, the FDR threshold line, and significant p-values (if any).
#   If ind is TRUE:
#   - A list containing either the indices of significant p-values or the FDR correction values depending on the 'ind' parameter.
fdr <- function(v1, Q, ind=F){
  o1 <- order(v1)
  pvec <- v1[o1]
  m <- length(v1)
  qline <- Q*c(1:m)/m
  
  if(!ind){
    c1 <- sum(1/(c(1:m)))
    qline <- Q*c(1:m)/(m*c1)
  }
  
  #Create plot of points and qline
  plot(c(c(1:m),c(1:m)), c(qline, pvec), type="n", xlab="ordering", ylab="pvalue")
  lines(c(1:m), qline)
  points(c(1:m), pvec)
  
  #Calculate Pstar (pmax) and identify all P<= pmax
  dv <- pvec-qline
  I1 <- (dv<0)
  I0 <- I1
  
  if(sum(I0) > .5){
    pmax <- max(pvec[I1])
    I2 <- pvec<=pmax
    points(c(1:m)[I2], pvec[I2], col="red")
    out <- list(interesting=o1[I2], ind=ind)
  
  }
  
  else{
    vec <- qbeta(c(.5, .95, .99, .999), 1, length(v1)+1)
    out <- list(q.5=vec[1], q.95=vec[2], q.99=vec[3], q.999=vec[4])  
  }

  return(out)
}




# Perform a multivariate normality test using the Durbin-Watson statistic.
# Input:
#   - m1: Matrix of observations with variables as columns.
#   - ntest: Number of tests to perform.
#   - Q: Desired false discovery rate (default = 0.01).

# Output:
#   - estimate.of.prob.not.normal: Result of false discovery rate estimation.
#   - pvals: List of p-values from the Durbin-Watson tests.
#   - Interesting.directions: Directions where the null hypothesis is rejected.
#   - rejectingplots: Plot indices where the null hypothesis is rejected.
Multivariate.normal.test <- function(m1, ntest=99, Q=.01){
  ndim <- length(m1[1,])
  dvec <- NULL
  pvec <- NULL
  nnn <- ceiling(sqrt(ntest+1))
  par(mfrow=c(nnn,nnn), ann=F, mar=c(0,0,0,0))

  for(i in 1:ntest){
    u1 <- rnorm(ndim, 0, 1)
    u1 <- u1/sqrt(sum(u1^2))
    dvec <- rbind(dvec, u1)
    v1 <- m1%*%u1
    qqnorm(v1)
    pvec <- c(pvec, Durbin.test(v1))
  }

  out0 <- fdr(pvec, Q, F)
  I1 <- out0$interesting

  if(length(I1) == 0){
    out <- list(estimate.of.prob.not.normal=out0, pvals=pvec, Interesting.directions=0, rejectingplots="none")
  }

  else{
    out <- list(Interesting.directions=dvec[I1,], rejectingplots=I1, pvals=pvec)
  }

  return(out)
}

# Perform a two-sample Hotelling's T-squared test to compare the means of two groups.
# Input:
#   - dat0: Data matrix.
#   - idcol: Group identifier column.
#   - mu00: Hypothesized difference in mean.

# Output:
#   - H: Hotelling's T-squared statistic.
#   - H1: Modified Hotelling's T-squared statistic (Welch's correction applied).
#   - P: p-value of the test.
#   - mu: Difference in means.
#   - df: Degrees of freedom.
Hotellings.twosample <- function(dat0, idcol, mu00){
  mat0 <- convert.data(dat0, idcol)
  id1 <- mat0[,idcol]
  ud1 <- unique(id1)

  #mean calculation and SS calculation
  mean.mat <- NULL
  nvec <- NULL
  SSW <- list()
  SS2 <- 0
  
  for(i in 1:length(ud1)){
    I1 <- (id1==ud1[i])
    m0 <- mat0[I1, -idcol]
    mu0 <- apply(m0, 2, mean)
    
    musubt <- function(vec){vec-mu0}
    
    m1 <- apply(m0, 1, musubt)
    SSW[[i]] <- m1%*%t(m1)
    mean.mat <- rbind(mean.mat, c(mu0))
    nvec <- c(nvec, length(m1[1,]))
  }
  
  mudiff <- mean.mat[1,] - mean.mat[2,]
  S <- SSW[[1]]/(nvec[1]^2) + SSW[[2]]/(nvec[2]^2)
  H <- t(mudiff - mu00)%*%gen.inv(S)%*%(mudiff - mu00)
  p <- length(mudiff)
  denom <- 0
  
  #performs the Welch's correction
  for(i in 1:2){
    denom < -denom+(1/nvec[i])*tr(((SSW[[i]]/(nvec[i]^2))%*%gen.inv(S))^2)
  }
  
  nu <- (p*(p+1))/denom
  H1 <- H*(nu-p+1)/(nu*p)
  pval.standard <- 1-pf(H1, p, nu-p+1)
  
  list(H=H, H1=H1, P=pval.standard, mu=mudiff-mu00, df=c(p,nu-p+1))
}

# Perform a permutation-based Hotelling's two-sample T-squared test.
# Input:
#   - dat0: Data matrix.
#   - idcol: Identifier column indicating group membership.
#   - mu0: Hypothesized difference in mean (default is 0).
#   - nperm: Number of permutations to perform (default is 10000).

# Output:
#   - H: Hotelling's T-squared statistic.
#   - H1: Modified Hotelling's T-squared statistic.
#   - P: P-value of the test.
#   - mu: Difference in means.
#   - df: Degrees of freedom.
#   - permP: Permutation-based P-value.
Hotellings.twosample.perm <- function(dat0, idcol, mu0=0, nperm=10000){
  dum <- Hotellings.twosample(dat0, idcol, mu0)
  H1 <- dum$H1
  mat0 <- convert.data(dat0, idcol)
  idvec <- mat0[,idcol]
  n0 <- length(idvec)
  H1vec <- NULL

  #permutation
  for(i in 1:nperm){
    nvec <- sample(n0)
    matp <- mat0[nvec, -idcol]
    datp <- cbind(idvec, matp)
    H1p <- Hotellings.twosample(datp, 1, mu0)$H1
    H1vec <- c(H1vec, H1p)
  }

  pval <- sum(H1vec>c(H1))/nperm
  dum$permP <- pval

  return(dum)
}

# Compute the confidence interval for Hotelling's two-sample T-squared test.
# Input:
#   - dat0: Data matrix.
#   - idcol: Identifier column indicating group membership.
#   - conmat: Contrast matrix for constructing the confidence interval.
#   - alpha: Significance level (default is 0.05).
#   - mu00: Hypothesized difference in mean (default is 0).
#
# Output:
#   - H: Hotelling's T-squared statistic.
#   - confmat: Confidence interval matrix.
#   - conf: Confidence level.
Hotellings.twosample.conf <- function(dat0, idcol, conmat, alpha=.05, mu00=0){
  mat0 <- convert.data(dat0, idcol)
  id1 <- mat0[,idcol]
  ud1 <- unique(id1)

  #mean calculation and SS calculation
  mean.mat <- NULL
  nvec <- NULL
  SSW <- list()

  for(i in 1:length(ud1)){
    I1 <- (id1 == ud1[i])
    m0 <- mat0[I1, -idcol]
    mu0 <- apply(m0, 2, mean)

    musubt <- function(vec){vec-mu0}

    m1 <- apply(m0, 1, musubt)
    SSW[[i]] <- m1%*%t(m1)
    mean.mat <- rbind(mean.mat, c(mu0))
    nvec <- c(nvec, length(m1[1,]))
  }

  mudiff <- mean.mat[1,] - mean.mat[2,]
  S <- SSW[[1]]/(nvec[1]^2) + SSW[[2]]/(nvec[2]^2)
  H <- t(mudiff-mu00)%*%gen.inv(S)%*%(mudiff-mu00)
  vv <- diag(conmat%*%S%*%conmat)
  p <- length(mudiff)
  denom <- 0

  #Welch's correction
  for(i in 1:2){
    denom <- denom+(1/nvec[i])*tr(((SSW[[i]]/(nvec[i]^2))%*%gen.inv(S))^2)
  }

  nu <- (p*(p+1))/denom
  H1 <- H*(nu-p+1)/(nu*p)
  qval <- (nu*p)/(nu-p+1)*qf(1-alpha, p, nu-p+1)
  mmudiff <- conmat%*%mudiff
  confmat <- cbind(mmudiff-sqrt(qval*vv), mmudiff, mmudiff+sqrt(qval*vv))

  list(H=H, confmat=confmat, conf=100*(1-alpha))
}

# Perform bootstrap two-sample Hotelling's T-squared test with simultaneous confidence intervals.
# Input:
#   - zdata: Data matrix.
#   - idcol: Identifier column indicating group membership.
#   - conmat: Contrast matrix for constructing the confidence interval.
#   - mu00: Hypothesized difference in mean (default is 0).
#   - nboot: Number of bootstrap samples (default is 10000).
#   - alpha: Significance level (default is 0.05).
#
# Output:
#   - bootconf: Simultaneous confidence intervals for the means.
#   - Hotelling: Hotelling's T-squared confidence interval.
Bootstrap.twosample.simconf <- function(zdata,idcol,conmat,mu00=0,nboot=10000,alpha=.05)
  {
    Hconf <- Hotellings.twosample.conf(zdata, idcol, conmat, alpha, mu00)$confmat
    mmu0 <- Hconf[,2]
    mmumat <- NULL
    v1 <- zdata[,idcol]
    vid <- unique(v1)
    nvec <- NULL
  
    for(i in 1:2){
      nvec <- c(nvec, sum(v1==vid[i]))
    }
  
    I1 <- v1==vid[1]
    I2 <- v1==vid[2]
    zbdata <- zdata
  
    for(i in 1:nboot){
      bootsamp1 <- sample(nvec[1], replace=T)
      bootsamp2 <- sample(nvec[2], replace=T)
      zbdata[I1,] <- (zdata[I1,])[bootsamp1,]
      zbdata[I2,] <- (zdata[I2,])[bootsamp2,]
      mmub <- Hotellings.twosample.conf(zbdata, idcol, conmat, alpha, mu00)$confmat[,2]
      mmumat <- rbind(mmumat, c(mmub-mmu0))
    }
  
    cv1 <- cov(mmumat)
    z1 <- NULL
    cv2 <- gen.inv(cv1)
  
    for(i in 1:nboot){
      z1 <- c(z1,t(mmumat[i,])%*%cv2%*%(mmumat[i,]))
    }
  
    o1 <- order(z1)
    q1 <- quantile(z1,1-alpha)
    I1 <- (z1<=q1)
    mubound <- mmumat[I1,]
    muconf <- NULL
  
    for(j in 1:length(mmu0)){
      muconf <- rbind(muconf, c(mmu0[j]-max(mubound[,j]), mmu0[j], mmu0[j]-min(mubound[,j])))
    }

    list(bootconf=muconf, Hotelling=Hconf)
  }

# Function to perform inference on eigenvalues based on bootstrap samples.
# Input:
#   - dat: Data matrix to be analyzed.
#   - alpha: Significance level for the hypothesis tests. Default is 0.05.
#   - scaled: Boolean indicating whether to scale the covariance matrix. Default is FALSE.
#   - nboot: Number of bootstrap samples to generate. Default is 10000.
# Output:
#   - eigen: Eigenvalues and eigenvectors of the covariance or correlation matrix.
#   - test.notunique: Test statistic for the uniqueness of eigenvalues.
#   - bootstrap.val: Bootstrap values for the eigenvalues.
#   - normal.val: Normal approximation values for the eigenvalues.
#   - proportion.var: Proportion of variance explained by each eigenvalue.
#   - vec.cov: List of covariance matrices for each eigenvector.
eigenvalue.inference <- function(dat, alpha = 0.05, scaled = FALSE, nboot = 10000) {
    v1 <- var(dat)
    n1 <- length(dat[, 1])
    if (scaled) {
        v1 <- cor(dat)
    }
    e1 <- eigen(v1)
    p1 <- length(e1$val)
    val0 <- e1$val
    vec0 <- e1$vec
    alpha0 <- alpha / 2
    alphabon <- alpha0 / p1
    z1 <- qnorm(alpha0)
    zbon <- qnorm(alphabon)
    denL1 <- 1 - z1 * sqrt(2 / n1)
    denLbon1 <- 1 - zbon * sqrt(2 / n1)
    denL2 <- 1 + z1 * sqrt(2 / n1)
    denLbon2 <- 1 + zbon * sqrt(2 / n1)
    bootmat <- NULL
    bootmatd <- NULL
    for (i in 1:nboot) {
        if ((i / 500) == floor(i / 500)) {
            print(i)
        }
        vbn <- sample(n1, replace = TRUE)
        bdat <- dat[vbn, ]
        vb1 <- var(bdat)
        if (scaled) {
            vb1 <- cor(bdat)
        }
        eb1 <- eigen(vb1)
        valb <- eb1$val
        bootmat <- cbind(bootmat, val0 / valb)
        bootmatd <- cbind(bootmatd, diff(valb))
    }
    my.quantile <- function(x) {
        quantile(x, c(alphabon, alpha0, 1 - alpha0, 1 - alphabon))
    }
    vz <- apply(bootmat, 1, my.quantile)
    vsd <- sqrt((2 / n1) * (val0[-1]^2 + val0[-p1]^2))
    vt <- diff(val0) / vsd
    Bootstrap <- t(vz) * val0
    Normal <- cbind(val0 / denLbon1, val0 / denL1, val0, val0 / denL2, val0 / denLbon2)
    vec.mat.list <- list()
    for (i in 1:p1) {
        lambda0 <- val0[i]
        E0 <- 0
        for (j in 1:p1) {
            if (j != i) {
                E0 <- E0 + (lambda0 * val0[j] / (sqrt(n1) * ((lambda0 - val0[j])^2))) * vec0 %*% t(vec0)
            }
        }
        vec.mat.list[[i]] <- E0
    }
    list(eigen = e1, test.notunique = pt(vt, n1 - 1), bootstrap.val = Bootstrap, normal.val = Normal, proportion.var = rbind(val0, cumsum(val0) / sum(val0)), vec.cov = vec.mat.list)
}

# This function creates a graphical user interface (GUI) for Principal Component Analysis (PCA)
# It prompts the user to input parameters for the eigenvalue.inference function
# After user input, it runs the eigenvalue.inference function with the selected parameters
# Input:
#   - Data Matrix: The matrix to be analyzed.
#   - Alpha: The significance level for the hypothesis tests.
#   - Scale Covariance: A boolean indicating whether to scale the covariance matrix.
#   - Number of Bootstrap Samples (Nboot): The number of bootstrap samples to generate.
# Output:
#   - eigen: Eigenvalues and eigenvectors of the covariance or correlation matrix.
#   - test.notunique: Test statistic for the uniqueness of eigenvalues.
#   - bootstrap.val: Bootstrap values for the eigenvalues.
#   - normal.val: Normal approximation values for the eigenvalues.
#   - proportion.var: Proportion of variance explained by each eigenvalue.
#   - vec.cov: List of covariance matrices for each eigenvector.
gui.princomp <- function() {
    library(tcltk)
    
    # Function to create GUI inputs
    inputs <- function() {
        # Initialize variables for GUI inputs
        x <- tclVar("T620")
        y <- tclVar(".05")
        z <- tclVar("F")
        w <- tclVar("10000")
        
        # Create a new GUI window
        tt <- tktoplevel()
        tkwm.title(tt, "Choose parameters for new function")
        
        # Entry widgets for input fields
        x.entry <- tkentry(tt, textvariable = x)
        y.entry <- tkentry(tt, textvariable = y)
        z.entry <- tkentry(tt, textvariable = z)
        w.entry <- tkentry(tt, textvariable = w)
        
        # Function to reset input values
        reset <- function() {
            tclvalue(x) <- ""
            tclvalue(y) <- ""
            tclvalue(z) <- ""
            tclvalue(w) <- ""
        }
        
        # Button to reset input values
        reset.but <- tkbutton(tt, text = "Reset", command = reset)
        
        # Function to submit input values
        submit <- function() {
            x <- tclvalue(x)
            y <- tclvalue(y)
            z <- tclvalue(z)
            w <- tclvalue(w)
            e <- parent.env(environment())
            e$x <- x
            e$y <- y
            e$z <- z
            e$w <- w
            tkdestroy(tt)
        }
        
        # Button to submit input values
        submit.but <- tkbutton(tt, text = "Start", command = submit)
        
        # Grid layout for widgets
        tkgrid(tklabel(tt, text = "Input data matrix"), columnspan = 2)
        tkgrid(tklabel(tt, text = "Data"), x.entry, pady = 10, padx = 30)
        tkgrid(tklabel(tt, text = "Alpha"), columnspan = 2)
        tkgrid(tklabel(tt, text = "Alpha"), y.entry, pady = 10, padx = 30)
        tkgrid(tklabel(tt, text = "Scale cov?"), columnspan = 2)
        tkgrid(tklabel(tt, text = "F"), z.entry, pady = 10, padx = 30)
        tkgrid(tklabel(tt, text = "Nboot"), columnspan = 2)
        tkgrid(tklabel(tt, text = "10000"), w.entry, pady = 10, padx = 30)
        
        # Grid layout for buttons
        tkgrid(submit.but, reset.but)

        # Wait for user interaction
        tkwait.window(tt)
        
        # Return selected parameters
        return(c(x, y, z, w))
    }
    
    # Now run the function to get inputs
    predictor_para <- inputs()
    
    # Print selected parameters
    print(predictor_para)
    
    # Extract selected parameters
    mat <- eval(parse(text = predictor_para[1]))
    alpha <- eval(parse(text = predictor_para[2]))
    xscale <- eval(parse(text = predictor_para[3]))
    nboot <- eval(parse(text = predictor_para[4]))
    
    # Run eigenvalue.inference function with selected parameters
    eigenvalue.inference(mat, alpha, xscale, nboot)
}







