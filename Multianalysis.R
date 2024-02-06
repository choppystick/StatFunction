#performs a one-sample Kolmogorov-Smirnov goodness-of-fit test to assess whether a given vector x follows a normal distribution. It then performs a 
#Monte Carlo simulation to obtain the p-value of the test statistics.
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

# implements the false discovery rate (FDR) method to control the proportion of false discoveries in multiple hypothesis testing. Given a vector of 
#p-values v1 in ascending order, the function calculates the FDR-adjusted p-value for each test, and identifies the subset of tests that are deemed "interesting" 
#based on a specified FDR threshold Q.
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


#performs a test for normality of a given multivariate data set. It generates a set of random directions in the space of the data and projects the data onto these 
#directions. For each direction, it performs a normality test on the projected data and stores the p-value of the test. Finally, it applies a multiple testing 
#correction to the p-values and identifies the interesting directions where the data deviates significantly from normality.
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

#perform a two-sample Hotelling's T-squared test to compare the means of two groups.
#dat0: data matrix
#idcol: group identificator column
#mu00: hypothesized difference in mean
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

#performs a permutation test to assess the significance of the Hotelling's two-sample T-squared statistic for two independent groups of multivariate data.
#dat0: data matrix
#idcol: group identificator column
#mu0: hypothesized difference in mean
Hotellings.twosample.perm <- function(dat0, idcol, mu0=0, nperm=10000){
  dum <- Hotellings.twosample(dat0, idcol, mu0)
  H1 <- dum$H1
  mat0 <- convert.data(dat0, idcol)
  idvec <- mat0[,idcol]
  n0 <- length(idvec)
  H1vec <- NULL

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

#compute a confidence interval for the difference between two sample means using the Hotelling's T-squared statistics. 
#dat0: data matrix
#idcol: group identificator column
#conmat: contrast matrix
#mu00: hypothesized difference in mean
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

#performs a two-sample Hotelling's T-squared confidence interval using a bootstrap approach.
#zdata: data matrix
#idcol: group identificator column
#conmat: contrast matrix
#mu00: hypothesized difference in mean
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

# This function creates a graphical user interface (GUI) for selecting parameters
# It prompts the user to input parameters for the eigenvalue.inference function
# After user input, it runs the eigenvalue.inference function with the selected parameters
#
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







