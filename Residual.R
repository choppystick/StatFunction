# Function to create a mean plot of residuals
# Inputs:
#   - str: A list containing residuals and other model-related data.
#   - mat: A matrix or data frame containing additional data.
#   - idcol: Column index or name identifying the ID column in 'mat'.
#   - i: Index specifying the specific iteration or model component.
#   - varsel: Optional. Vector specifying the indices of variables to be included. Default is 0.
# Output:
#   - A scatterplot matrix displaying the mean residuals along with additional data.
resid.meanplot <- function(str, mat, idcol, i, varsel = 0) {
  if (varsel[1] == 0) {
    n1 <- length(str$resid[[i]][1, ])
    varsel <- c(1:n1)
  }
  
  m1 <- str$resid[[i]][, c(1, varsel)]
  m3 <- str$muAlist[[i]][, c(1, varsel)]
  ma <- cbind(m1, m3[, -1] + m1[, -1])
  m2 <- convert.data(mat, idcol)
  m0 <- cbind(m2[, idcol], ma[, -1])
  pairs(m0, pch = as.character(ma[, 1]), col = ma[, 1])
}

# Function to create a residual mean plot for multivariate analysis
# Inputs:
#   - str: A structure containing residuals and other information
#   - mat: Input matrix
#   - idcol: Column index for identification
#   - i: Index
#   - varsel: Variable selection. Default is 0.
# Output:
#   - A plot showing the relationship between variables in the residual matrix
Residplot <- function(str, mat, idcol, i, varsel = 0){
  if (varsel[1] == 0) {
    n1 <- length(str$resid[[i]][1, ])
    varsel <- c(1:n1)
  }
  
  m3 <- str$resid[[i]]
  m1 <- m3[, c(1, varsel)]
  m2 <- convert.data(mat, idcol)
  m0 <- cbind(m2[, idcol], m1[, -1])
  pairs(m0, pch = as.character(m1[, 1]), col = m1[, 1])
  assign("m99a3", m0, envir = .GlobalEnv)
  gui.selectres()
}

# Function to perform multivariate smoothing
# Inputs:
#   - mat: Input matrix
#   - ycol: Columns for the response variables
#   - xcol: Columns for the predictor variables
#   - idcol: Column index for identification
#   - do.plot: Logical indicating whether to plot. Default is TRUE.
# Output:
#   - A list containing lambda, SSE, SSB, SST, output matrix, and smooth structure
multismooth <- function(mat, ycol, xcol, idcol, do.plot = TRUE){
  xid <- mat[, idcol]
  u1 <- unique(xid)
  SSE <- 0
  SST <- 0
  xmat0 <- mat
  
  for(i in 1:length(u1)){
    I1 <- xid == u1[i]
    nx <- length(xcol)
    ny <- length(ycol)
    xmat <- mat[I1, xcol]
    ymat <- mat[I1, ycol]
    SST <- SST + t(ymat) %*% ymat
    
    for(k in 1:ny){
      y0 <- ymat[, k]
      smst.str <- list()
      
      for(j in 1:nx){
        smst <- smooth.spline(xmat[, j], y0)
        smst.str[[j]] <- smst
        resid <- c(y0) - c(predict(smst, xmat[, j])$y)
        y0 <- resid
      }
      
      xmat0[I1, ycol[k]] <- resid
    }
  }
  
  residmat <- xmat0[, ycol]
  SSE <- t(residmat) %*% residmat
  
  if(do.plot){
    pairs(xmat0, pch = as.character(xmat0[, idcol]), col = xmat0[, idcol])
  }
  
  list(lambda = mdet(SSE) / mdet(SST), SSE = SSE, SSB = SST - SSE, SST = SST, outmat = xmat0, smstr = smst.str)
}

# Function to perform multivariate regression
# Inputs:
#   - mat: Input matrix
#   - ycol: Columns for the response variables
#   - xcol: Columns for the predictor variables
#   - idcol: Column index for identification
#   - do.plot: Logical indicating whether to plot. Default is FALSE.
# Output:
#   - A list containing lambda, SSE, SSB, SST, output matrix, and least squares output
multireg <- function(mat, ycol, xcol, idcol, do.plot = FALSE){
  v1 <- mat[, idcol]
  u1 <- unique(v1)
  SST <- 0
  SSE <- 0
  xmat0 <- mat
  
  for(i in 1:length(u1)){
    I1 <- v1 == u1[i]
    nx <- length(xcol)
    ny <- length(ycol)
    xmat <- mat[I1, xcol]
    ymat <- mat[I1, ycol]
    SST <- SST + t(ymat) %*% ymat
    ls.str <- lsfit(xmat, ymat)
    res <- ls.str$res
    SSE <- SSE + t(res) %*% res
    xmat0[I1, ycol] <- res
  }
  
  if(do.plot){
    pairs(xmat0, pch = as.character(xmat0[, idcol]), col = xmat0[, idcol])
  }
  
  SSB <- SST - SSE
  lambda <- mdet(SSE) / mdet(SST)
  
  list(lambda = lambda, SSE = SSE, SSB = SSB, SST = SST, outmat = xmat0, lsout = ls.str)
}

# Function to perform multiple testing
# Inputs:
#   - mat: Input matrix
#   - ycol: Columns for the response variables
#   - xcol: Columns for the predictor variables
#   - idcol: Column index for identification
# Output:
#   - A list containing preg, psmooth, reg, and smooth outputs
multitest <- function(mat, ycol, xcol, idcol) {
  reg.out <- multireg(mat, ycol, xcol, idcol, TRUE)
  print("type in Y and hit return twice when ready to move on")
  duh <- scan(, what = "character")
  smooth.out <- multismooth(reg.out$outmat, ycol, xcol, idcol, TRUE)
  lambdar0 <- reg.out$lambda
  lambdas0 <- smooth.out$lambda
  lambdarvec <- NULL
  lambdasvec <- NULL
  n1 <- length(mat[, 1])
  maty <- mat[, ycol]
  mat0 <- mat
  xid <- mat[, idcol]
  u1 <- unique(xid)
  for (i1 in 1:10000) {
    if ((i1 / 500) == floor(i1 / 500)) {
      print(i1)
    }
    v1 <- c(1:n1)
    v2 <- v1
    for (j1 in 1:length(u1)) {
      I1 <- xid == u1[j1]
      v2a <- sample(v2[I1])
      v1[I1] <- v2a
    }
    mat0[, ycol] <- maty[v1, ]
    preg.out <- multireg(mat0, ycol, xcol, idcol, FALSE)
    lambdarvec <- c(lambdarvec, preg.out$lambda)
    lambdasvec <- multismooth(preg.out$outmat, ycol, xcol, idcol, FALSE)$lambda
  }
  preg <- sum(lambdarvec < lambdar0) / 10000
  ps <- sum(lambdasvec < lambdas0) / 10000
  list(preg = preg, psmooth = ps, reg = reg.out, smooth = smooth.out)
}

# This function creates a graphical user interface (GUI) for selecting parameters
# It prompts the user to input parameters for the multitest function
# After user input, it runs the multitest function with the selected parameters
gui.selectres <- function() {
    library(tcltk)
    
    # Function to create GUI inputs
    inputs <- function() {
        # Initialize variables for GUI inputs
        x <- tclVar("m99a3")
        y <- tclVar("ycol")
        z <- tclVar("xcol")
        w <- tclVar("idcol")

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
        tkgrid(tklabel(tt, text = "Matrix from residplot"), columnspan = 2)
        tkgrid(tklabel(tt, text = "Don't touch"), x.entry, pady = 10, padx = 30)
        tkgrid(tklabel(tt, text = "Input Y variables from plot"), columnspan = 2)
        tkgrid(tklabel(tt, text = "Columns"), y.entry, pady = 10, padx = 30)
        tkgrid(tklabel(tt, text = "Input X variables from plot"), columnspan = 2)
        tkgrid(tklabel(tt, text = "Columns"), z.entry, pady = 10, padx = 30)
        tkgrid(tklabel(tt, text = "Input idcolumn from plot"), columnspan = 2)
        tkgrid(tklabel(tt, text = "1 column"), w.entry, pady = 10, padx = 30)
        
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
    ycol <- eval(parse(text = predictor_para[2]))
    xcol <- eval(parse(text = predictor_para[3]))
    idcol <- eval(parse(text = predictor_para[4]))
    
    # Run multitest function with selected parameters
    multitest(mat, ycol, xcol, idcol)
}

