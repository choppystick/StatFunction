# Function to generate a contingency table and perform statistical analysis.
# Input:
#   - m1: Input matrix.
# Output:
#   - Obs: Observed counts in the contingency table.
#   - Exp: Expected counts in the contingency table.
#   - Dev: Deviation of observed counts from expected counts.
#   - chisq: Chi-square statistic.
#   - df: Degrees of freedom for the chi-square test.
#   - p: p-value for the chi-square test.
#   - fulltab: Full contingency table with observed and row/column totals.
#   - odds: Odds ratio.
#   - lodd: Log of the odds ratio.
cont.table <- function(m1) {
    rm <- as.matrix(apply(m1, 1, sum), length(m1[, 1]), 1)
    cm <- t(as.matrix(apply(m1, 2, sum), 1, length(m1[1, ])))
    print(rm)
    print(cm)
    Etable <- rm %*% cm / sum(cm)
    dtable <- (m1 - Etable) / sqrt(Etable)
    x <- sum(dtable^2)
    d1 <- dim(m1)
    df <- (length(rm) - 1) * (length(cm) - 1)
    pval <- 1 - pchisq(x, df)
    odds.ratio <- (m1[1, 1] * m1[2, 2]) / (m1[1, 2] * m1[2, 1])
    list(Obs = m1, Exp = Etable, Dev = dtable, chisq = x, df = df, p = pval, fulltab = rbind(cbind(m1, rm), c(cm, sum(cm))), odds = odds.ratio, lodd = log(odds.ratio)))
}

# Function to perform Fisher's Discriminant Analysis and visualize the resulting discriminant space.
# Fisher's Discriminant Analysis is a dimensionality reduction and feature extraction technique used for classification tasks. 
# Finds a linear combination of features that best separates the classes while maximizing the between-class variance 
# and minimizing the within-class variance.

# Input:
#   - dat0: Input data matrix.
#   - idcol: Index of the column representing the class labels.
#   - num.vec: Number of discriminant vectors to retain.
#   - do.plot: Boolean indicating whether to plot the resulting discriminant space. Default is TRUE.

# Output:
#   - mat.out: Transformed data matrix representing the discriminant space.
#   - llambda: Log-likelihood ratio statistic.
Fisher.disc.space <- function(dat0, idcol, num.vec, do.plot = TRUE) {
    mat0 <- convert.data(dat0, idcol)  # Convert data matrix with class labels to appropriate format
    id1 <- mat0[, idcol]  # Extract class labels
    ud1 <- unique(id1)  # Unique class labels
    mean.mat <- NULL  # Matrix to store class means
    nvec <- NULL  # Vector to store sample sizes for each class
    SSB <- 0  # Between-class scatter matrix
    SSW <- 0  # Within-class scatter matrix
    residmat <- mat0  # Matrix to store residuals
    mumat <- mat0  # Matrix to store class means repeated for each sample
    detvec <- NULL  # Vector to store determinants of within-class scatter matrices
    
    for (i in 1:length(ud1)) {
        I1 <- (id1 == ud1[i])  # Index of samples belonging to class i
        m0 <- mat0[I1, -idcol]  # Data matrix for class i
        
        # Calculate class mean
        mu0 <- apply(m0, 2, mean)
        musubt <- function(vec) {vec - mu0}
        m1 <- apply(m0, 1, musubt)  # Calculate deviations from class mean
        SSW <- SSW + m1 %*% t(m1)  # Accumulate within-class scatter matrix
        detvec <- c(detvec, mdet(m1 %*% t(m1)))  # Calculate determinant of within-class scatter matrix
        mean.mat <- rbind(mean.mat, c(mu0))
        nvec <- c(nvec, length(m1[1,]))  
        residmat[I1, -idcol] <- t(m1) 
        
        # Repeat class mean for each sample in the class
        for (i in 1:nvec[i]) {
            mumat[I1, -idcol][i, ] <- mu0
        }
    }
    
    # Calculate overall mean
    mutot <- (nvec %*% mean.mat) / sum(nvec)
    musubt <- function(vec) {vec - mutot}
    b0 <- apply(mean.mat, 1, musubt)  # Calculate deviations from overall mean
    
    # Scale deviations by square root of sample size for each class
    for (i in 1:length(nvec)) {
        b0[, i] <- b0[, i] * sqrt(nvec[i])
    }
    
    # Calculate between-class scatter matrix
    SSB <- b0 %*% t(b0)
    
    # Compute transformation matrix for Fisher's Discriminant Analysis
    mat.toexp <- gen.inv1.2(SSW) %*% SSB %*% gen.inv1.2(SSW)
    z1 <- eigen(mat.toexp)
    n1 <- length(z1$val)
    
    # Transform data to the discriminant space
    matdisp <- mat0[, -idcol] %*% z1$vec[, 1:num.vec]
    
    # Plot the resulting discriminant space if required
    if (do.plot) {
        par(mfrow = c(num.vec, num.vec))
        for (i in 1:num.vec) {
            for (j in 1:num.vec) {
                plot(matdisp[, i], matdisp[, j], type = "n", main = paste("vec", i, "vs vec", j))
                for (k in 1:length(ud1)) {
                    I1 <- (id1 == ud1[k])
                    text(matdisp[I1, i], matdisp[I1, j], k, col = k)
                }
            }
        }
    }
    
    # Calculate log-likelihood ratio statistic
    lambda1 <- mdet(SSW) / detvec
    llambda <- sum(log(lambda1))
    
    # Return transformed data matrix and log-likelihood ratio statistic
    list(mat.out = cbind(matdisp, id1), llambda = llambda)
}

# Function to perform permutation-based covariance test for Fisher's Discriminant Analysis.

# Input:
#   - dat0: Input data matrix.
#   - idcol: Index of the column representing the class labels.
#   - numvec: Number of discriminant vectors to retain.

# Output:
#   - plambda: P-value for the permutation-based covariance test.
Perm.cov.test <- function(dat0, idcol, numvec) {
    mat0 <- convert.data(dat0, idcol)  # Convert data matrix with class labels to appropriate format
    n1 <- length(mat0[, 1])  # Total number of samples
    nperm <- 10000  # Number of permutations
    llambda0 <- Fisher.disc.space(mat0, idcol, numvec, TRUE)$llambda  # Compute log-likelihood ratio for original data
    llambdavec <- NULL  # Vector to store log-likelihood ratios for permuted data
    
    # Perform permutation test
    for (i in 1:nperm) {
        if ((i / 500) == floor(i / 500)) {
            print(i)
        }
        idvec <- mat0[, idcol]  # Extract class labels
        nv1 <- sample(n1, n1)  # Generate random sample indices
        mat1 <- mat0[nv1, ]  # Permute the data
        mat1[, idcol] <- idvec  # Restore original class labels
        llambdavec <- c(llambdavec, Fisher.disc.space(mat1, idcol, numvec, FALSE)$llambda)  # Compute log-likelihood ratio
    }
    
    # Calculate p-value by comparing the log-likelihood ratios of permuted data with the original one
    plambda <- sum(llambdavec > llambda0) / nperm
    return(plambda)
}
