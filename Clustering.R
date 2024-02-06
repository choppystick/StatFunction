# Function for performing mixture model fitting with multivariate normal distributions.
# Implements the Expectation-Maximization (EM) algorithm for fitting a Gaussian mixture model to multivariate data. 
# Iteratively updates the means, covariance matrices, and mixing proportions of the mixture components until convergence 
# or until the likelihood difference falls below a specified threshold.
# While updating the parameters based on the observed data and maximizing the likelihood of the Gaussian mixture model.

# Input:
#   - x0: Input data matrix with observations as rows and variables as columns
#   - k: Number of mixture components
#   - thresh.stop: Threshold to stop iteration if likelihood difference is below it
#   - do.scale: Boolean indicating whether to scale the input data

# Output:
#   - mu: Estimated means of the mixture components
#   - sig: Estimated covariance matrices of the mixture components
#   - pivec: Estimated mixing proportions of the mixture components
#   - lik: Final log-likelihood of the model
#   - post: Posterior probabilities of each observation belonging to each component
#   - cluster: Cluster assignments for each observation
my.mvknorm <- function(x0, k, thresh.stop = 1e-9, do.scale = FALSE) {    
    # Scale the input data if required
    if (do.scale) {
        x <- scale(x0)
    } else {
        x <- x0
    }
    
    n <- nrow(x)  # Number of observations
    k1 <- ncol(x)  # Number of variables
    
    # Initialize parameters: means, covariance matrices, mixing proportions
    muvec <- NULL
    sigvec <- list()
    for (i in 1:k) {
        muvec <- rbind(muvec, x[i,])
        sigvec[[i]] <- diag(k1)
    }
    
    # Function to compute the minimum eigenvalue of a list of matrices
    minfunx <- function(z) {
        nz <- length(z)
        v1 <- NULL
        for (i in 1:nz) {
            v1 <- c(v1, eigen(z[[i]])$val)
        }
        min(v1)
    }
    
    # Initialize likelihood difference and start likelihood
    likdif <- 1e10
    likstart <- (-1e10)
    
    # Initialize mixing proportions
    pivec <- rep(1/k, k)
    
    # Main loop: Iterate until convergence
    while ((abs(likdif) > thresh.stop) && (minfunx(sigvec) > 0.0007)) {
        # E-step: Compute posterior probabilities
        pmat <- NULL
        Nvec <- NULL
        for (i in 1:k) {
            pvec0 <- pivec[i] * dmvnorm(x, muvec[i, ], sigvec[[i]])
            pmat <- rbind(pmat, c(pvec0))
        }
        piden <- apply(pmat, 2, sum)
        for (i in 1:k) {
            pmat[i, ] <- pmat[i, ] / piden
            Nvec <- c(Nvec, sum(pmat[i, ]))
        }
        
        # M-step: Update parameters
        for (i in 1:k) {
            muvec[i, ] <- apply(pmat[i, ] %*% x, 2, sum) / Nvec[i]
            musubt <- function(vec) { vec - muvec[i, ] }
            z9 <- apply(x, 1, musubt)
            sigvec[[i]] <- ((z9) %*% diag(pmat[i, ]) %*% t(z9)) / Nvec[i]
        }
        
        # Evaluation step: Compute likelihood and check convergence
        liknewa <- NULL
        for (i in 1:k) {
            pi0 <- sum(pmat[i, ]) / (sum(Nvec))
            liknewa <- rbind(liknewa, (pi0 * dmvnorm(x, muvec[i, ], sigvec[[i]])))
            pivec[i] <- pi0
        }
        liknew0 <- apply(liknewa, 2, sum)
        liknew <- sum(log(liknew0))
        likdif <- liknew - likstart
        likstart <- liknew
    }
    
    # Assign clusters based on posterior probabilities
    pmat1 <- t(pmat)
    z <- rep(0, length(pmat1[, 1]))
    for (i in 1:k) {
        z[pmat1[, i] > 0.5] <- i
    }
    x99 <- cbind(z, x)
    
    # Perform Fisher Discriminant Space analysis on the clustered data
    doh <- Fisher.disc.space(x99, 1, k)
    
    # Return the estimated parameters and results
    list(mu = muvec, sig = sigvec, pivec = pivec, lik = likstart, post = pmat, cluster = z)
}
