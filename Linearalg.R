# Function to orthogonalize, with Gram-Schmidt orthogonalization, the rows of a matrix
# Inputs:
#   - mat: Input matrix
# Output:
#   - Orthogonalized matrix
orthogonalize <- function(mat){
  n1 <- length(mat[,1])
  v1 <- mat[1,]
  u1 <- v1/sqrt(sum(v1^2))
  m0 <- t(as.matrix(u1, length(u1), 1))
  print(m0)

  for(i in 2:n1){
    v0 <- mat[i,]
    vv0 <- v0 - t(m0) %*% m0 %*% v0
    u0 <- vv0/sqrt(sum(vv0^2))
    m0 <- rbind(m0, c(u0))
  }

  return(m0)
}

# Function to compute the generalized inverse of a matrix.

# Input:
#   - mat: Input matrix.
#   - thresh: Threshold for eigenvalues below which they are considered zero.

# Output:
#   - ret: Generalized inverse of the input matrix.
gen.inv <- function(mat, thresh = 1e-10) {
    v1 <- sum(is.na(mat))  # Count NaN values in the matrix
    v2 <- sum(is.inf(mat))  # Count infinite values in the matrix
    
    # Check if the matrix contains NaN or infinite values
    if ((v1 + v2) > 0.5) {
        print(mat)
    }
    
    # Compute eigenvalues and eigenvectors of the input matrix
    e1 <- eigen(mat, symmetric = TRUE)
    val <- Re(e1$val)  # Real part of eigenvalues
    vec <- Re(e1$vec)  # Real part of eigenvectors
    val1 <- val / max(val)  # Normalize eigenvalues
    
    # Threshold eigenvalues
    I1 <- val1 > thresh
    I3 <- is.na(I1)
    
    # Compute the generalized inverse
    if (sum(I3) < 0.5) {
        val2 <- val[I1]
        I2 <- I1
        if (sum(I2) > 1.5) {
            ret <- vec[, I1] %*% diag(1 / sqrt(val2)) %*% t(vec[, I1])
        } else {
            v1 <- as.matrix(vec[, I1], length(c(vec[, I1])), 1)
            ret <- (1 / val2) * v1 %*% t(v1)
        }
    } else {
        ret <- diag(length(I1)) * 0
    }
    return(ret)
}

# Function to calculate the determinant of a matrix using matrix decomposition
# Inputs:
#   - mat: Input matrix
# Output:
#   - Determinant of the matrix
mdet <- function(mat){
  z1 <- eigen(mat)$val
  I1 <- z1>1e-12
  return(exp(sum(log(z1[I1]))))
}

# Function to include all the second-order interaction terms of the columns of a matrix
# Inputs:
#   - x: Input matrix
#   - only.squared: Logical indicating whether to create only squared interaction terms. Default is FALSE.
# Output:
#   - Matrix including all the second-order interaction terms
matrix.2nd.order <- function(x, only.squared=F){
  x0 <- x
  dimn <- dimnames(x)[[2]] 
  num.col <- length(x[1,]) 

  for(i in 1:num.col){
    if(!only.squared){
      for(j in i:num.col){
        x0 <- cbind(x0, x[,i]*x[,j])
        dimn <- c(dimn, paste(dimn[i], dimn[j], sep=""))
      }
    }
    
    else{
      x0 <- cbind(x0, x[,i]*x[,i])
      dimn <- c(dimn, paste(dimn[i], "2", sep=""))
      }
  }
  
  dimnames(x0)[[2]] <- dimn
  
  return(x0)
}

#takes a matrix mat and returns the trace
trace <-function(mat){
  sum(diag(mat))
}
