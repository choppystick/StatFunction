#performs a Gram-Schmidt orthogonalization on on a matrix's rows to produce an orthonormal basis for its row space.
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

#create a generalized inverse matrix for a matrix input, given a certain threshold as indication for zero.
gen.inv <- function(mat, thresh = 1e-10){
  v1 <- sum(is.na(mat))
  v2 <- sum(is.inf(mat))

  if((v1 + v2) > 0.5) {
    print(mat)
  }

  e1 <- eigen(mat, symmetric = T)
  val <- Re(e1$val)
  vec <- Re(e1$vec)
  val1 <- val/max(val)
  #
  #	print("normalized eigen values")
  #	print(val1)	#
  #	n1 <- length(val1)
  #	plot(c(1:n1), abs(val1), log = "y", xlab = "eigen rank", ylab
  #		 = "log10 of value")

  I1 <- val1 > thresh
  I3 <- is.na(I1)

  if(sum(I3) < 0.5) {
    val2 <- val[I1]
    I2 <- I1

    if(sum(I2) > 1.5) {
      ret <- vec[, I1] %*% diag(1/val2) %*% t(vec[, I1])
    }

    else {
      v1 <- as.matrix(vec[, I1], length(c(vec[, I1])), 1)
      ret <- (1/val2) * v1 %*% t(v1)
    }
  }

  else {
    ret <- diag(length(I1)) * 0
  }

  return(ret)
}

#calculates the determinant of the matrix using matrix decomposition. Takes the logarithm and exponentiating the result to compute the product of the eigenvalues in 
#a numerically stable way.
mdet <- function(mat){
  z1 <- eigen(mat)$val
  I1 <- z1>1e-12
  return(exp(sum(log(z1[I1]))))
}

#takes a matrix x as input and creates a new matrix x0 that includes all the second-order interaction terms (i.e., products of pairs of variables) of the columns of x.
#only.squared: function only create squared interaction terms if True 
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
