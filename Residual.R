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

# Function to create a residual plot
# Inputs:
#   - str: A list containing residuals and other model-related data.
#   - mat: A matrix or data frame containing additional data.
#   - idcol: Column index or name identifying the ID column in 'mat'.
#   - i: Index specifying the specific iteration or model component.
#   - varsel: Optional. Vector specifying the indices of variables to be included. Default is 0.
# Output:
#   - A scatterplot matrix displaying the residuals along with additional data.
Residplot1 <- function(str, mat, idcol, i, varsel = 0) {
  if (varsel[1] == 0) {
    n1 <- length(str$resid[[i]][1, ])
    varsel <- c(1:n1)
  }

  m3 <- str$resid[[i]]
  m1 <- m3[, c(1, varsel)]
  m2 <- convert.data(mat, idcol)
  m0 <- cbind(m2[, idcol], m1[, -1])
  pairs(m0, pch = as.character(m1[, 1]), col = m1[, 1])
}
