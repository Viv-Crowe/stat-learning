# Cubic splines ------

knots <- c(3)
Xs <- c(2, 0, 4, 2, 2, 5)

getPos <- function(x){
  return(x*(x>=0))
}

getDesignMatrixRow <- function(x, knots){
  row <- c(1, x, x^2, x^3)
  for (c in knots){
    row <- c(row, getPos((x - c)^3))
  }
  return(row)
}
getDesignMatrix <- function(Xs, knots){
  designMatrix <- matrix(nrow = length(Xs), ncol = length(knots) + 4)
  
  for (i in 1:length(Xs)){
    designMatrix[i, ] <- getDesignMatrixRow(Xs[i], knots)
  }
  return(designMatrix)
}

getDesignMatrix(Xs, knots)



# OLS regression

x <- c(2, 0, 4, 2, 2, 5)
y <- c(5, 2, 8, 5, 4, 6)

b <- solve(t(x) %*% x) %*% t(x) %*% y
b

b*x

b <- (10+8+30)/(4+4+25)
yFit <- b*x[4:6]

sum( (yFit - y[4:6])^2 )

mean(c(8.03026,7.181818))

# Subset 1 ----
train <- 1:3
validSet <- 4:6
# Compute parameter from training set
b <- sum(x[train]*y[train]) / sum(x[train]^2)
# Predict response of validSet set
yFit <- b*x[validSet]
# Compute error
mse1 <- sum((yFit - y[validSet])^2)/length(train)

# Subset 2 ----
train <- 4:6
validSet <- 1:3
# Compute parameter from training set
b <- sum(x[train]*y[train])/sum(x[train]^2)
# Predict response of validSet set
yFit <- b*x[validSet]
# Compute error
mse2 <- sum( (yFit - y[validSet])^2 )/length(validSet)

(mse1+mse2)/2
