############ Assignment 2: Part B ##############


# B.1 --------
# This code has been adapted from code by Prof. Frédéric Godin (Concordia)

PredictNearestNeighborsParzen1D <- function(yval, xval, xpred, WindowSize){

  #
  # INPUTS:
  #   yval: dependent variable for each data point
  #   xval: independent variable for each data point
  #   xpred: vector of independent variable values at which the forecast is made 
  #   WindowSize: the region around xval used to make the prediction  
  # OUTPUTS:
  #   PWpreds: vector of predictions for each entry of the vector xpred
  #
  
  nout = length(xpred)
  PWpreds = rep(0,nout)
  
  for(i in 1:nout){
    distances <- sqrt((xval - xpred[i])^2)
    indexPW = which(distances <= WindowSize)
    PWpreds[i] = mean(yval[indexPW])
  }
  
  return(PWpreds)
  
}

# Part b - simulating the data ----

set.seed(2022) #Set the random seed (ensures results are reproducible)


nIN = 200 # number of  observations insample
nOOS = 200 # number of  observations out-of-sample

x_min =-2 # min x value
x_max = 2 # max x value
xvalIS = (x_max-x_min)*runif(nIN) + x_min
xvalOOS = (x_max-x_min)*runif(nOOS) + x_min

fIS = 0.05*xvalIS^5 -3*xvalIS^2 # true pattern for in-sample observations
fOOS = 0.05*xvalOOS^5 -3*xvalOOS^2 # true pattern for out-of-sample observations
yobsIS = fIS + rnorm(nIN) # noisy in sample observations
yobsOOS = fOOS + rnorm(nOOS) # noisy out-of-sample observations

# Part c - predictions ----

xpred <- seq(from=x_min, to=x_max, by = 0.01)
PWvalues <- c(0.1, 0.5)

plot(xvalIS,yobsIS,main='Parzen Window Prediction',xlab = 'Predictor x', ylab = 'Response y')

colors <- viridis::rocket(3) # color rainbow for the plot

for(PW in PWvalues) {
  
  PWpreds = PredictNearestNeighborsParzen1D(yval=yobsIS,xval=xvalIS,xpred=xpred, WindowSize = PW)
  
  lines(xpred, PWpreds, col=colors[which(PWvalues == PW)], lwd = 1.8)
  
}

legendlist = c(paste("D=",toString(PWvalues[1]),sep=""),  paste("D=",toString(PWvalues[2]),sep=""))
legend(y=-6, x=-1, legend=legendlist, col=colors, lty=c(1,1), lwd = 1.8)

# Part d

PWvalues2 = seq(0.05,1, 0.05)
ValidationMSE = rep(0,length(PWvalues2))
TrainingMSE = rep(0,length(PWvalues2))

for(PW in 1:length(PWvalues2)) {
  
  PWpredsIS = PredictNearestNeighborsParzen1D(yval=yobsIS,xval=xvalIS,xpred=xvalIS,WindowSize=PWvalues2[PW])
  PWpredsOOS = PredictNearestNeighborsParzen1D(yval=yobsIS,xval=xvalIS,xpred=xvalOOS,WindowSize=PWvalues2[PW])
  
  ValidationMSE[PW] = mean( (PWpredsOOS - yobsOOS)^2)
  TrainingMSE[PW] = mean( (PWpredsIS - yobsIS)^2 )
  
}

# Part f

plot(PWvalues2, TrainingMSE, main='Training vs Validation MSE', type='n',
     ylab='MSE',xlab='D')
lines(PWvalues2, TrainingMSE, col='blue', type="p", pch=1)
lines(PWvalues2, ValidationMSE, col='red', type="p", pch=1)

# Part f

bestD <- PWvalues2[which.min(ValidationMSE)]

abline(v=bestD, col='darkgreen')
legend(y=1.3, x=0.63, legend=c('Training MSE','Validation MSE','Optimal D'), col=c('blue','red','darkgreen') , lty=c(0,0,1), pch=c(1,1,NaN))


# B.2 --------

library(ISLR2)

Auto <- read.table("Datasets/Auto.data", header = T, na.strings = "?",
                   stringsAsFactors = T)
Auto <- na.omit(Auto) 

# Part a
plot(Auto)

# Part b
# remove "name" variable, then find correlation matrix
colNumOfName <- match("name", names(Auto)) 
cor(Auto[-colNumOfName])

# Part c
lm.fit <- lm(mpg ~. -name, data = Auto)
summary(lm.fit)

# Part d
plot(lm.fit)

# identify the point with high leverage
which.max(hatvalues(lm.fit))

# Part e

# Get just the quantitative variables
colNumOfQualVars <- match(c("name", "cylinders", "origin"), names(Auto))

# define MSE function
mse <- function(sm) 
  mean(sm$residuals^2)

formulas <- list()
errors <- vector()

formulas[[1]] <- as.formula("mpg ~ (.)^2")
formulas[[2]] <- as.formula("mpg ~ . + displacement:horsepower + acceleration:weight")
formulas[[3]] <- as.formula("mpg ~ displacement*horsepower")
formulas[[4]] <- as.formula("mpg ~ weight*year + horsepower*acceleration")
formulas[[5]] <- as.formula("mpg ~ weight*year + horsepower*acceleration + displacement")
formulas[[6]] <- as.formula("mpg ~ . -year:weight")
# formulas[[7]] <- as.formula("mpg ~ .")
# formulas[[8]] <- as.formula("mpg ~ .")
# formulas[[9]] <- as.formula("mpg ~ .")

for(i in 1:length(formulas)){
  lm.fit <- lm(formulas[[i]] , data = Auto[-colNumOfQualVars])
  sm <- summary(lm.fit)
  errors[i] <- mse(sm)
}

plot(errors)

lm.fitAll <- lm(formulas[[1]] , data = Auto)

# Considering all predictors and their pair-wise interactions
lm.fitAll <- lm(mpg ~ (.-name)^2 , data = Auto)
summary(lm.fitAll)

lm.fit <- lm(mpg ~.-name + acceleration:horsepower, data = Auto)
summary(lm.fit)

# Part f

# displacement^2
lm.fit <- lm(mpg ~. -displacement + I(displacement^2), data = Auto[-colNumOfQualVars])
summary(lm.fit)

# log() on mpg ----
lm.fit <- lm(log(mpg) ~., data = Auto[-colNumOfQualVars])
summary(lm.fit)


# B.3 --------
# Section 5.4, exercise 8 from ISL
# Part a
set.seed (1)
x <- rnorm (100)
y <- x - 2 * x^2 + rnorm (100)
df <- data.frame(x, y)

# Part b
plot(x, y)
# It looks like a quadratic relationship, values appear more dense around the origin

# Part c
set.seed(2022)

cv.error <- rep(0, 4)

for (i in 1:4){
  glm.fit <- glm(y ~ poly(x, i), data = df)
  cv.error[i] <- cv.glm(df, glm.fit)$delta[1]
}
cv.error
round(cv.error, 3)

# Part d
set.seed(2023)

cv.error <- rep(0, 4)
fits <- list()

for (i in 1:4){
  fits[[i]] <- glm(y ~ poly(x, i), data = df)
  cv.error[i] <- cv.glm(df, fits[[i]])$delta[1]
}
cv.error
round(cv.error, 3)

# Part e (see pdf)

# Part f (see pdf for interpretation)
for (i in 1:4){
  print(paste("Model ", i))
  print(summary(fits[[i]]))
}


