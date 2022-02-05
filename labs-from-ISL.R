### LAB 2.3 -----
# Some plotting examples for 3D data

x <- y <- seq(-pi, pi, length = 50)

f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))

contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)

fa <- (f - t(f)) / 2
contour(x, y, fa , nlevels = 15)

image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa , theta = 30)
persp(x, y, fa , theta = 30, phi = 20)
persp(x, y, fa , theta = 30, phi = 70)
persp(x, y, fa , theta = 30, phi = 40)

# Loading data
library(ISLR2)

Auto <- read.table("Datasets/Auto.data", header = T, na.strings = "?",
                   stringsAsFactors = T)
Auto <- na.omit(Auto) 
cylinders <- as.factor(cylinders) # turns quantitative data into qualitative
attach(Auto) # so we can refer directly to variables, instead of Auto$...

plot(cylinders , mpg)

plot(horsepower , mpg)
identify(horsepower , mpg , name) # click points on graph, then press esc


### LAB 3.6 -------

libary(MASS)
attach(Boston)
lm.fit <- lm(medv ~ lstat)

# to get values out of the model, it's better to use these 'extractor functions'
# as opposed to something like lm.fit$coefficients
names(lm.fit)
coef(lm.fit)

confint(lm.fit)

predict(lm.fit , data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction")

plot(lstat , medv)
abline(lm.fit)

par(mfrow = c(2, 2)) # paritions the plot window to view multiple at once
plot(lm.fit) # produces a four standard plots for evaluating a linear model

plot(hatvalues(lm.fit)) # hatvalues computes leverage statistics to identify possible outliers
which.max(hatvalues(lm.fit))

# add a new variable to the model
lm.fit1 <- lm(medv ∼.- age, data = Boston)
lm.fit1 <- update(lm.fit , ∼. - age)

# Interaction terms
# syntax lstat:black tells R to include an interaction term between lstat and black
# lstat*black is shorthand for lstat + black + lstat:black

# Non-linear transformations
# be careful: some symbols have special meaning in the formula object, 
# wrap operations with I() to avoid issues
lm.fit2 <- lm(medv ~ lstat +I(lstat^2))
summary(lm.fit2)

# compare including the quadratic term vs not w/ anova

lm.fit <- lm(medv ~ lstat)
anova(lm.fit , lm.fit2)

par(mfrow = c(2, 2))
plot(lm.fit2)

# Fitting polynomials
lm.fit5 <- lm(medv ~ poly(lstat,5))
summary(lm.fit5)

# Qualitative predictors
#TODO


### LAB 5.3 ------
library(boot)
set.seed(2022)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta
