############ Assignment 2: Part B ##############
library(boot)
# B.1 --------

# B.2 --------

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


