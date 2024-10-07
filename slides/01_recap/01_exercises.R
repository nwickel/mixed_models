# 01_exercises.R
#
# Solutions for exercises on Programming in R
#
# content: (1) Writing functions
#          (2) Conditional execution
#          (3) Loops
#          (4) Avoiding loops
#          (5) Random number generationLoops
#          (6) Data frames str()/summary()
#
# input: --
# output: --
#
# Last mod: Oct/07/2024, NW

#--------------- (1) Writing functions ----------------------------------------

x <- 1:20

csum <- function(x) {
  y <- x
  for(i in 2:length(x)) y[i] <- y[i - 1] + x[i]
  y
}

#--------------- (2) Conditional execution ------------------------------------

sign1 <- function(x){
  retval <- if(x < 0) -1
            else if(x == 0) 0
            else 1
  retval
}

# Alternatively
sign2 <- function(x) if(x < 0) -1 else if(x == 0) 0 else 1

#--------------- (3) Loops ----------------------------------------------------

x <- numeric(8)                            # w/allocating
for(n in seq_along(x)) x[n] <- n*2 + 1

x <- NULL                                  # w/growing
for(n in seq_len(8)) x <- c(x, n*2 + 1)

#--------------- (4) Avoiding loops -------------------------------------------

data(iris)

iris2 <- iris[, sapply(iris, is.numeric)]

m <- NULL
for (i in 1:4) {
  m <- c(m, mean(iris2[, i]))
}

colMeans(iris2)
lapply(iris2, mean)
sapply(iris2, mean)
aggregate(as.matrix(iris2) ~ 1, FUN = mean)
apply(iris2, 2, mean)

#--------------- (5) Random number generation ---------------------------------

dat <- data.frame(x = rnorm(n = 20, mean = 100, sd = 15), 
                  y = rbinom(n = 20, size = 10, prob = 0.2),
                  z = rpois(n = 20, lambda = 1))

dat$s <- with(dat, x + y + z)

#--------------- (6) Data frames str()/summary() ------------------------------

dat <- data.frame(
  id   = 1:50,
  hand = factor(rep(c("left","right"), each = 25),
                levels = c("right","left")),
  cond = factor(rep(1:5, 10)),
  RT   = rnorm(n = 50, mean = 400, sd = sqrt(625))
)

# or:

dat <- expand.grid(id = (1:30), hand = factor(c("left", "right")),
                   cond = factor(1:5))
dat$RT <- rnorm(n = 50, mean = 400, sd = sqrt(625))

str(dat)
summary(dat)

