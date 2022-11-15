# simulation_baayen_solution.R

library(lattice)
library(lme4)

n <- 30

datsim <- expand.grid(subject = factor(paste0("s", 1:n)),
                      item = factor(c("w1" , "w2" , "w3" )),
                      soa = factor(c("long" , "short" )))
datsim <- datsim[order(datsim$subject), ]

beta0 <- 522.11
beta1 <- -18.89

sw  <- 21.1
sy0 <- 23.89
sy1 <- 9
ry  <- -1
se  <- 9.9

w <- rnorm(3, mean=0, sd=sw)
e <- rnorm(n*6, mean=0, sd=se)

# Bivariate normal distribution
sig <- matrix(c(sy0^2, ry*sy0*sy1, ry*sy0*sy1, sy1^2), 2, 2)
y01 <- mvtnorm::rmvnorm(n, mean=c(0, 0), sigma=sig)

beta <- c(beta0, beta1)
# Random effects
theta <- c(w = w,
          y0 = y01[,1],
          y1 = y01[,2])

X <- model.matrix( ~ soa, datsim)
Z <- model.matrix( ~ 0 + item + subject + subject:soa, datsim,
                  contrasts.arg=list( subject=contrasts(datsim$subject,
                                                        contrasts=FALSE)))

datsim$rt <- X %*% beta + Z %*% theta + e

xyplot(rt ~ soa | subject, datsim, group=item, type="b", layout=c(5,6))

