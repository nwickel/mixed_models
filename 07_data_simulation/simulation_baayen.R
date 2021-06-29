# simulation_baayen.R
#
# content: (1) Create data frame
#          (2) Define parameters
#          (3) Create vectors and simulate data
#          (4) Simulate data using model matrices
#          (5) Visualize simulated data
#
# input: --
# output: --
#
# created: Jun/29/2021, NU
# last mod: Jun/29/2021, NU

library(lattice)
library(lme4)

#--------------- (1) Create data frame ---------------

datsim <- expand.grid(subject = factor(c("s1" , "s2" , "s3" )),
                      item = factor(c("w1" , "w2" , "w3" )),
                      soa = factor(c("long" , "short" )))
datsim <- datsim[order(datsim$subject), ]

#--------------- (2) Define parameters ---------------
beta0 <- 522.11
beta1 <- -18.89

sw  <- 21.1
sy0 <- 23.89
sy1 <- 9
ry  <- -1
se  <- 9.9

#--------------- (3) Create vectors and simulate data ---------------

# Fixed effects
b0 <- rep(beta0, 18)
b1 <- rep(rep(c(0, beta1), each=3), 3)
# Draw random effects
w <- rep(rnorm(3, mean=0, sd=sw), 6)
e <- rnorm(18, mean=0, sd=se)

# Bivariate normal distribution
sig <- matrix(c(sy0^2, ry*sy0*sy1, ry*sy0*sy1, sy1^2), 2, 2)
y01 <- mvtnorm::rmvnorm(3, mean=c(0, 0), sigma=sig)
y0 <- rep(y01[,1], each=6)
y1 <- rep(c(0, y01[1,2],
            0, y01[2,2],
            0, y01[3,2]), each=3)

datsim$rt <- b0 + b1 + w + y0 + y1 + e

#--------------- (4) Simulate data using model matrices ---------------

X <- model.matrix( ~ soa, datsim)
Z <- model.matrix( ~ 0 + item + subject + subject:soa, datsim,
                  contrasts.arg=list( subject=contrasts(datsim$subject,
                                                        contrasts=FALSE)))

# Fixed effects
beta <- c(beta0, beta1)
# Random effects
theta <- c(w = unique(w),
          y0 = y01[,1],
          y1 = y01[,2])

dat$rt2 <- X %*% beta + Z %*% theta + e

#--------------- (5) Visualize simulated data ---------------

xyplot(rt ~ soa | subject, datsim, group=item, type="b", layout=c(3,1))


