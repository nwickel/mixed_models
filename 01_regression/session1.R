# session1.R
#
# content: (1) Exercise 1
#          (2) Exercise 2
#          (3) Anscombe
#
# input: --
# output: anscombe.pdf
#
# last mod: April/15/2021, NU

library(lattice)

setwd("C:/Users/numbach/Nextcloud/Documents/teaching/regression/01_regression/figures/")

#--------------- (1) Exercise 1 ---------------

x <- 1:20
n <- length(x)
a <- 0.2
b <- 0.3
sigma <- 0.5
y <- 0.2 + 0.3*x + rnorm(n, sd=sigma)

dat <- data.frame(x, y)

lm1 <- lm(y ~ x, dat)
summary(lm1)

mean(resid(lm1))
sd(resid(lm1))
hist(resid(lm1), breaks=15)

# plot data
plot(y ~ x, dat)
abline(lm1)

#--------------- (2) Exercise 2 ---------------

n <- 100 # 40
x0 <- 1:20
x <- sample(x0, n, replace=TRUE)
a <- 0.2
b <- 0.3
sigma <- 0.5
y <- 0.2 + 0.3*x + rnorm(n, sd=sigma)

dat <- data.frame(x, y)

pars <- replicate(2000, {
  ysim <- 0.2 + 0.3*x + rnorm(n, sd=sigma)
  lm1 <- lm(ysim ~ x, dat)
  c(coef(lm1), sigma(lm1))
})

rowMeans(pars)
# standard errors
apply(pars, 1, sd)

hist(pars[1, ])
hist(pars[2, ])
hist(pars[3, ])

plot(y ~ jitter(x), dat)

#--------------- (3) Anscombe ---------------

data(anscombe)

lm1 <- lm(y1 ~ x1, anscombe)
lm2 <- lm(y2 ~ x2, anscombe)
lm3 <- lm(y3 ~ x3, anscombe)
lm4 <- lm(y4 ~ x4, anscombe)

rbind(coef(lm1), coef(lm2), coef(lm3), coef(lm4))

par(mfrow=c(2,2))
plot(y1 ~ x1, anscombe, pch=16, col="blue")
abline(lm1, lwd=2)
plot(y2 ~ x2, anscombe, pch=16, col="blue")
abline(lm2, lwd=2)
plot(y3 ~ x3, anscombe, pch=16, col="blue")
abline(lm3, lwd=2)
plot(y4 ~ x4, anscombe, pch=16, col="blue")
abline(lm4, lwd=2)

dat <- reshape(anscombe, direction="long", varying=list(1:4, 5:8),
               timevar="reg", v.names=c("x", "y"))[,-4]



pdf("anscombe.pdf", height=5, width=5.5, pointsize=10)
xyplot(y ~ x | as.factor(reg), dat, pch=16, type=c("p", "r"))
dev.off()

xyplot(y ~ x | as.factor(reg), dat, pch=16, type=c("p", "smooth"))
xyplot(y ~ x | as.factor(reg), dat, pch=16, type=c("p", "spline"))

par(mfrow=c(2,2))
plot(lm1)
plot(lm2)
plot(lm3)
plot(lm4)

