# 01_exercises.R
#
# content: (1) Anscombe
#          (2) Exercise 1
#          (3) Exercise 2
#          (4) Exercise 3
#          (5) Exercise 4
#
# input: --
# output: anscombe.pdf
#
# last mod: Oct/14/2024, NW

library(lattice)

#--------------- (1) Anscombe -------------------------------------------------

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

#--------------- (2) Exercise 1 -----------------------------------------------

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

#--------------- (3) Exercise 2 -----------------------------------------------

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

#--------------- (4) Exercise 3 ---------------

x   <- rnorm(100, mean = 1)
y   <- rnorm(100, mean = 2)
dat <- data.frame(id    = 1:200,
                  group = rep(c("x","y"), each = 100),
                  score = c(x, y))

rm(x,y)

t1   <- t.test(score ~ group, data = dat, var.equal = TRUE)
lm1  <- lm(score ~ group, data = dat)
aov1 <- aov(score ~ group, data = dat)
(stat <- list(
    coef = matrix(c(t1$estimate, lm1$coef, aov1$coef),
                  nrow = 2,
                  ncol = 3,
                  dimnames = list(NULL, c("ttest", "lm", "aov"))),
    statistics = matrix(c(t = t1$statistic^2,
                          Flm = summary(lm1)$fstatistic[1],
                          Faov = unlist(summary(aov1))[7]),
                        nrow = 1,
                        ncol = 3,
                        dimnames = list(NULL, c("t","Flm","Faov"))))
)
#--------------- (5) Exercise 4 ---------------

data(cars)

lm1 <- lm(dist ~ speed, cars)
summary(lm1)

hist(resid(lm1))

par(mfrow=c(2,2))
plot(lm1)

lm2 <- lm(dist ~ speed + I(speed^2), cars)

anova(lm1, lm2)

