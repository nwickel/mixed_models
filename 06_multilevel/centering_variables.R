# centering_variables.R
#
# Answering questions about variable centering
#
# content: (1) Centering with GLMs?
#          (2) Centering DV in ANOVA?
#
# created: Jun/28/2021, NU

#--------------- (1) Centering with GLMs? ---------------

dat <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")

dat$gpac <- dat$gpa - mean(dat$gpa)
dat$grec <- dat$gre - mean(dat$gre)
dat$rankc <- dat$rank - mean(dat$rank)

glm1 <- glm(admit ~ gre + gpa + rank, dat, family=binomial)
glm2 <- glm(admit ~ gre + gpac + rank, dat, family=binomial)
glm3 <- glm(admit ~ grec + gpac + rankc, dat, family=binomial)
# --> Only affects intercept, as it should

# Plot

glm0 <- glm(admit ~ gpa, dat, family=binomial)
glm0c <- glm(admit ~ gpac, dat, family=binomial)

pre0 <- predict(glm0, type="resp", newdat=data.frame(gpa=seq(-2, 7, length.out=100)))
pre0c <- predict(glm0c, type="resp", newdat=data.frame(gpac=seq(-2, 7, length.out=100)))

plot(admit ~ gpa, dat, pch=16, xlab="GPA", ylab="P(admit)", ylim=c(0, 1), xlim=c(-2, 7))
points(admit ~ gpac, dat, col="blue")
lines(pre0 ~ seq(-2, 7, length.out=100))
lines(pre0c ~ seq(-2, 7, length.out=100), col="blue")

abline(v = mean(dat$gpa), h = predict(glm0, type="resp", newdat=data.frame(gpa=0)), lty=2)
abline(v = 0, h = predict(glm0c, type="resp", newdat=data.frame(gpac=0)), col="blue", lty=2)
# --> Shift on x-axis, same as for regular linear models

exp(coef(glm0c))
# (Intercept)        gpac 
#    0.451837    2.860821 
predict(glm0c, type="resp", newdat=data.frame(gpac=0)) / 
  (1 - predict(glm0c, type="resp", newdat=data.frame(gpac=0)))
# 0.451837 

exp(coef(glm0))
predict(glm0, type="resp", newdat=data.frame(gpa=0)) / 
  (1 - predict(glm0, type="resp", newdat=data.frame(gpa=0)))


#--------------- (2) Centering DV in ANOVA? ---------------

library(multcomp)
dat <- cholesterol

plot(response ~ trt, dat)

lm1 <- lm(response ~ trt, dat)

datm <- aggregate(response ~ trt, dat, mean)
plot(response ~ as.numeric(trt), datm, type="h")

dat$respc <- dat$response - mean(dat$response)

lm2 <- lm(respc ~ trt, dat)

model.matrix(lm2)

datm$respc <- aggregate(respc ~ trt, dat, mean)[,2]
plot(respc ~ as.numeric(trt), datm, type="h")

summary(lm1)
summary(lm2)

anova(lm1)
anova(lm2)

# with effect coding
dat$trt_eff <- dat$trt
contrasts(dat$trt_eff) <- "contr.sum"

lm3 <- lm(response ~ trt_eff, dat)
# --> compare to datm for centered DV!
datm

# 5th parameter
-sum(datm$respc[1:4])

anova(lm3)

