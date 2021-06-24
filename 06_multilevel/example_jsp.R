# example_jsp.R
#
# content: (1) Inspect data
#          (2) Descriptive statistics and plots
#          (3) Centering of variables
#          (4) Random intercept model
#          (5) Multilevel model
#
# input: --
# output: jsp_box1.pdf, jsp_box2.pdf, jsp_lattice1.pdf, jsp_lattice2.pdf,
#         jsp_scatter.pdf
#
# created: Jun/23/2021, NU
# last mod: Jun/23/2021, NU

library(faraway)
library(lattice)
library(lme4)

setwd("C:/Users/numbach/Nextcloud/Documents/teaching/regression/06_multilevel/figures/")

#--------------- (1) Inspect data ---------------

data(jsp)
str(jsp)
xtabs( ~ school + class, jsp, sparse=T)

# Investigate nested structure of the data
class2 <- with(jsp, factor(school:class))
xtabs( ~ school + class2, jsp, sparse=T)

# Several data points per student
table(jsp$year)

dat <- jsp[jsp$year == 0, ]
# --> for simplicity, let's just consider the first measurement

#--------------- (2) Descriptive statistics and plots ---------------

plot(math ~ social, jsp,
  xlab="Social Class",
  ylab="Math Score")

# Plots for slides
pdf("jsp_box1.pdf", height=3.375, width=3.375, pointsize=10)

par(mai=c(.6,.6,.1,.1), mgp=c(2.4,1,0))
plot(math ~ social, jsp, xlab="Social Class", ylab="Math Score")

dev.off()

pdf("jsp_box2.pdf", height=3.375, width=3.375, pointsize=10)

par(mai=c(.6,.6,.1,.1), mgp=c(2.4,1,0))
plot(math ~ gender, jsp, xlab="Gender", ylab="Math Score")

dev.off()

pdf("jsp_scatter.pdf", height=3.375, width=3.375, pointsize=10)

par(mai=c(.6,.6,.1,.1), mgp=c(2.4,1,0))
plot(jitter(math) ~ jitter(raven), dat, cex=.5, 
  xlab="Raven Score", ylab="Math Score", 
  xlim=c(0, 36), col="gray")

dev.off()

#--------------- (3) Centering of variables ---------------

# Centering around grand mean
dat$craven <- dat$raven - mean(dat$raven)

lm1 <- lm(math ~ raven, dat)
lm2 <- lm(math ~ craven, dat)

# Visualization
plot(jitter(math) ~ jitter(raven), dat, cex=.7, 
  xlab="Raven score", ylab="Math score", 
  xlim=c(0, 36), col="gray")
abline(v = 0, h = coef(lm1)[1], col="darkgray")
abline(v = mean(dat$raven), h = coef(lm2)[1],
  col="darkgray")
abline(lm1)

# Centering around group mean for each school
## add mean raven score per school
dat$mraven <- with(dat, ave(raven, school))
dat$mraven <- dat$mraven - mean(dat$mraven)
## center raven score: mean=0 for each school
dat$gcraven <- dat$craven - dat$mraven

aggregate(craven ~ school, dat, mean)
aggregate(gcraven ~ school, dat, mean)

# Visualization
plot(jitter(math) ~ jitter(craven), dat, cex=.7, 
  xlab="Centered raven score", ylab="Math score", 
  col="gray")
abline(v = unique(dat$mraven), col="lightblue")
abline(v = mean(dat$craven), col="blue", lwd=2)

plot(math ~ gcraven, jsp,
  xlab="Raven Score",
  ylab="Math Score")

pdf("jsp_lattice1.pdf")

xyplot(math ~ gcraven | school, data=dat, xlab="School-centered Raven Score", 
       ylab="Math Score", type=c("p", "g", "r"))

dev.off()

pdf("jsp_lattice2.pdf", width=15, height=5)

xyplot(math ~ gcraven | school:class, data=dat, xlab="School-centered Raven Score", 
       ylab="Math Score", type=c("p", "g", "r"), layout=c(31, 3))

dev.off()


#--------------- (4) Random intercept model ---------------

lme1 <- lmer(math ~ craven*social + (1 | school) + 
  (1 | school:class), dat, REML=F)

# significance tests
confint(lme1, method="Wald")

# model diagnostics
plot(lme1)
qqnorm(resid(lme1)); qqline(resid(lme1))

#--------------- (5) Multilevel model ---------------

lme2 <- lmer(math ~ mraven*gcraven + gcraven + 
  (gcraven | school), dat, REML=F)

lme3 <- lmer(math ~ mraven*gcraven + gcraven + social +
  (gcraven | school), dat, REML=F)

lme4 <- lmer(math ~ mraven*gcraven + gcraven*social + 
  (gcraven | school), dat, REML=F)

anova(lme2, lme3, lme4)

summary(lme3)
confint(lme3)



