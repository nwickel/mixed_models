# example_baayen.R
#
# content: (1) Read data
#          (2) Descriptive statistics
#          (3) Random intercept model
#          (4) Random slope model
#
# input: reisby.txt
# output: 
#
# created: Jul/20/2018, NU
# last mod: Jan/05/2023, NU

#setwd("C:/Users/nwickelmaier/Nextcloud/Documents/teaching/regression/06_data_simulation/")

library(lattice)
library(lme4)
library(xtable)

#--------------- (1) Read data ---------------

dat0 <- read.table(text=
 "subj item soa rt int intsoa itemint subint subsoa res
  s1 w1 Long  466 522.2 0     -28.3 -26.2 0       -2.0 
  s1 w2 Long  520 522.2 0     14.2    -26.2 0       9.8 
  s1 w3 Long  502 522.2 0     14.1    -26.2 0       -8.2 
  s1 w1 Short 475 522.2 -19 -28.3 -26.2 11      15.4 
  s1 w2 Short 494 522.2 -19 14.2    -26.2 11      -8.4 
  s1 w3 Short 490 522.2 -19 14.1    -26.2 11      -11.9 
  s2 w1 Long  516 522.2 0     -28.3 29.7    0       -7.4 
  s2 w2 Long  566 522.2 0     14.2    29.7    0       0.1 
  s2 w3 Long  577 522.2 0     14.1    29.7    0       11.5 
  s2 w1 Short 491 522.2 -19 -28.3 29.7    -12.5 -1.5 
  s2 w2 Short 544 522.2 -19 14.2    29.7    -12.5 8.9 
  s2 w3 Short 526 522.2 -19 14.1    29.7    -12.5 -8.2 
  s3 w1 Long  484 522.2 0     -28.3 -3.5  0       -6.3 
  s3 w2 Long  529 522.2 0     14.2    -3.5  0       -3.5 
  s3 w3 Long  539 522.2 0     14.1    -3.5  0       6.0 
  s3 w1 Short 470 522.2 -19 -28.3 -3.5  1.5     -2.9 
  s3 w2 Short 511 522.2 -19 14.2    -3.5  1.5     -4.6 
  s3 w3 Short 528 522.2 -19 14.1    -3.5  1.5     13.2 "
  , header=TRUE)

dat <- dat0[, 1:4]

mean(dat0$itemint)
mean(dat0$subint)
mean(dat0$subsoa)
mean(dat0$res)

sd(dat0$itemint)
sd(dat0$subint)
sd(dat0$subsoa)
sd(dat0$res)

#--------------- (2) Descriptive statistics ---------------

aggregate(rt ~ soa, dat, mean)
aggregate(rt ~ item, dat, mean)
aggregate(rt ~ soa + item, dat, mean)

sd(dat0$subint)
sd(dat0$intsoa)
sd(dat0$subsoa)
sd(dat0$itemint)
sd(dat0$res)
cor(dat0$subint, dat0$subsoa)

# plot
pdf("baayen_ex.pdf", width=7, height=3.375, pointsize=10)
print(xyplot(rt ~ soa | subj, dat, group=item, type="b", auto.key=T, xlab="SOA",
       ylab="Reaction time"))
dev.off()

means <- aggregate(rt ~ soa, dat, mean)
se_long <- sd(dat$rt[dat$soa == "Long"]) / 3
se_short <- sd(dat$rt[dat$soa == "Short"]) / 3

pdf("baayen_ex_agg.pdf", width=3.375, height=3.375, pointsize=10)
par(mai=c(.6,.6,.1,.1), mgp=c(2.4, 1, 0))
#
plot(rt ~ as.numeric(soa), means, ylim=c(450, 550), xlim=c(.5,2.5),
     type="b", pch=16, axes=F, xlab="SOA", ylab="Reaction time")
axis(1, at=1:2, labels=c("Long", "Short"))
axis(2)
box()
#
arrows(1, means$rt[1] - se_long, 1, means$rt[1] + se_long, code=3,
       angle=90, length=.05)
arrows(2, means$rt[2] - se_short, 2, means$rt[2] + se_short, code=3,
       angle=90, length=.05)
#
dev.off()

#--------------- (3) Random intercept and random slope model ---------------

lme1 <- lmer(rt ~ soa + (1 | item) + (soa | subj), dat, REML=F)
summary(lme1)

lme2 <- lmer(rt ~ soa + (1 | item) + (1 | subj) + (1 | subj:soa), dat, REML=F)
lme3 <- lmer(rt ~ soa + (1 | item) + (1 | subj), dat, REML=F)
lme4 <- lmer(rt ~ soa + (1 | subj), dat, REML=F)

anova(lme4, lme3, lme2, lme1)

confint(lme3, method="boot")

# looking at model assumptions
pdf("figures/baayen_assumptions.pdf", width=5, height=5, pointsize=10)
par(mai=c(.6,.6,.1,.1), mgp=c(2.4, 1, 0))
#
plot(lme3)
#
dev.off()

pdf("baayen_qqnorm.pdf", width=5, height=5, pointsize=10)
par(mai=c(.6,.6,.6,.1), mgp=c(2.4, 1, 0))
#
qqnorm(resid(lme3))
qqline(resid(lme3))
#
dev.off()


