# mixed2.R
# Mixed-effects models (2)
#
# Input:  reisby.txt
# Output: --
#
# Contents: (1) Read data
#           (2) Curvilinear trend model
#
# Last mod: Dec/06/2011, FW


library(lattice)
library(nlme)

setwd("W:/teaching/mie/2011ws/07-mixed2/exer/")


########## (1) Read data ##########

dat      <- read.table("../../06-mixed1/exer/reisby.txt", header=TRUE)
dat$id   <- factor(dat$id)
dat$diag <- factor(dat$diag, levels=c("nonen", "endog"))
dat2     <- na.omit(dat)     # drop missing values


########## (2) Curvilinear trend model ##########

lme1 <- lme(hamd ~ week + I(week^2), dat2, ~week + I(week^2)|id, method="ML")

Su <- getVarCov(lme1)                     # Kovarianzmatrix der zuf. Effekte

Z <- cbind(1, 0:5, (0:5)^2)
Z %*% Su %*% t(Z) + lme1$sigma^2*diag(6)  # marginale Kovarianzmatrix

cov(matrix(dat$hamd, nc=6, byrow=TRUE), use="pair")  # empirische Kov.matrix

## Modellvorhersagen
plot(augPred(lme1, ~week, level=0:1), pch=16, par.strip.text=list(cex=.7))
plot(lme1, resid(.) ~ week|id, abline=0)  # siehe ?plot.lme

## Welche zufaelligen Effekte sind noetig?

## LRT: Individueller quadratischer Trend?
lme2 <- lme(hamd ~ week + I(week^2), dat2, ~week|id, method="ML")
anova(lme2, lme1)

## Vergleich der Modellvorhersagen
plot(comparePred(lme1, lme2, ~week))

## Variation der individuellen Regressionskoeffizienten
lml1 <- lmList(hamd ~ week + I(week^2)|id, dat2)
plot(intervals(lml1))

## OLS-Residuen gegen Zeit
xyplot(resid(lm(hamd ~ week)) ~ week|diag,
  dat2[dat2$id %in% sample(dat2$id, 6),], groups=id, type="l")

## Wie unterscheiden sich die Parameterschaetzungen?
plot(compareFits(coef(lme1), coef(lme2)))

