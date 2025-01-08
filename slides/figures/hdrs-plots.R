# mixed.R
# Analyses in Hedeker & Gibbons (2006, chap. 4)
#
# Input:  reisby.txt, reisbyt4.txt
# Output: --
#
# Contents: (1) Random intercept model
#           (2) Random intercept and trend MRM
#
# Last mod: Nov/30/2011, FW
# Last mod: Nov/25/2024, NU


library(lattice)
library(nlme)
library(lme4)

# Depressivitaet (HDRS) ueber sechs Wochen bei 66 stationaeren Patienten
# mit 225mg Imipramin ab Woche 2 (Reisby, 1977, Psychopharmacology)
dat      <- read.table("data/reisby.dat", header = TRUE)
dat$id   <- factor(dat$id)
dat$diag <- factor(dat$diag, levels = c("nonen", "endog"))
dat2     <- na.omit(dat)     # drop missing values

########## (1) Random intercept model ##########

## Doesn't really show the shrinkage
lme1 <- lme(hamd ~ week, dat2, ~1|id, method = "ML")
lm1  <- lm(hamd ~ week, dat2)        # pooled regressions
lml1 <- lmList(hamd ~ 1 | id, dat2)  # individual regressions

########## (2) Random slope model ##########

lme2 <- lme(hamd ~ week, dat2, ~week|id, method = "ML")
lm2  <- lm(hamd ~ week, dat2)           # pooled regressions
lml2 <- lmList(hamd ~ week | id, dat2)  # individual regressions

plot(augPred(lme2, ~week, level = 0:1))  # 6: 316, 505; 5: 304 4: 610

## Shrinkage
pdf("hdrs-shnk.pdf", height=5.1, width=5.1, pointsize=12)

par(mfrow = c(2,2), omi = c(.5,.5,.1,.1), mai = c(0,0,0,0), mgp = c(2,.7,0))

xlim <- c(-.3, 5.3); ylim <- c(0, 40)

id <- "316"
plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, col="grey")
abline(lm2)
abline(unlist(coef(lme2)[id,]), lty=2, col="darkblue")
abline(unlist(coef(lml2)[id,]), lty=3)
points(hamd ~ week, dat2[dat2$id == id,], pch=21, bg="white")
legend("topright", paste("ID", id), bty="n")
legend("bottom", c("OLS individual", "Empirical Bayes", "OLS pooled"),
  lty=3:1, col=c("black", "darkblue", "black"), bty="n")
axis(2)
axis(3, labels=FALSE)
box()

id <- "505"
plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, col="grey")
abline(lm2)
abline(unlist(coef(lme2)[id,]), lty=2, col="darkblue")
abline(unlist(coef(lml2)[id,]), lty=3)
points(hamd ~ week, dat2[dat2$id == id,], pch=21, bg="white")
legend("topright", paste("ID", id), bty="n")
axis(3, labels=FALSE)
axis(4, labels=FALSE)
box()

id <- "304"
plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, col="grey")
abline(lm2)
abline(unlist(coef(lme2)[id,]), lty=2, col="darkblue")
abline(unlist(coef(lml2)[id,]), lty=3)
points(hamd ~ week, dat2[dat2$id == id,], pch=21, bg="white")
legend("topright", paste("ID", id), bty="n")
axis(1)
axis(2)
box()

id <- "610"
plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, col="grey")
abline(lm2)
abline(unlist(coef(lme2)[id,]), lty=2, col="darkblue")
abline(unlist(coef(lml2)[id,]), lty=3)
points(hamd ~ week, dat2[dat2$id == id,], pch=21, bg="white")
legend("topright", paste("ID", id), bty="n")
axis(1)
axis(4, labels=FALSE)
box()

mtext("HDRS score", 2, 2, outer=TRUE)
mtext("Time (Week)", 1, 2, outer=TRUE)

dev.off()


########## (3) Quadratic trend model ##########

#lme3 <- lme(hamd ~ week + I(week^2), dat2, ~week + I(week^2)|id, method="ML")
lme3.0 <- lmer(hamd ~ week + I(week^2) + (week|id), dat2, REML = FALSE)
lme3 <- lmer(hamd ~ week + I(week^2) + (week + I(week^2)|id), dat2, REML = FALSE)
xval <- seq(0, 5, len = 100)

pdf("slides/figures/hdrs-pred.pdf", height = 6, width = 6, pointsize = 10)

xyplot(hamd + predict(lme3) ~ week | id, dat2,
       type = c("p", "l"),
       pch = 16,
       grid = TRUE,
       distribute.type = TRUE,
       layout = c(11, 6),
       ylab = "HDRS score",
       xlab="Time (Week)")

dev.off()

pdf("hdrs-quad.pdf", height = 3.375, width = 3.375, pointsize = 10)

par(mai = c(.6,.6,.1,.1), mgp = c(2,.7,0)) 

plot(1, type = "n", xlim = c(0, 5), ylim = c(5, 35),
  ylab = "HDRS score", xlab = "Time (Week)")
abline(v = 0, col = "grey")
lines(predict(lme3, data.frame(week = xval), re.form = NA) ~ xval)
lines(predict(lme3, data.frame(week = xval, id = 361)) ~ xval, lty = 2)
lines(predict(lme3, data.frame(week = xval, id = 607)) ~ xval, lty = 2)
text(c(2.5, 2.5, 2.5), c(11, 20.5, 30.5), c("ID 361", "Group trend",
  "ID 607"))

dev.off()


########## (4) Polynomials ##########

## Quadratic

f1 <- function(x, b0, b1, b2) b0 + b1*x + b2*x^2

pdf("quad.pdf", height=4.1, width=5.1, pointsize=10)

par(mfrow=c(2,2), omi=c(.44,.4,.1,.1), mai=c(0,0,0,0), mgp=c(2,.7,0))
xlim <- c(-.1, 5.1); ylim <- c(0, 16)

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, h=0, col="grey")
curve(f1(x, b0=2, b1=2, b2=-.25), 0, 5, add=TRUE, col="darkblue")
text(2.5, 2.5, "decelerated growth")
text(2.5, 1, expression(y == 2 + 2 %.% t - 0.25 %.% t^2))
axis(2)
axis(3, labels=FALSE)
box()

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, h=0, col="grey")
curve(f1(x, b0=2, b1=2, b2=.25), 0, 5, add=TRUE, col="darkblue")
text(2.5, 2.5, "accelerated growth")
text(2.5, 1, expression(y == 2 + 2 %.% t + 0.25 %.% t^2))
axis(3, labels=FALSE)
axis(4, labels=FALSE)
box()

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, h=0, col="grey")
curve(f1(x, b0=14, b1=-2, b2=.25), 0, 5, add=TRUE, col="darkblue")
text(2.5, 15, "decelerated decline")
text(2.5, 13.5, expression(y == 14 - 2 %.% t + 0.25 %.% t^2))
axis(1)
axis(2)
box()

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, h=0, col="grey")
curve(f1(x, b0=14, b1=-2, b2=-.25), 0, 5, add=TRUE, col="darkblue")
text(2.5, 15, "accelerated decline")
text(2.5, 13.5, expression(y == 14 - 2 %.% t - 0.25 %.% t^2))
axis(1)
axis(4, labels=FALSE)
box()

mtext("y", 2, 2, outer=TRUE)
mtext("Time, t", 1, 2, outer=TRUE)

dev.off()

## Cubic

f1 <- function(x, b0, b1, b2, b3) b0 + b1*x + b2*x^2 + b3*x^3

pdf("cubic.pdf", height=4.1, width=5.1, pointsize=10)

par(mfrow=c(2,2), omi=c(.44,.4,.1,.1), mai=c(0,0,0,0), mgp=c(2,.7,0))
xlim <- c(-.1, 5.1); ylim <- c(0, 16)

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, h=0, col="grey")
curve(f1(x, b0=1, b1=5.5, b2=-1.75, b3=0.25), 0, 5, add=TRUE, col="darkblue")
text(2.5, 1, expression(y == 1 + 5.5 %.% t - 1.75 %.% t^2 + 0.25 %.% t^3))
axis(2)
axis(3, labels=FALSE)
box()

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, h=0, col="grey")
curve(f1(x, b0=5, b1=-4.5, b2=3.15, b3=-0.4), 0, 5, add=TRUE, col="darkblue")
text(2.5, 1, expression(y == 5 - 4.5 %.% t + 3.15 %.% t^2 - 0.40 %.% t^3))
axis(3, labels=FALSE)
axis(4, labels=FALSE)
box()

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, h=0, col="grey")
curve(f1(x, b0=0, b1=-0.5, b2=2.3, b3=-.325), 0, 5, add=TRUE, col="darkblue")
text(2.5, 15, expression(y == 0 - 0.5 %.% t + 2.30 %.% t^2 - 0.325 %.% t^3))
axis(1)
axis(2)
box()

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=0, h=0, col="grey")
curve(f1(x, b0=1, b1=10.5, b2=-3.25, b3=.35), 0, 5, add=TRUE, col="darkblue")
text(2.5, 15, expression(y == 1 + 10.5 %.% t - 3.25 %.% t^2 + 0.35 %.% t^3))
axis(1)
axis(4, labels=FALSE)
box()

mtext("y", 2, 2, outer=TRUE)
mtext("Time, t", 1, 2, outer=TRUE)

dev.off()

## Cubic gone bad

bgtxt <- function(lx, ly, s, border="white", col="white"){
  hh <- strheight(s)/2
  ww <- strwidth(s)/2
  rect(lx - ww, ly - hh, lx + ww, ly + hh, border=border, col=col)
  text(lx, ly, labels=s)
}

pdf("cubic-gone-bad.pdf", height=4.1, width=5.1, pointsize=10)

par(mfrow=c(2,2), omi=c(.44,.4,.1,.1), mai=c(0,0,0,0), mgp=c(2,.7,0))
xlim <- c(-2.1, 7.1); ylim <- c(0, 16)

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=c(0, 5), h=0, col="grey")
curve(f1(x, b0=1, b1=5.5, b2=-1.75, b3=.25), -2, 7, add=TRUE, col="darkblue")
bgtxt(2.5, 1, expression(y == 1 + 5.5 %.% t - 1.75 %.% t^2 + 0.25 %.% t^3))
axis(2)
axis(3, labels=FALSE)
box()

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=c(0, 5), h=0, col="grey")
curve(f1(x, b0=5, b1=-4.5, b2=3.15, b3=-.4), -2, 7, add=TRUE, col="darkblue")
bgtxt(2.5, 1, expression(y == 5 - 4.5 %.% t + 3.15 %.% t^2 - 0.40 %.% t^3))
axis(3, labels=FALSE)
axis(4, labels=FALSE)
box()

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=c(0, 5), h=0, col="grey")
curve(f1(x, b0=0, b1=-.5, b2=2.3, b3=-.325), -2, 7, add=TRUE, col="darkblue")
bgtxt(2.5, 15, expression(y == 0 - 0.5 %.% t + 2.30 %.% t^2 - 0.325 %.% t^3))
axis(1)
axis(2)
box()

plot(1, type="n", xlim=xlim, ylim=ylim, axes=FALSE)
abline(v=c(0, 5), h=0, col="grey")
curve(f1(x, b0=1, b1=10.5, b2=-3.25, b3=.35), -2, 7, add=TRUE, col="darkblue")
bgtxt(2.5, 15, expression(y == 1 + 10.5 %.% t - 3.25 %.% t^2 + 0.35 %.% t^3))
axis(1)
axis(4, labels=FALSE)
box()

mtext("y", 2, 2, outer=TRUE)
mtext("Time, t", 1, 2, outer=TRUE)

dev.off()

