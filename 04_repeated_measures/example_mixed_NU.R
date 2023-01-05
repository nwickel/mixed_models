# example_mixed.R
#
# content: (1) Read data
#          (2) Descriptive statistics
#          (3) Repeated measures ANOVA
#          (4) Random intercept model
#          (5) Random slope model
#          (6) Include diagnosis
#
# input: reisby.txt
# output: hdrs-ind.pdf, hdrs-means.pdf, hdrs-means-se.pdf, hdrs-lme1.pdf, hdrs-lme2.pdf
#
# created: Jul/20/2018, NU
# last mod: Jun/07/2021, NU

# setwd("C:/Users/nwickelmaier/Nextcloud/Documents/teaching/regression/04_repeated_measures/")

library(lattice)
library(lme4)
library(ez)

#--------------- (1) Read data ---------------

dat      <- read.table("reisby.dat", header=TRUE)
dat$id   <- factor(dat$id)
dat$diag <- factor(dat$diag, levels=c("nonen", "endog"))
dat      <- na.omit(dat)     # drop missing values

pdf("figures/hdrs-ind.pdf", height=5.5, width=7.7, pointsize=8)

xyplot(hamd ~ week | id, dat, type=c("g", "r", "p"), pch=16,
  strip = strip.custom(bg="grey96"),
  par.strip.text=list(cex=.8), layout=c(11, 6),
  ylab="HDRS score", xlab="Time (week)")

dev.off()

#--------------- (2) Descriptive statistics ---------------

means    <- aggregate(hamd ~ week + diag, dat, mean)
means$sd <- aggregate(hamd ~ week + diag, dat, sd)[, 3]
means$n  <- aggregate(hamd ~ week + diag, dat, length)[, 3]
means$se <- with(means, sd/sqrt(n))

pdf("figures/hdrs-means.pdf", height=3.375, width=3.375, pointsize=10)

par(mai=c(.6,.6,.1,.1), mgp=c(2,.7,0))
plot(hamd ~ week, means[means$diag == "nonen",], type="b", ylim=c(0,28),
     xlab="Week", ylab="HDRS score", pch=16)
points(hamd ~ week, means[means$diag == "endog",], type="b", pch=21, bg="white", lty=2)
legend("bottomleft", c("Endogenous", "Non endogenous"), lty=1:2, pch=c(16,21),
  bty="n", pt.bg="white")

dev.off()


pdf("figures/hdrs-means-se.pdf", height=3.375, width=3.375, pointsize=10)

par(mai=c(.6,.6,.1,.1), mgp=c(2,.7,0))
plot(hamd ~ I(week-.02), means[means$diag == "nonen",], type="b",
     ylim=c(0,28), xlab="Week", ylab="HDRS score", pch=16)
with(means[means$diag == "nonen",], arrows(0:5-0.02, hamd - se, 0:5-0.02,
                                           hamd + se, code=3, angle=90,
                                           length=.05))
with(means[means$diag == "endog",], arrows(0:5+0.02, hamd - se, 0:5+0.02,
                                           hamd + se, code=3, angle=90,
                                           length=.05))
points(hamd ~ I(week+.02), means[means$diag == "endog",], type="b", pch=21,
       bg="white", lty=2)

legend("bottomleft", c("Endogenous", "Non endogenous"), lty=1:2, pch=c(16,21),
  bty="n", pt.bg="white")

dev.off()

#--------------- (3) Repeated measures anova ---------------

aggregate(hamd ~ week, dat, mean)
aggregate(hamd ~ week, dat, sd)
aggregate(hamd ~ week, dat, length)

cor(reshape(dat[, c("hamd", "id", "week")], 
            direction="wide",
            timevar="week")[, 2:7],
    use="pairwise.complete.obs")

# Week needs to be a factor when computing a ANOVA
dat$week2 <- factor(dat$week)
summary(aov(hamd ~ week2 + Error(id/week2), dat))

# "SPSS"-style
ezANOVA(data = dat, dv = hamd, wid = id,
        within = week2, type = 3)

# Check data
ezDesign(data = dat, x = week, y = id, col = diag)

# Remove IDs with missing observations
ids <- names(which( addmargins(
  xtabs( ~ week + diag + id, dat) )
  ["Sum", "Sum",] == 6))
dat_val <- dat[dat$id %in% ids, ]

# Fit ANOVAs again
aov1 <- aov(hamd ~ week2 + Error(id/week2), dat_val)
summary(aov1)

ez1 <- ezANOVA(data = dat_val, dv = hamd, wid = id,
               within = week2, type = 3)
ez1$ANOVA
 
# How close can we get with a mixed-effects model?
lme1 <- lmer(hamd ~ week2 + (1 | id), dat_val)
anova(lme1)
summary(lme1)

lme0 <- lmer(hamd ~ 1 + (1 | id), dat_val)
anova(lme0, lme1)
 
# Calculate mean sum of squares for id by hand
sp <- attr(VarCorr(lme1)$id, "stddev")
se <- attr(VarCorr(lme1), "sc")
se^2 + 6*sp^2

## Now add the group factor back in
# Fit ANOVAs again
aov2 <- aov(hamd ~ week2*diag + Error(id/week2),
            dat_val)
summary(aov2)

ez2 <- ezANOVA(data = dat_val, dv = hamd, wid = id,
               within = week2, between = diag,
               type = 3)
ez2$ANOVA
 
lme2 <- lmer(hamd ~ week2*diag + (1 | id), dat_val)
anova(lme2)

# Calculate mean sum of squares for id by hand
sp <- attr(VarCorr(lme2)$id, "stddev")
se <- attr(VarCorr(lme2), "sc")
se^2 + 6*sp^2
 
# And this works on the complete data set
lme3 <- lmer(hamd ~ week2*diag + (1 | id), dat)
anova(lme3)

# Type I sum of squares
m0 <- lmer(hamd ~ 1 + (1 | id), dat, REML=FALSE)
m1 <- lmer(hamd ~ week2 + (1 | id), dat, REML=FALSE)
m2 <- lmer(hamd ~ week2 + diag + (1 | id), dat,
           REML=FALSE)
anova(m0, m1, m2, lme3)
 

#--------------- (4) Random intercept model ---------------

lme1 <- lmer(hamd ~ week + (1|id), dat, REML=FALSE)

plot(lme1, resid(.) ~ week | id, abline=0, cex=.7,
  strip = strip.custom(bg="grey96"),
  par.strip.text=list(cex=.8), layout=c(11, 6), type=c("p","h"))

rfx  <- ranef(lme1)$id[order(ranef(lme1)$id$"(Intercept)"), ,drop=FALSE]

pdf("figures/hdrs-lme1.pdf", height=3.375, width=3.375, pointsize=10)

par(mai=c(.6,.6,.1,.1), mgp=c(2,.7,0))
plot(1, type="n", xlim=c(0, 5), ylim=c(5, 35),
  ylab="HDRS score", xlab="Time (week)")
abline(v=0, col="grey")
abline(a=fixef(lme1)[1], b=fixef(lme1)[2])
abline(a=fixef(lme1)[1] + rfx["505",], b=rep(fixef(lme1)[2], 4), lty=2)
abline(a=fixef(lme1)[1] + rfx["123",], b=rep(fixef(lme1)[2], 4), lty=2)
abline(a=fixef(lme1)[1] + rfx["360",], b=rep(fixef(lme1)[2], 4), lty=2)
text(2, c(9.5, 15.5, 21.5, 29), c("ID 505", "ID 123", "Group trend",
  "ID 360"))

dev.off()

#--------------- (5) Random slope model ---------------

lme2 <- lmer(hamd ~ week + (week | id), dat, REML=FALSE)
rfx  <- ranef(lme2)$id

pdf("figures/hdrs-lme2.pdf", height=3.375, width=3.375, pointsize=10)

par(mai=c(.6,.6,.1,.1), mgp=c(2,.7,0))
plot(1, type="n", xlim=c(0, 5), ylim=c(5, 35),
  ylab="HDRS score", xlab="Time (week)")
abline(v=0, col="grey")
abline(a=fixef(lme2)[1], b=fixef(lme2)[2])
abline(as.numeric(fixef(lme2) + rfx["101",]), lty=2)
abline(as.numeric(fixef(lme2) + rfx["319",]), lty=2)
abline(as.numeric(fixef(lme2) + rfx["354",]), lty=2)
text(c(3.2, 4.2, 3.2, 3.2), c(7.7, 10, 18.7, 25.5), c("ID 101", "ID 319",
  "Group trend", "ID 354"))

dev.off()


#--------------- (6) Include diagnosis ---------------

lme3 <- lmer(hamd ~ week*diag + (week | id), dat, REML=FALSE)
anova(lme2, lme3)

# boot strap
nsim <- 400
LR <- rep(NA, nsim)
sim2 <- simulate(lme2, nsim, REML=FALSE)
for (i in seq_len(nsim)) {
  fit1 <- lmer(sim2[, i] ~ week + (week | id), dat, REML=FALSE)
  fit2 <- lmer(sim2[, i] ~ week*diag + (week | id), dat, REML=FALSE)
  LR[i] <- 2*(summary(fit2)$logLik - summary(fit1)$logLik)
}

hist(LR, breaks=20, col="grey", border="white", freq=FALSE, main="")
curve(dchisq(x, df=2), add=T)
abline(v=4.11, lty=2)
mean(LR > 4.11)  # empirical p-value: .135

# predictions with diagnosis as fixed effect
plot(predict(lme3, data.frame(week=0:5, diag="endog"), re.form=~0) ~ I(0:5),
  type="l", ylim=c(0,28), xlab="Week", ylab="HDRS score", axes=F)
lines(predict(lme3, data.frame(week=0:5, diag="nonen"), re.form=~0) ~ I(0:5),
  lty=2)
points(hamd ~ week, means[means$diag == "nonen",], pch=16)
points(hamd ~ week, means[means$diag == "endog",], pch=21, bg="white")
axis(1)
axis(2, seq(0, 28,4))
box(bty="L")
legend("bottomleft", c("Endogenous", "Non endogenous"), lty=1:2, pch=c(16,21),
  bty="n")



