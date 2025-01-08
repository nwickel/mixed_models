# example_winter.R
#
# Example on growth curve models, taken from https://doi.org/10.1093/jole/lzv003
#
# content: (1) Read data
#          (2) Visualization of data
#          (3) Centering of variables
#          (4) Fitting models
#             (4.1) Check random effects structure
#             (4.2) Test quadratic and linear effects
#             (4.3) Check model assumptions
#             (4.4) Test higher-order polynomials 
#             (4.5) Visualization of model predictions
#
# input:  example2_dyads.csv
# output: icon.pdf
#         icon-pre.pdf
#         icon-both.pdf
#         icon-qqplot.pdf
#         icon-residplot.pdf
#
# last mod: Jan/08/2025, NW

# setwd("C:/Users/nwickelmaier/Nextcloud/Documents/teaching/iwm/mixed_models/slides/06_growth_curve")

library(lme4) 
library(lattice)

#--------------- (1) Read data ---------------

dat <- read.csv("../../data/lzw003_supplementary-data/example2_dyads.csv")
dat$dyad <- as.factor(dat$dyad)

#--------------- (2) Visualization of data ---------------

plot(iconicity ~ t, dat, type="n", xlab="Time / Interaction round",
     ylab="Iconicity", main="Growth curve analysis")
for (i in levels(dat$dyad)) {
  lines(iconicity ~ t, dat[dat$dyad == i,])
}
text(0, 10, "(a)")

# using lattice
pdf("../figures/icon.pdf", width=3.375, height=3.375, pointsize=10)
#
xyplot(iconicity ~ t, dat, groups=dyad, type="l", xlab="Time / Interaction round",
     ylab="Iconicity")
#
dev.off()

#--------------- (3) Centering of variables ---------------

dat$t_c <- dat$t - mean(dat$t)

# Show that they are uncorrelated
cor(dat$t_c, dat$t_c^2)  # r = 0

# Compare to un-centered case
cor(dat$t, dat$t^2)  # r = 0.97

#--------------- (4) Fitting models ---------------

gcm1 <- lmer(iconicity ~ t_c + I(t_c^2) +                      # fixed effects
        (1 | dyad) + (0 + t_c | dyad) + (0 + I(t_c^2) | dyad), # random effects
        data=dat, REML=F)

pdf("../figures/icon-pre.pdf", width=3.375, height=3.375, pointsize=10)
#
xyplot(predict(gcm1) ~ t, dat, groups=dyad, type="l", xlab="Time / Interaction round",
     ylab="Iconicity")
#
dev.off()

#--------------- (4.1) Check random effects structure ---------------

# Create model without quadratic random slope, without linear random slope,
# and without both

gcm2 <- lmer(iconicity ~ t_c + I(t_c^2) +     # fixed effects
        (1 | dyad) + (0 + t_c | dyad),        # random effects
        data=dat, REML=F)
gcm3 <- lmer(iconicity ~ t_c + I(t_c^2) +     # fixed effects
        (1 | dyad) + (0 + I(t_c^2) | dyad),   # random effects
        data=dat, REML=F)
gcm4 <- lmer(iconicity ~ t_c + I(t_c^2) +     # fixed effects
        (1 | dyad),                           # random effects
        data=dat, REML=F)

anova(gcm4, gcm2)
# --> the model with the linear slope does not differ from the one without
# slopes; thus the linear slopes are not warranted

anova(gcm4, gcm3)
# --> the model with the quadratic slope does differ from the one without
# slopes; thus the quadratic slopes are warranted

anova(gcm3, gcm1)
# --> the model with both slopes does not differ from the one with only
# quadratic slopes, thus gcm3 is the best model

coef(gcm3) 
# --> Note that "t_c^2" is different for the different dyads and "t_c" is
# the same because we dropped the random slopes for the linear effect

#--------------- (4.2) Test quadratic and linear effects ---------------

gcm5 <- lmer(iconicity ~ 1 +                      # fixed effects
        (1 | dyad) + (0 + I(t_c^2) | dyad),       # random effects
        data=dat, REML=F)
gcm6 <- lmer(iconicity ~ t_c +                    # fixed effects
        (1 | dyad) + (0 + I(t_c^2) | dyad),       # random effects
        data=dat, REML=F)

anova(gcm5, gcm6, gcm3)

#--------------- (4.3) Check model assumptions ---------------

hist(residuals(gcm1))   # good

pdf("../figures/icon-qqplot.pdf", width=4, height=4, pointsize=10)

qqmath(gcm1)            # o.k.

dev.off()

pdf("../figures/icon-residplot.pdf", width=4, height=4, pointsize=10)

plot(gcm1)              # o.k.

dev.off()

#--------------- (4.4) Test higher-order polynomials ---------------

# In this case, visualizing the data obviously revealed that a quadratic
# effect is appropriate; in other cases however, it is not obvious what
# order of polynomials is supported by the data. To demonstrate, we show
# how to choose a polynomial form for the data

poly1 <- lmer(iconicity ~ poly(t,1) + (1 | dyad), data=dat, REML=F)
poly2 <- lmer(iconicity ~ poly(t,2) + (1 | dyad), data=dat, REML=F)
poly3 <- lmer(iconicity ~ poly(t,3) + (1 | dyad), data=dat, REML=F)
poly4 <- lmer(iconicity ~ poly(t,4) + (1 | dyad), data=dat, REML=F)

anova(poly1, poly2, poly3, poly4)
# --> only the quadratic model differs significantly from the linear model
# thus there is no support for polynomials above order 2

#--------------- (4.5) Visualization of model predictions ---------------

#dat$pre <- predict(gcm3)
dat$pre <- predict(gcm1)

pdf("../figures/icon-both.pdf", width=6.5, height=3.375)
par(mfrow=1:2, mai=c(.7,.7,.4,.1), mgp=c(2.4,1,0))

plot(iconicity ~ t, dat, type="n", xlab="Time / Interaction round",
     ylab="Iconicity", main="Raw data")
for (i in levels(dat$dyad)) {
  lines(iconicity ~ t, dat[dat$dyad == i,])
}
text(1.5, 9.5, "(a)")

plot(iconicity ~ t, dat, type="n", xlab="Time / Interaction round",
     ylab="Iconicity", main="Growth curve analysis")
for (i in levels(dat$dyad)) {
  lines(pre ~ t, dat[dat$dyad == i,], col="gray")
}
# add group trend
#lines(unique(predict(gcm3, re.form=~0)) ~ I(1:10), lwd=2)
lines(unique(predict(gcm1, re.form=~0)) ~ I(1:10), lwd=2)
text(1.5, 9.5, "(b)")

dev.off()

# using lattice
xyplot(pre ~ t, dat, groups=dyad, type="l")



