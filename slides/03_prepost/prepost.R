# prepost.R
#
# content: (1) Kleinhenz analysis
#          (2) Plots
#
# input:  ../../data/kleinhenz.txt
# output: ../figures/acu-anco.pdf
#         ../figures/acu-chng.pdf
#         ../figures/acu-fwup.pdf
#
# last mod: Oct/16/2024, NW

# setwd("C:/Users/nwickelmaier/Nextcloud/Documents/teaching/iwm/mixed_models/slides/03_prepost/")

library(lme4)
library(lmerTest)

#--------------- (1) Kleinhenz analysis ---------------------------------------

# Read data
dat <- read.table("../../data/kleinhenz.txt" , header = TRUE)
dat$grp <- factor(dat$grp, levels = c("plac" , "acu" ))

# Change score analysis (beta1 = 1)
m0 <- lm(post - pre ~ grp, data = dat)
m1 <- lm(post ~ offset(pre) + grp, data = dat)

# Reshape data frame
datl <- reshape(dat, direction = "long", varying = list(1:2), v.names = "score")
datl$time <- factor(datl$time, levels = 1:2, labels = c("pre" , "post" ))

# LMM
m2 <- lmer(score ~ grp * time + (1 | id), data = datl)

# Compare residual variances
sigma(m1)^2
2 * sigma(m2)^2

# ANCOVA (beta1 frei setzen)
m3 <- lm(post ~ pre + grp, data = dat)
anova(m1, m3)

predict(m3, newdata = data.frame(pre = mean(dat$pre), grp = c("plac", "acu")))

m4 <- lm(post - pre ~ pre + grp, data = dat)

## Plots

plot(post ~ pre, dat[dat$grp == "plac", ], xlim = c(30, 95), ylim = c(30, 95))
points(post ~ pre, dat[dat$grp == "acu", ], pch = 16)

# Predictions change score
abline(coef(m1)[1], 1, lty=2)
abline(coef(m1)[1] + coef(m1)[2], 1)

# Predictions ANCOVA
abline(coef(m3)[1], coef(m3)[2], lty = 2, col = "blue")
abline(coef(m3)[1] + coef(m3)[3], coef(m3)[2], col = "blue")

# Mean post value
abline(h = mean(dat$post), col = "red")

#--------------- (2) Plots ----------------------------------------------------

lm0 <- lm(post ~ pre, dat)
lm1 <- lm(post ~ pre + grp, dat)          # ANCOVA
lm2 <- lm(post ~ pre * grp, dat)
lm3 <- lm(post ~ offset(pre) + grp, dat)  # Change Score Model
lm4 <- lm(post ~ grp, dat)                # t test post score

omean <- tapply(dat$post, dat$grp, mean)
pmean1 <- predict(lm1, data.frame(pre = mean(dat$pre), grp = c("plac", "acu")))
pmean3 <- predict(lm3, data.frame(pre = mean(dat$pre), grp = c("plac", "acu")))

pdf("../figures/acu-fwup.pdf", height = 3.375, width = 3.375, pointsize = 10)

par(mai = c(.5,.5,.1,.1), mgp = c(2,.7,0))
plot(post ~ pre, dat, type = "n", xlim = c(20,100), ylim = c(20,100),
  xlab = "Pre-treatment score", ylab = "Post-treatment score")
abline(v = tapply(dat$pre, dat$grp, mean), col = "lightgrey")
xval <- 1:110
lines(predict(lm4, data.frame(pre = xval, grp = "plac")) ~ xval, lty = 2)
lines(predict(lm4, data.frame(pre = xval, grp = "acu"))  ~ xval, col = "#3CB4DC")
points(post ~ pre, dat[dat$grp == "plac",], pch = 21, bg = "white")
points(post ~ pre, dat[dat$grp == "acu",], pch = 8, col = "#3CB4DC")
points(omean ~ tapply(pre, grp, mean), dat, pch = 16, col = c("black","#3CB4DC"))
legend("bottomright", c("Acupuncture", "Placebo"), pch = c(8,1), lty = 1:2,
  col = c("#3CB4DC", "black"), bty = "n")

dev.off()

pdf("../figures/acu-chng.pdf", height = 3.375, width = 3.375, pointsize = 10)

par(mai = c(.5,.5,.1,.1), mgp = c(2,.7,0))
plot(post ~ pre, dat, type = "n", xlim = c(20,100), ylim = c(20,100),
  xlab = "Pre-treatment score", ylab = "Post-treatment score")
abline(h = omean, v = c(mean(dat$pre), tapply(dat$pre, dat$grp, mean)),
  col = "lightgrey")
lines(predict(lm3, data.frame(pre = xval, grp = "plac")) ~ xval, lty = 2)
lines(predict(lm3, data.frame(pre = xval, grp = "acu"))  ~ xval, col = "#3CB4DC")
points(post ~ pre, dat[dat$grp == "plac",], pch = 21, bg = "white")
points(post ~ pre, dat[dat$grp == "acu",], pch = 8, col = "#3CB4DC")
points(omean ~ tapply(pre, grp, mean), dat, pch = 16, col = c("black","#3CB4DC"))
points(pmean3 ~ rep(mean(pre), 2), dat, pch = 24, bg = "white",
  col = c("black","#3CB4DC"))
legend("bottomright", c("Acupuncture", "Placebo"), pch = c(8,1), lty = 1:2,
  col = c("#3CB4DC", "black"), bty = "n")

dev.off()

pdf("../figures/acu-anco.pdf", height = 3.375, width = 3.375, pointsize = 10)

par(mai = c(.5,.5,.1,.1), mgp = c(2,.7,0))
plot(post ~ pre, dat, type = "n", xlim = c(20,100), ylim = c(20,100),
  xlab = "Pre-treatment score", ylab = "Post-treatment score")
abline(h = omean, v = c(mean(dat$pre), tapply(dat$pre, dat$grp, mean)), col = "lightgrey")
lines(predict(lm1, data.frame(pre = xval, grp = "plac")) ~ xval, lty = 2)
lines(predict(lm1, data.frame(pre = xval, grp = "acu"))  ~ xval, col = "#3CB4DC")
points(post ~ pre, dat[dat$grp == "plac",], pch = 21, bg = "white")
points(post ~ pre, dat[dat$grp == "acu",], pch = 8, col = "#3CB4DC")
points(omean ~ tapply(pre, grp, mean), dat, pch = 16, col = c("black","#3CB4DC"))
points(pmean1 ~ rep(mean(pre), 2), dat, pch = 24, bg = "white",
  col = c("black","#3CB4DC"))
legend("bottomright", c("Acupuncture", "Placebo"), pch = c(8,1), lty = 1:2,
  col = c("#3CB4DC", "black"), bty = "n")

dev.off()


