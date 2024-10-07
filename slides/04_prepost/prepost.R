# setwd("C:/Users/nwickelmaier/Nextcloud/Documents/teaching/iwm/mixed_models/slides/04_prepost/")

library(lme4)
library(lmerTest)

# Read data
dat <- read.table("kleinhenz.txt" , header = TRUE)
dat$grp <- factor(dat$grp, levels = c("plac" , "acu" ))

# Change score analysis (beta1 = 1)
m0 <- lm(post - pre ~ grp, dat)
m1 <- lm(post ~ offset(pre) + grp, dat)

# Reshape data frame
datl <- reshape(dat, direction = "long", varying = list(1:2), v.names = "score")
datl$time <- factor(datl$time, levels = 1:2, labels = c("pre" , "post" ))

# Repeated-measures ANOVA
m2 <- lmer(score ~ grp*time + (1 | id), datl)

# Compare residual variances
sigma(m1)^2
2 * sigma(m2)^2

# ANCOVA (beta1 frei setzen)
m3 <- lm(post ~ pre + grp, dat)
anova(m1, m3)

m4 <- lm(post - pre ~ pre + grp, dat)

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
abline(h = mean(dat$post), col="red")

