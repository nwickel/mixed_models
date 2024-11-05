# plots.R
#
# input:  --
# output: oddslogit.pdf
#
# last mod: Nov/05/2024, NW

# setwd("C:/Users/nwickelmaier/Nextcloud/Documents/teaching/iwm/mixed_models/slides/02_glm/")

library(ggplot2)
library(ggExtra)

# Logit vs Odds

prob <- seq(0, .99, .01)

odds <- prob / (1 - prob)

logit <- log(odds)


pdf("../figures/oddslogit.pdf", width = 6.75, height = 3.375, pointsize = 10)
par(mfrow = c(1, 2), mai = c(.6, .6, .1, .1), mgp = c(2.4, 1, 0))

plot(odds ~ prob, type = "l", col = "#FF6900", ylab = "Odds", xlab = "Probability")
abline(h = 0, v = 0.5, lty = 2, col = "#3CB4DC")
plot(logit ~ prob, type = "l", col = "#FF6900", ylab = "Logit", xlab = "Probability")
abline(h = 0, v = 0.5, lty = 2, col = "#3CB4DC")

dev.off()


# Visualization of predictions from poisson regression

# Read data
dat <- read.csv("../../data/poisson_sim.csv")

# Define factors
dat$prog <- factor(dat$prog, levels = 1:3,
                   labels = c("General", "Academic", "Vocational"))
dat$id <- factor(dat$id)

# Fit poisson regression
m1 <- glm(num_awards ~ prog + math, family = poisson, data = dat)
summary(m1)

# Evaluate goodness-of-fit
1 - pchisq(m1$deviance, df = m1$df.residual)

# Predictions

newdat <- data.frame(
    prog = rep(c("General", "Academic", "Vocational"), table(dat$pro)),
    math = c(seq(30, 80, length.out = table(dat$pro)[1]),
             seq(30, 80, length.out = table(dat$pro)[2]),
             seq(30, 80, length.out = table(dat$pro)[3])))

newdat$phat <- predict(m1, newdata = newdat, type = "response")


pdf("../figures/pois_pre.pdf", width = 3.375, height = 3.375, pointsize = 10)
par(mai = c(.6, .6, .1, .1), mgp = c(2.4, 1, 0))

plot(num_awards ~ math, dat, type = "n", ylim = c(-.5, 6.1),
  xlab = "Math grade", ylab = "Number of awards")

points(jitter(num_awards, 1) ~ jitter(math, 1),
  dat[dat$prog == "General",], pch = 16, col = "#91C86E")
points(jitter(num_awards, 1) ~ jitter(math, 1),
  dat[dat$prog == "Academic",], pch = 16, col = "#78004B")
points(jitter(num_awards, 1) ~ jitter(math, 1),
  dat[dat$prog == "Vocational",], pch = 16, col = "#3CB4DC")
lines(phat ~ math, newdat[newdat$prog == "General",], col = "#91C86E")
lines(phat ~ math, newdat[newdat$prog == "Academic",], col = "#78004B")
lines(phat ~ math, newdat[newdat$prog == "Vocational",], col = "#3CB4DC")

legend("topleft", c("General", "Academic", "Vocational"),
  col = c("#91C86E", "#78004B", "#3CB4DC"), lty = 1, pch = 16, bty = "n")

dev.off()

# Plot for introduction

set.seed(1344)

x <- seq(-4, 4, length.out = 500)
y <- 10 + x + rnorm(length(x), sd = 2)

plot(y ~ x)

qqnorm(y)
qqline(y)
plot(density(y))

p <- ggplot() +
       geom_point(aes(x = x, y = y), color = "#78004B") +
       theme_bw()

pdf("../figures/glm_lin.pdf", width = 4.5, height = 3.375, pointsize = 10)

ggMarginal(p, margins = "y", color = "#3CB4DC", fill = "#3CB4DC", size = 3, type = "histogram")

dev.off()

x <- seq(-4, 4, length.out = 50)

y <- c(rbinom(length(x) / 2, size = 1, prob = .2),
       rbinom(length(x) / 2, size = 1, prob = .8))

plot(y ~ x)

p <- ggplot() +
       geom_point(aes(x = x, y = y), color = "#78004B") +
       theme_bw()

pdf("../figures/glm_bin.pdf", width = 4.5, height = 3.375, pointsize = 10)

ggMarginal(p, margins = "y", color = "#3CB4DC", fill = "#3CB4DC", size = 3, type = "histogram")

dev.off()

