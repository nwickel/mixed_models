library(lme4)
library(lattice)

load("data/healing.RData")

dat <- DFmodel
str(dat)

dat$Subject <- factor(dat$Subject)
dat$ResponseId <- factor(dat$ResponseId)

# Visualize data

xyplot(Healing ~ Condition | Subject, dat, groups = ResponseId, type = "l")

vioplot::vioplot(Healing ~ Condition, dat, col = c("#3CB4DC", "#91C86E", "#FF6900"))
stripchart(Healing ~ Condition, dat,
           vertical = TRUE,
           pch = 21,
           add = TRUE,
           col = c("#FF6900", "#78004B", "#3CB4DC"),
           method = "jitter")

# Plot for mean healing for each participant

datm <- aggregate(Healing ~ Condition + Subject, dat, mean)

pdf("slides/figures/heal_vioplot.pdf", width = 3.375, height = 3.375, pointsize = 10)
par(mai = c(.6, .6, .1, .1), mgp = c(2.4, 1, 0))

vioplot::vioplot(Healing ~ Condition, datm, col = c("#3CB4DC", "#91C86E", "#FF6900"))
stripchart(Healing ~ Condition, datm,
           vertical = TRUE,
           pch = 21,
           add = TRUE,
           col = c("#FF6900", "#78004B", "#3CB4DC"),
           method = "jitter")

dev.off()

pdf("slides/figures/heal_subjects.pdf", width = 8, height = 4.5)

xyplot(Healing ~ Condition | Subject, datm, type = "b", layout = c(11, 3), ratio = "xy")

dev.off()

# Plot for mean healing for each rater

datm <- aggregate(Healing ~ Condition + ResponseId, dat, mean)

pdf("slides/figures/heal_raters.pdf", width = 5.5, height = 5.5)

xyplot(Healing ~ Condition | ResponseId, datm, type = "b", layout = c(5, 5))

dev.off()

# Fit model

m1 <- lmer(Healing ~ Condition + (1 | Subject) + (1 | ResponseId), dat)
m2 <- lmer(Healing ~ Condition + (Condition | Subject) + (1 | ResponseId), dat)
m3 <- lmer(Healing ~ Condition + (1 | Subject) +
           (0 + dummy(Condition, "28") | Subject) +
           (0 + dummy(Condition, "56") | Subject) +
           (1 | ResponseId), dat)

pm1 <- profile(m1)
pm2 <- profile(m2)
pm3 <- profile(m3)

ci1 <- confint(pm1)
ci2 <- confint(pm2)
ci3 <- confint(pm3)

xy1 <- ci1[c("(Intercept)", "Condition28", "Condition56"), ]
xy2 <- ci2[c("(Intercept)", "Condition28", "Condition56"), ]
xy3 <- ci3[c("(Intercept)", "Condition28", "Condition56"), ]

dp <- data.frame(par = c("(Intercept)", "Condition28", "Condition56"),
                 model = rep(c("m1", "m2", "m3"), each = 3),
                 ci = c(xy1[, 1], xy2[, 1], xy3[, 1], xy1[, 2], xy2[, 2], xy3[, 2])
)

dotplot(par ~ ci, dp[dp$model != "m3",], groups = model, auto.key = T, xlab = "")

pdf("slides/figures/heal_ci.pdf", width = 3.375, height = 4.5)

dotplot(model ~ ci | par, dp, groups = model, xlab = "", layout = c(1, 3), pch = 16)

dev.off()

