library(lme4)

dat <- read.table("../../data/bock.txt", T)
dat$id <- factor(dat$id)

## Deskriptive Statistik (Tabelle 6.1)
tapply(dat$wpss, list(dat$order, dat$week), mean)
tapply(dat$wpss, list(dat$order, dat$week), sd)
#tapply(matrix(dat$wpss, 75, 6, T), sd)  ???
cor(matrix(dat$wpss, nrow = 75, ncol = 6, byrow = TRUE))

xyplot(jitter(wpss) ~ week | id, dat, groups = order)

m1 <- lmer(wpss ~ weekc * order + (weekc | id), dat)
summary(m1)


xyplot(wpss + predict(m1) ~ week | id, data = dat,
       type = c("p", "l", "g"),
       pch = 16,
       distribute.type = TRUE,
       ylab = "WPSS score",
       xlab = "Time (Week)")


m2 <- lmer(wpss ~ weekc * order + I(weekc^2) * order + (weekc | id), dat)
summary(m2)

m3 <- lmer(wpss ~ weekc * order + I(weekc^2) * order + (weekc + I(weekc^2) | id), dat)
summary(m3)


xyplot(wpss + predict(m3) ~ week | id, data = dat,
       type = c("p", "l", "g"),
       pch = 16,
       distribute.type = TRUE,
       ylab = "WPSS score",
       xlab = "Time (Week)")

