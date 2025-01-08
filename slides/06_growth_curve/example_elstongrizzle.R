# Example taken from:
# https://embraceuncertaintybook.com/longitudinal.html#the-elstongrizzle-data

library(lme4)
library(lattice)

# Read and visualize data

dat <- read.table("../../data/elstongrizzle.dat", header = TRUE)
dat$subject <- factor(dat$subject)

pdf("../figures/eg_subjects.pdf", height = 6, width = 6, pointsize = 10)

xyplot(ramusht ~ age, dat, groups = subject,
       type = c("b", "g"),
       xlab = "Age (yr)",
       ylab = "Ramus bone length (mm)")

dev.off()

pdf("../figures/eg_subjects_panel.pdf", height = 6, width = 6, pointsize = 10)

xyplot(ramusht ~ age | subject, dat,
       type = c("g", "p", "r"),
       aspect = "xy",
       index.cond = function(x,y) coef(lm(y ~ x)) %*% c(1, 8),
       xlab = "Age (yr)",
       ylab = "Ramus bone length (mm)")

dev.off()

# Anything quadratic?
pdf("../figures/eg_subjects_panel_spline.pdf", height = 6, width = 6, pointsize = 10)

xyplot(ramusht ~ age | subject, dat,
       type = c("g", "p", "spline"),
       aspect = "xy",
       index.cond = function(x,y) coef(lm(y ~ x)) %*% c(1, 8),
       xlab = "Age (yr)",
       ylab = "Ramus bone length (mm)")

dev.off()


# Random slope model

m1 <- lmer(ramusht ~ age + (age | subject), dat)
summary(m1)

# Although it seems that there isn’t a strong correlation between initial bone
# length and growth rate in these data, a model with an overall linear trend and
# possibly correlated random effects for intercept and slope by subject
# estimates a strong negative correlation (-0.97) between these random effects.
# 
# The reason for this seemingly unlikely result is that the (Intercept) term in
# the fixed effects and the random effects represents the bone length at age 0,
# which is not of interest here. Notice that the fixed-effects (Intercept)
# estimate is about 33.5 mm, which is far below the observed range of the data
# (45.0 to 55.5 mm.)
# 
# Extrapolation from the observed range of ages, 8 years to 9.5 years, back to 0
# years, will almost inevitably result in a negative correlation between slope
# and intercept.

# Correlational structure for intercepts and slopes
pdf("../figures/eg_caterpillar.pdf", height = 4.5, width = 8, pointsize = 10)

dotplot(ranef(m1, condVar = TRUE),
        scales = list(x = list(relation = "free")))[[1]]

dev.off()

# Centering the time variable (at a value of interest)

dat$time <- dat$age - 8

m2 <- lmer(ramusht ~ time + (time | subject), dat)
summary(m2)

dotplot(ranef(m2, condVar = TRUE),
        scales = list(x = list(relation = "free")))[[1]]

## centering at mean age
dat$time_c <- dat$age - mean(dat$age)

m3 <- lmer(ramusht ~ time_c + (time_c | subject), dat)
summary(m3)

lattice::dotplot(ranef(m3, condVar = TRUE),
                 scales = list(x = list(relation = "free")))[[1]]

## investigating random effects

confint(m2)

m4 <- lmer(ramusht ~ time + (time || subject), dat)
summary(m4)

dotplot(ranef(m4, condVar = TRUE),
        scales = list(x = list(relation = "free")))[[1]]

## fitting a quadratic trend

m5 <- lmer(ramusht ~ time_c + I(time_c^2) + (time_c + I(time_c^2) || subject), dat)
summary(m5)

dotplot(ranef(m5, condVar = TRUE),
        scales = list(x = list(relation = "free")))[[1]]

# Shrinkage plots

df     <- coef(lmList(ramusht ~ time | subject, dat))
fclow  <- subset(df, `(Intercept)` < 50)
fchigh <- subset(df, `(Intercept)` > 50)
cc1    <- as.data.frame(coef(m2)$subject)
names(cc1) <- c("A", "B")

df <- cbind(df, cc1)
ff <- fixef(m2)

pdf("../figures/eg_shrinkage.pdf", height = 6, width = 6, pointsize = 10)

with(df,
  xyplot(`(Intercept)` ~ time, aspect = 1,
    x1 = B, y1 = A,
    panel = function(x, y, x1, y1, subscripts, ...) {
        panel.grid(h = -1, v = -1)
        x1 <- x1[subscripts]
        y1 <- y1[subscripts]
        larrows(x, y, x1, y1, type = "closed", length = 0.1,
                angle = 15, ...)
        lpoints(x, y,
                pch = trellis.par.get("superpose.symbol")$pch[2],
                col = trellis.par.get("superpose.symbol")$col[2])
        lpoints(x1, y1,
                pch = trellis.par.get("superpose.symbol")$pch[1],
                col = trellis.par.get("superpose.symbol")$col[1])
        lpoints(ff[2], ff[1], 
                pch = trellis.par.get("superpose.symbol")$pch[3],
                col = trellis.par.get("superpose.symbol")$col[3])
        ltext(fclow[,2], fclow[,1], row.names(fclow), adj = c(0.5, 1.7))
        ltext(fchigh[,2], fchigh[,1], row.names(fchigh), adj = c(0.5, -0.6))
    },
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-subject", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = trellis.par.get("superpose.symbol")$pch[1:3]))
  )
)

dev.off()



df     <- coef(lmList(ramusht ~ time_c + I(time_c^2) | subject, dat))
cc1    <- as.data.frame(coef(m5)$subject)
names(cc1) <- c("A", "B", "C")

df <- cbind(df, cc1)
ff <- fixef(m5)

pdf("../figures/eg_shrinkage_int-time.pdf", height = 6, width = 6, pointsize = 10)

with(df,
  xyplot(`(Intercept)` ~ time_c, aspect = 1,
    x1 = B, y1 = A,
    panel = function(x, y, x1, y1, subscripts, ...) {
        panel.grid(h = -1, v = -1)
        x1 <- x1[subscripts]
        y1 <- y1[subscripts]
        larrows(x, y, x1, y1, type = "closed", length = 0.1, fill = "black",
                angle = 15, ...)
        lpoints(x, y,
                pch = 16,
                col = trellis.par.get("superpose.symbol")$col[2])
        lpoints(x1, y1,
                pch = 16,
                col = trellis.par.get("superpose.symbol")$col[1])
        lpoints(ff[2], ff[1], 
                pch = 16,
                col = trellis.par.get("superpose.symbol")$col[3])
    },
    xlab = "time_c",
    ylab = "(Intercept)",
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-subject", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = 16))
  )
)

dev.off()


pdf("../figures/eg_shrinkage_int-timesq.pdf", height = 6, width = 6, pointsize = 10)

with(df,
  xyplot(`(Intercept)` ~ `I(time_c^2)`, aspect = 1,
    x1 = C, y1 = A,
    panel = function(x, y, x1, y1, subscripts, ...) {
        panel.grid(h = -1, v = -1)
        x1 <- x1[subscripts]
        y1 <- y1[subscripts]
        larrows(x, y, x1, y1, type = "closed", length = 0.1, fill = "black",
                angle = 15, ...)
        lpoints(x, y,
                pch = 16,
                col = trellis.par.get("superpose.symbol")$col[2])
        lpoints(x1, y1,
                pch = 16,
                col = trellis.par.get("superpose.symbol")$col[1])
        lpoints(ff[3], ff[1], 
                pch = 16,
                col = trellis.par.get("superpose.symbol")$col[3])
    },
    xlab = expression(time_c^2),
    ylab = "(Intercept)",
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-subject", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = 16))
  )
)

dev.off()


pdf("../figures/eg_shrinkage_time-timesq.pdf", height = 6, width = 6, pointsize = 10)

with(df,
  xyplot(time_c ~ `I(time_c^2)`, aspect = 1,
    x1 = C, y1 = B,
    panel = function(x, y, x1, y1, subscripts, ...) {
        panel.grid(h = -1, v = -1)
        x1 <- x1[subscripts]
        y1 <- y1[subscripts]
        larrows(x, y, x1, y1, type = "closed", length = 0.1, fill = "black",
                angle = 15, ...)
        lpoints(x, y,
                pch = 16,
                col = trellis.par.get("superpose.symbol")$col[2])
        lpoints(x1, y1,
                pch = 16,
                col = trellis.par.get("superpose.symbol")$col[1])
        lpoints(ff[3], ff[2], 
                pch = 16,
                col = trellis.par.get("superpose.symbol")$col[3])
    },
    xlab = expression(time_c^2),
    ylab = "time_c",
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-subject", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = 16))
  )
)

dev.off()

