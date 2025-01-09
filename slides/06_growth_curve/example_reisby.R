# example_imipramin.R
#
# Example on growth curve models for Reisby et al. (1977)
#
# content: (1) Read data
#          (2) Linear analysis
#          (3) Fitting quadratic model
#          (3) Centering of variables
#          (4) Check random effects structure
#          (5) Check model assumptions
#
# input:  resiby.dat
# output: hdrs-ind_pred-randomslope.pdf
#         hdrs-ind_pred-quad.pdf
#         hdrs-caterpillar.pdf
#         hdrs_shrinkage_int-week.pdf
#         hdrs_shrinkage_int-weeksq.pdf
#         hdrs_shrinkage_week-weeksq.pdf
#
# last mod: Jan/08/2025, NW

# setwd("C:/Users/nwickelmaier/Nextcloud/Documents/teaching/iwm/mixed_models/slides/06_growth_curve")

library(lme4) 
library(lattice)

#--------------- (1) Read data ------------------------------------------------

dat      <- read.table("../../data/reisby.dat", header = TRUE)
dat$id   <- factor(dat$id)
dat$diag <- factor(dat$diag, levels = c("nonen", "endog"))
dat      <- na.omit(dat)    # drop missing values

#--------------- (2) Linear analysis ------------------------------------------

# random intercept model
lme1 <- lmer(hamd ~ week + (1 | id), dat, REML = FALSE)

# random slope model
lme2 <- lmer(hamd ~ week + (week | id), dat, REML = FALSE)

anova(lme1, lme2)

pdf("../figures/hdrs-ind_pred-randomslope.pdf", height = 6, width = 6)

xyplot(hamd + predict(lme2) ~ week | id, data = dat,
       type = c("p", "l", "g"),
       pch = 16,
       ratio = "xy",
       col = c("#3CB4DC", "#FF6900"),
       distribute.type = TRUE,
       layout = c(11, 6),
       ylab = "HDRS score",
       xlab = "Time (week)")

dev.off()

#--------------- (3) Fitting quadratic model ----------------------------------

# model with quadratic time trend
lme3 <- lmer(hamd ~ week + I(week^2) + (week + I(week^2) | id), dat,
             REML = FALSE)

# Analysis with centered variables
dat$week_c <- dat$week - mean(dat$week)

# random slope model
lme2c <- lmer(hamd ~ week_c + (week_c | id), dat, REML = FALSE)

# model with quadratic time trend
lme3c <- lmer(hamd ~ week_c + I(week_c^2) + (week_c + I(week_c^2) | id), dat,
              REML = FALSE)

pdf("../figures/hdrs-ind_pred-quad.pdf", height = 6, width = 6)

xyplot(hamd + predict(lme3) ~ week | id, data = dat,
       type = c("p", "l", "g"),
       pch = 16,
       ratio = "xy",
       col = c("#3CB4DC", "#FF6900"),
       distribute.type = TRUE,
       layout = c(11, 6),
       ylab = "HDRS score",
       xlab = "Time (week)")

dev.off()


# model without random quadratic time effect
lme3.0 <- lmer(hamd ~ week_c + I(week_c^2) + (week_c | id), dat, REML = FALSE)

# model without fixed quadratic time effect
lme3.1 <- lmer(hamd ~ week_c + (week_c + I(week_c^2) | id), dat, REML = FALSE)

# LRTs
anova(lme3.0, lme3)
anova(lme3.1, lme3)

xyplot(hamd + predict(lme3) ~ week | id, data = dat,
       type = c("p", "l", "g"),
       pch = 16,
       distribute.type = TRUE,
       ylab = "HDRS score",
       xlab = "Time (Week)")

xyplot(hamd + predict(lme3.0) ~ week | id, data = dat,
       type = c("p", "l", "g"),
       pch = 16,
       distribute.type = TRUE,
       ylab = "HDRS score",
       xlab = "Time (Week)")

#--------------- (4) Check random effects structure ---------------

lme3reml <- lmer(hamd ~ week_c + I(week_c^2) + (week_c + I(week_c^2) | id), dat)

pm3 <- profile(lme3reml, which = "theta_")

xyplot(pm3)
densityplot(pm3)
splom(pm3)

lme3noncorr <- lmer(hamd ~ week_c + I(week_c^2) + (week_c + I(week_c^2) || id), dat)

pm3nc <- profile(lme3noncorr)

xyplot(pm3nc)
xyplot(log(pm3nc))
xyplot(varianceProf(pm3nc))

densityplot(pm3nc)
densityplot(log(pm3nc))
densityplot(varianceProf(pm3nc))

splom(pm3nc)
splom(log(pm3nc))
splom(varianceProf(pm3nc))

confint(pm3nc)

pdf("../figures/hdrs-caterpillar.pdf", height = 8, width = 20)

dotplot(ranef(lme3reml), scales = list(x = list(relation = "free")))[[1]]

dev.off()

# Shrinkage plots

df     <- coef(lmList(hamd ~ week_c + I(week_c^2) | id, dat))
cc1    <- as.data.frame(coef(lme3reml)$id)
names(cc1) <- c("A", "B", "C")

df <- cbind(df, cc1)
ff <- fixef(lme3reml)


pdf("../figures/hdrs_shrinkage_int-week.pdf", height = 6, width = 6, pointsize = 10)

with(df,
  xyplot(`(Intercept)` ~ week_c, aspect = 1,
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
    xlab = "week_c",
    ylab = "(Intercept)",
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-subject", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = 16))
  )
)

dev.off()


pdf("../figures/hdrs_shrinkage_int-weeksq.pdf", height = 6, width = 6, pointsize = 10)

with(df,
  xyplot(`(Intercept)` ~ `I(week_c^2)`, aspect = 1,
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
    xlab = expression(week_c^2),
    ylab = "(Intercept)",
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-subject", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = 16))
  )
)

dev.off()


pdf("../figures/hdrs_shrinkage_week-weeksq.pdf", height = 6, width = 6, pointsize = 10)

with(df,
  xyplot(week_c ~ `I(week_c^2)`, aspect = 1,
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
    xlab = expression(week_c^2),
    ylab = "week_c",
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-subject", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = 16))
  )
)

dev.off()

# Shrinkage plots without covariances

df     <- coef(lmList(hamd ~ week_c + I(week_c^2) | id, dat))
cc1    <- as.data.frame(coef(lme3noncorr)$id)
names(cc1) <- c("A", "B", "C")

df <- cbind(df, cc1)
ff <- fixef(lme3noncorr)


pdf("../figures/hdrs_shrinkage_int-week_noncorr.pdf", height = 6, width = 6, pointsize = 10)

with(df,
  xyplot(`(Intercept)` ~ week_c, aspect = 1,
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
    xlab = "week_c",
    ylab = "(Intercept)",
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-subject", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = 16))
  )
)

dev.off()


pdf("../figures/hdrs_shrinkage_int-weeksq_noncorr.pdf", height = 6, width = 6, pointsize = 10)

with(df,
  xyplot(`(Intercept)` ~ `I(week_c^2)`, aspect = 1,
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
    xlab = expression(week_c^2),
    ylab = "(Intercept)",
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-subject", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = 16))
  )
)

dev.off()


pdf("../figures/hdrs_shrinkage_week-weeksq_noncorr.pdf", height = 6, width = 6, pointsize = 10)

with(df,
  xyplot(week_c ~ `I(week_c^2)`, aspect = 1,
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
    xlab = expression(week_c^2),
    ylab = "week_c",
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-subject", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = 16))
  )
)

dev.off()

# LRT for quadratic time effect
lme0.0 <- lmer(hamd ~ week_c + (week_c || id), dat, REML = FALSE)
lme0.1 <- lmer(hamd ~ week_c + I(week_c^2) + (week_c + I(week_c^2) || id), dat, REML = FALSE)

anova(lme0.0, lme0.1)

# Look at profiles
pm0 <- profile(lme0.0)

xyplot(log(pm0))
densityplot(log(pm0))
splom(log(pm0))

confint(pm0)

pm1 <- profile(lme0.1)

xyplot(log(pm1))
densityplot(log(pm1))
splom(log(pm1))

confint(pm1)

# Centering matters, so pick something sensible!
lme6 <- lmer(hamd ~ week_c + (week_c | id), dat, REML = FALSE)
lme7 <- lmer(hamd ~ week + (week | id), dat, REML = FALSE)
lme8 <- lmer(hamd ~ week + (week || id), dat, REML = FALSE)

dotplot(ranef(lme6, condVar = TRUE),
        scales = list(x = list(relation = "free")))[[1]]

dotplot(ranef(lme7, condVar = TRUE),
        scales = list(x = list(relation = "free")))[[1]]
## --> When the intercept represents the baseline, we have a negative
## correlation (or possible none as the confidence interval suggests...)
## --> When the intercept represents the mean, then we already moved down the
## regression line and surprise: the farther we moved down, the steeper the
## slope! But is this an interesting result?

xyplot(hamd + predict(lme7) ~ week | id, data = dat,
       type = c("p", "l", "g"),
       pch = 16,
       distribute.type = TRUE,
       #aspect = "xy",
       index.cond = function(x, y) coef(lm(y ~ x))[1],
       #index.cond = function(x,y) coef(lm(y ~ x)) %*% c(1, 6),
       ylab = "HDRS score",
       xlab = "Time (Week)")

#--------------- (5) Check model assumptions ---------------

hist(residuals(lme6))   # good
qqmath(lme6)            # o.k.
plot(lme6)              # o.k.

