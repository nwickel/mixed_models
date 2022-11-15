# example_bates.R
#
# content: (1) Read data
#          (2) Visualize
#          (3) Fit models
#
# input: --
# output: Plots
#
# created: Oct/12/2018, NU
# last mod: May/25/2021, NU

setwd("C:/Users/numbach/Nextcloud/Documents/teaching/regression/04_longitudinal/figures")

library(lattice)
library(lme4)

#--------------- (1) Load data ---------------

data(sleepstudy)
#?sleepstudy
str(sleepstudy)
summary(sleepstudy)
head(sleepstudy)

xtabs( ~ Subject + Days, sleepstudy)

#--------------- (2) Visualize data ---------------

pdf("sleep_subjects.pdf", height=8, width=8, pointsize=10)

xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
  layout = c(6,3), type = c("g", "b"),
  #index.cond = function(x,y) coef(lm(y ~ x))[1],
  xlab = "Days of sleep deprivation",
  ylab = "Average reaction time (ms)"
)

dev.off()

# random intercept model
lme0 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy, REML=FALSE)

pdf("sleep_random_intercept.pdf", height=8, width=8, pointsize=10)

xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
  layout = c(6,3), type = c("g", "p"),
  coef.list = coef(lme0)$Subject,
  panel = function(..., coef.list) {
    panel.xyplot(...)
    panel.abline(as.numeric(coef.list[packet.number(),]),
                 col.line = trellis.par.get("superpose.line")$col[1],
                 lty = trellis.par.get("superpose.line")$lty[2]
                 )
  },
  index.cond = function(x,y) coef(lm(y ~ x))[1],
  xlab = "Days of sleep deprivation",
  ylab = "Average reaction time (ms)"
)

dev.off()

# random slope model
lme1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML=FALSE)

pdf("sleep_random_slope.pdf", height=8, width=8, pointsize=10)

xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
  layout = c(6,3), type = c("g", "p"),
  coef.list = coef(lme1)$Subject,
  panel = function(..., coef.list) {
    panel.xyplot(...)
    panel.abline(as.numeric(coef.list[packet.number(),]),
                 col.line = trellis.par.get("superpose.line")$col[1],
                 lty = trellis.par.get("superpose.line")$lty[2]
                 )
  },
  index.cond = function(x,y) coef(lm(y ~ x))[1],
  xlab = "Days of sleep deprivation",
  ylab = "Average reaction time (ms)"
)

dev.off()

pdf("sleep_lattice.pdf", height=8, width=8, pointsize=10)

xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
  index.cond = function(x,y) coef(lm(y ~ x))[1],
  xlab = "Days of sleep deprivation",
  ylab = "Average reaction time (ms)", aspect = "xy")

dev.off()

pdf("sleep_box.pdf", height=3.375, width=3.375, pointsize=10)
par(mai=c(.6,.6,.1,.1), mgp=c(2.4,1,0))

boxplot(Reaction ~ Days, sleepstudy)
# --> good to see that assumption of homoscedasticity is violated

dev.off()

plot(aggregate(Reaction ~ Days, sleepstudy, sd)[, 2], type="h")

#--------------- (3) Fit models ---------------

lme2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy, REML=FALSE)

anova(lme1, lme2)

confint(lme2)

plot(lme2)

qqnorm(resid(lme2))
qqline(resid(lme2))

hist(resid(lme2), border="white", col="gray", main="")

#--------------- (4) Examine random effects ---------------

# caterpillar plot

pdf("sleep_caterpillar.pdf", height=4.5, width=8, pointsize=10)

dotplot(ranef(lme2, condVar=TRUE), scales = list(x = list(relation =
                                                          "free")))[[1]]

dev.off()

# shrinkage
df <- coef(lmList(Reaction ~ Days | Subject, sleepstudy))
fclow <- subset(df, `(Intercept)` < 251)
fchigh <- subset(df, `(Intercept)` > 251)
cc1 <- as.data.frame(coef(lme2)$Subject)
names(cc1) <- c("A", "B")
df <- cbind(df, cc1)
ff <- fixef(lme2)

pdf("sleep_shrinkage.pdf", height=6, width=6, pointsize=10)

with(df,
  xyplot(`(Intercept)` ~ Days, aspect = 1,
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
        ltext(fclow[,2], fclow[,1], row.names(fclow),
              adj = c(0.5, 1.7))
        ltext(fchigh[,2], fchigh[,1], row.names(fchigh),
              adj = c(0.5, -0.6))
    },
    key = list(space = "top", columns = 3,
    text = list(c("Mixed model", "Within-group", "Population")),
    points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
    pch = trellis.par.get("superpose.symbol")$pch[1:3]))
  )
)

dev.off()

# shrinkfit
pdf("sleep_shrinkfit.pdf", height=6, width=6, pointsize=10)

xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
  layout = c(6,3), type = c("g", "p", "r"),
  coef.list = df[,3:4],
  panel = function(..., coef.list) {
    panel.xyplot(...)
    panel.abline(as.numeric(coef.list[packet.number(),]),
                 col.line = trellis.par.get("superpose.line")$col[2],
                 lty = trellis.par.get("superpose.line")$lty[2]
                 )
    panel.abline(fixef(lme2),
                 col.line = trellis.par.get("superpose.line")$col[4],
                 lty = trellis.par.get("superpose.line")$lty[4]
                 )
  },
  index.cond = function(x,y) coef(lm(y ~ x))[1],
  xlab = "Days of sleep deprivation",
  ylab = "Average reaction time (ms)",
  key = list(space = "top", columns = 3,
  text = list(c("Within-subject", "Mixed model", "Population")),
  lines = list(col = trellis.par.get("superpose.line")$col[c(1:2,4)],
  lty = trellis.par.get("superpose.line")$lty[c(1:2,4)]))
)

dev.off()

