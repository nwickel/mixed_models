# example_bates.R
#
# content: (1) Load data
#          (2) Visualize data
#          (3) Fit models
#          (4) Examine random effects
#
# input: --
# output: sleep_subjects.pdf
#         sleep_random_intercept.pdf
#         sleep_random_slope.pdf
#         sleep_lattice.pdf
#         sleep_box.pdf
#         sleep_caterpillar.pdf
#         sleep_shrinkage.pdf
#         sleep_shrinkfit.pdf
#
# last mod: Oct/16/2024, NW

# setwd("C:/Users/nwickelmaier/Nextcloud/Documents/teaching/iwm/mixed_models/slides/04_longitudinal/")

library(lattice)
library(lme4)

#--------------- (1) Load data ------------------------------------------------

data(sleepstudy)
#?sleepstudy
str(sleepstudy)
summary(sleepstudy)
head(sleepstudy)

xtabs( ~ Subject + Days, sleepstudy)

#--------------- (2) Visualize data -------------------------------------------

pdf("../figures/sleep_subjects.pdf", height = 6, width = 6, pointsize = 10)

xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
       layout = c(6,3), type = c("g", "b"),
       col = "#3CB4DC",
       #index.cond = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)"
)

dev.off()

# random intercept model
lme0 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy, REML = FALSE)

pdf("../figures/sleep_random_intercept.pdf", height = 6, width = 6, pointsize = 10)

xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
       layout = c(6,3), type = c("g", "p"),
       col = "#3CB4DC",
       coef.list = coef(lme0)$Subject,
       panel = function(..., coef.list) {
         panel.xyplot(...)
         panel.abline(as.numeric(coef.list[packet.number(),]),
                      col.line = "#3CB4DC",
                      lty = trellis.par.get("superpose.line")$lty[2]
                      )
       },
       index.cond = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)"
)

dev.off()

# random slope model
lme1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML = FALSE)

pdf("../figures/sleep_random_slope.pdf", height = 6, width = 6, pointsize = 10)

xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
       layout = c(6,3), type = c("g", "p"),
       col = "#3CB4DC",
       coef.list = coef(lme1)$Subject,
       panel = function(..., coef.list) {
         panel.xyplot(...)
         panel.abline(as.numeric(coef.list[packet.number(),]),
                      col.line = "#3CB4DC",
                      lty = trellis.par.get("superpose.line")$lty[2]
                      )
       },
       index.cond = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)"
)

dev.off()

pdf("../figures/sleep_lattice.pdf", height = 8, width = 8, pointsize = 10)

xyplot(Reaction ~ Days | Subject, sleepstudy, type = c("g","p","r"),
       index.cond = function(x,y) coef(lm(y ~ x))[1],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)",
       aspect = "xy")

dev.off()

pdf("../figures/sleep_box.pdf", height = 3.375, width = 3.375, pointsize = 10)

par(mai = c(.6,.6,.1,.1), mgp = c(2.4,1,0))
boxplot(Reaction ~ Days, sleepstudy, col = "#3CB4DC")
# --> good to see that assumption of homoscedasticity is violated

dev.off()

plot(aggregate(Reaction ~ Days, sleepstudy, sd)[, 2], type = "h")

#--------------- (3) Fit models -----------------------------------------------

lme2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy, REML = FALSE)

anova(lme1, lme2)

confint(lme2)

plot(lme2, col = "#3CB4DC")

qqnorm(resid(lme2))
qqline(resid(lme2))

hist(resid(lme2), border = "white", col = "gray", main = "")

#--------------- (4) Examine random effects -----------------------------------

# caterpillar plot

pdf("../figures/sleep_caterpillar.pdf", height = 4.5, width = 8, pointsize = 10)

dotplot(ranef(lme2, condVar = TRUE),
        col = "#FF6900",
        scales = list(x = list(relation = "free")))[[1]]

dev.off()

# shrinkage
df      <- coef(lmList(Reaction ~ Days | Subject, sleepstudy))
fclow   <- subset(df, `(Intercept)` < 251)
fchigh  <- subset(df, `(Intercept)` > 251)
cc1     <- as.data.frame(coef(lme2)$Subject)
names(cc1) <- c("A", "B")

df <- cbind(df, cc1)
ff <- fixef(lme2)

pdf("../figures/sleep_shrinkage.pdf", height = 6, width = 6, pointsize = 10)

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
        ltext(fclow[,2], fclow[,1], row.names(fclow), adj = c(0.5, 1.7))
        ltext(fchigh[,2], fchigh[,1], row.names(fchigh), adj = c(0.5, -0.6))
    },
    key = list(space = "top", columns = 3,
               text = list(c("Mixed model", "Within-group", "Population")),
               points = list(col = trellis.par.get("superpose.symbol")$col[1:3],
                             pch = trellis.par.get("superpose.symbol")$pch[1:3]))
  )
)

dev.off()

# shrinkfit
pdf("../figures/sleep_shrinkfit.pdf", height = 6, width = 6, pointsize = 10)

xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
  layout = c(6,3), type = c("g", "p", "r"),
  col = "#3CB4DC",
  coef.list = df[,3:4],
  panel = function(..., coef.list) {
    panel.xyplot(...)
    panel.abline(as.numeric(coef.list[packet.number(),]),
                 col.line = "#FF6900",
                 lty = trellis.par.get("superpose.line")$lty[2]
                 )
    panel.abline(fixef(lme2),
                 col.line = "#78004B",
                 lty = trellis.par.get("superpose.line")$lty[4]
                 )
  },
  index.cond = function(x, y) coef(lm(y ~ x))[1],
  xlab = "Days of sleep deprivation",
  ylab = "Average reaction time (ms)",
  key = list(space = "top", columns = 3,
             text = list(c("Within-subject", "Mixed model", "Population")),
             lines = list(col = c("#3CB4DC", "#FF6900", "#78004B"),
                          lty = trellis.par.get("superpose.line")$lty[c(1:2,4)]))
)

dev.off()

# assumption plots

pdf("../figures/assump_resid_slope.pdf", height = 6, width = 6, pointsize = 10)

plot(lme2, col = sleepstudy$Subject, pch = sleepstudy$Days, main = "Random slope")

dev.off()

pdf("../figures/assump_resid_intercept.pdf", height = 6, width = 6, pointsize = 10)

plot(lme0, col = sleepstudy$Subject, pch = sleepstudy$Days, main = "Random intercept")

dev.off()


pdf("../figures/assump_qqplot_slope.pdf", height = 6, width = 6, pointsize = 10)

qqmath(lme2, col = sleepstudy$Subject, pch = sleepstudy$Days, main = "Random slope")

dev.off()

pdf("../figures/assump_qqplot_intercept.pdf", height = 6, width = 6, pointsize = 10)

qqmath(lme1, col = sleepstudy$Subject, pch = sleepstudy$Days, main = "Random intercept")

dev.off()


