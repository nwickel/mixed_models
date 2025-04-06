
library(lattice)
library(lme4)

# IVs:
# Partner: 2 within levels AI vs. hu
# Stakes:  2 within levels LS vs. HS
# Context: 5 between levels edu vs. fin vs. law vs. med vs. psy
#
# DVs:
# Trustworthiness
# Risk

#------ Hypothesis plot -------------------------------------------------------

design <- data.frame(Partner = factor(rep(c("AI", "hu"), each = 2, times = 5)),
                     Stakes = factor(rep(c("HS", "LS"), times = 5 * 2)),
                     Context = factor(rep(c("edu", "fin", "law", "med", "psy"),
                                          each = 2 * 2)))

beta <- c(4, .5, -.2, .5, 0, .5, .5, .5, .5, .5, .5, .5, .5, .5, 0, .5, 0, .5, .5, 0)

design$means <- model.matrix( ~ Partner * Stakes * Context, design) %*% beta

xyplot(means ~ Context | Stakes, design, groups = Partner,
       type = "b",
       auto.key = list(space = "top", column = 2),
       ylim = c(1, 7))

#------ Read and visualize data -----------------------------------------------

dat <- read.csv("data/data-nico.csv", stringsAsFactors = TRUE)
dat$Participant <- factor(dat$Participant)

length(unique(dat$Participant))
# 898

xyplot(Trustworthiness + Risk ~ Context | Stakes, data = dat,
       groups = Partner,
       ylim = c(1, 7),
       auto.key = TRUE,
       type = c("a", "g"))

plot(jitter(Trustworthiness) ~ jitter(Risk), dat,
     xlab = "Risk", ylab = "Trustworthiness")

ids <- sample(unique(dat$Participant), 40)

subdat <- subset(dat, dat$Participant %in% ids)

xyplot(Trustworthiness ~ Stakes | factor(Participant),
       groups = Partner,
       data = subdat,
       layout = c(5, 8),
       auto.key = list(space = "top", column = 2),
       type = "b")

xyplot(Trustworthiness + Risk ~ Partner | Stakes, data = dat,
       groups = Context,
       ylim = c(1, 7),
       auto.key = list(space = "top", column = 5),
       type = c("a", "g"))

#------ Fit LMM ---------------------------------------------------------------

m0 <- lmer(Risk ~ (Context + Partner + Stakes)^2 +
           (1 + Partner + Stakes | Participant), data = dat)
m1 <- lmer(Risk ~ Context * Partner * Stakes +
           (1 + Partner + Stakes | Participant), data = dat)
summary(m1)
anova(m0, m1)

rePCA(m1)
# --> does not look generate

#------ Check assumptions -----------------------------------------------------

hist(resid(m1))
plot(m1, pch = dat$Partner, col = dat$Context)
qqmath(m1, pch = dat$Partner, col = dat$Context)

#------ Investigate random effects --------------------------------------------

dotplot(ranef(m1, condVar = TRUE),
        scales = list(x = list(relation = "free")))[[1]]

pm1 <- profile(m1)

xyplot(pm1, which = "theta_")
densityplot(pm1, which = "theta_")
splom(pm1, which = "theta_")
confint(pm1)

## Simplify model
m2 <- lmer(Risk ~ Context * Partner * Stakes +
           (1 | Participant) +
           (0 + dummy(Partner) | Participant) +
           (0 + dummy(Stakes) | Participant),
         data = dat)
# --> what happens to the variance term for Stakes for Trustworthiness?

pm2 <- profile(m2)

xyplot(pm2, which = "theta_")
densityplot(pm2, which = "theta_")
splom(pm2, which = "theta_")
confint(pm2)


m3 <- lmer(Risk ~ Context * Partner * Stakes +
           (1 | Participant) +
           (0 + dummy(Partner) | Participant),
         data = dat)


pm3 <- profile(m3)

xyplot(pm3, which = "theta_")
densityplot(pm3, which = "theta_")
splom(pm3, which = "theta_")
confint(pm3)

anova(m3, m2, m1)


ci <- rbind(confint(pm1, "beta_"),
            confint(pm2, "beta_"),
            confint(pm3, "beta_")) |> as.data.frame()

ci$model <- rep(c("m1", "m2", "m3"), each = 20)
ci$par <- rownames(confint(pm1, "beta_"))

dotplot(model ~ `2.5 %` + `97.5 %` | par, ci)

## Random intercept model

m4 <- lmer(Risk ~ Context * Partner * Stakes + (1 | Participant),
         data = dat)

pm4 <- profile(m4)

xyplot(pm4, which = "theta_")
densityplot(pm4, which = "theta_")
splom(pm4, which = "theta_")
confint(pm4)

# interaction
ci <- rbind(confint(pm1, "Contextmed:Partnerhu:StakesLS"),
            confint(pm2, "Contextmed:Partnerhu:StakesLS"),
            confint(pm3, "Contextmed:Partnerhu:StakesLS"),
            confint(pm4, "Contextmed:Partnerhu:StakesLS")) |> as.data.frame()

ci$model <- factor(c("random slopes", "zero correlations", "no slope for stakes",
                     "random intercept"),
                   levels = c("random slopes", "zero correlations",
                              "no slope for stakes", "random intercept"))

dat_ci1 <- reshape(ci, direction = "long", varying = 1:2, v.names = "ci",
                  timevar = "bound", times = c("2.5%", "97.5%"))

dat_ci1$par <- "Contextmed:Partnerhu:StakesLS"

# pdf("slides/figures/nico_cis_data-risk_ia.pdf", width = 4.8, height = 4, pointsize = 10)
# dotplot(model ~ ci, dat_ci1, pch = "|", type = "b", groups = model, xlab = "",
#         main = "Contextmed:Partnerhu:StakesLS")
# dev.off()
# CI smaller, since it is a confidence interval for the fixed effect???

# main effect for partner
ci <- rbind(confint(pm1, "Partnerhu"),
            confint(pm2, "Partnerhu"),
            confint(pm3, "Partnerhu"),
            confint(pm4, "Partnerhu")) |> as.data.frame()

ci$model <- factor(c("random slopes", "zero correlations", "no slope for stakes",
                     "random intercept"),
                   levels = c("random slopes", "zero correlations",
                              "no slope for stakes", "random intercept"))

dat_ci2 <- reshape(ci, direction = "long", varying = 1:2, v.names = "ci",
                  timevar = "bound", times = c("2.5%", "97.5%"))

dat_ci2$par <- "Partnerhu"

# pdf("slides/figures/nico_cis_data-risk_me.pdf", width = 4.8, height = 4, pointsize = 10)
# dotplot(model ~ ci, dat_ci2, pch = "|", type = "b", groups = model, xlab = "",
#         main = "Partnerhu")
# dev.off()

pdf("slides/figures/nico_cis_data-risk.pdf", width = 7.5, height = 4, pointsize = 10)
dotplot(model ~ ci | par, rbind(dat_ci1, dat_ci2),
        pch = "|", type = "b", groups = model, xlab = "",
        scales = list(x = list(relation = "free")))
dev.off()

#------ Separate models for between groups ------------------------------------

# Context: medicine
med1 <- lmer(Risk ~ Partner * Stakes + (1 | Participant),
             data = dat, subset = Context == "med")

med2 <- lmer(Risk ~ Partner * Stakes +
             (1 | Participant) +
             (0 + dummy(Partner) | Participant) +
             (0 + dummy(Stakes) | Participant),
             data = dat, subset = Context == "med")

med3 <- lmer(Risk ~ Partner * Stakes +
             (1 + Partner + Stakes | Participant),
             data = dat, subset = Context == "med")

pmed1 <- profile(med1)
pmed2 <- profile(med2)
pmed3 <- profile(med3)

ci <- rbind(confint(pmed1, "Partnerhu:StakesLS"),
            confint(pmed2, "Partnerhu:StakesLS"),
            confint(pmed3, "Partnerhu:StakesLS")) |> as.data.frame()

ci$model <- factor(c("random intercept", "random slope no cor", "random slope"),
                   levels = c("random intercept", "random slope no cor", "random slope"))

dat_ci <- reshape(ci, direction = "long", varying = 1:2, v.names = "ci",
                  timevar = "bound", times = c("2.5%", "97.5%"))

dotplot(model ~ ci, dat_ci, pch = "|", type = "b", groups = model, xlab = "")


# Context: psychology
psy1 <- lmer(Risk ~ Partner * Stakes + (1 | Participant),
             data = dat, subset = Context == "psy")

psy2 <- lmer(Risk ~ Partner * Stakes +
             (1 | Participant) +
             (0 + dummy(Partner) | Participant) +
             (0 + dummy(Stakes) | Participant),
             data = dat, subset = Context == "psy")

psy3 <- lmer(Risk ~ Partner * Stakes +
             (1 + Partner + Stakes | Participant),
             data = dat, subset = Context == "psy")

ppsy1 <- profile(psy1)
ppsy2 <- profile(psy2)
ppsy3 <- profile(psy3)

ci <- rbind(confint(ppsy1, "Partnerhu:StakesLS"),
            confint(ppsy2, "Partnerhu:StakesLS"),
            confint(ppsy3, "Partnerhu:StakesLS")) |> as.data.frame()

ci$model <- factor(c("random intercept", "random slope no cor", "random slope"),
                   levels = c("random intercept", "random slope no cor", "random slope"))

dat_ci <- reshape(ci, direction = "long", varying = 1:2, v.names = "ci",
                  timevar = "bound", times = c("2.5%", "97.5%"))

dotplot(model ~ ci, dat_ci, pch = "|", type = "b", groups = model, xlab = "")


#------ Separate models for between groups - ANOVA ----------------------------

# Context: medicine
a1 <- aov(Risk ~ Partner * Stakes + Error(Participant),
        data = subset(dat, Context == "med"))

a2 <- aov(Risk ~ Partner * Stakes +
          Error(Participant + Participant:Partner + Participant:Stakes),
        data = subset(dat, Context == "med"))


# Context: psychology
a1 <- aov(Risk ~ Partner * Stakes + Error(Participant),
        data = subset(dat, Context == "psy"))

a2 <- aov(Risk ~ Partner * Stakes +
          Error(Participant + Participant:Partner + Participant:Stakes),
        data = subset(dat, Context == "psy"))


