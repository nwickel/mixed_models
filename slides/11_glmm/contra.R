# Redo in R: https://repsychling.github.io/SMLP2024/glmm.html

library("lme4")

dat <- read.table("data/contra.dat", header = TRUE)

dat$districtID <- factor(dat$districtID)
dat$childCode <- factor(dat$childCode,
                        levels = 1:4,
                        labels = c("0", "1", "2", "3+"))
dat$isUrban <- factor(dat$isUrban,
                      levels = 0:1,
                      labels = c("n", "y"))

# simplify names
names(dat) <- c("dist", "use", "livch", "age", "urban")

# The information recorded included woman’s age, the number of live children she
# has, whether she lives in an urban or rural setting, and the political
# district in which she lives.

lattice::xyplot(use ~ age | urban, dat, groups = livch,
                type = "smooth", auto.key = TRUE)
# Smoothed relative frequency of contraception use versus centered age for women
# in the 1989 Bangladesh Fertility Survey

#contrasts(dat$urban) <- "contr.helmert"
#contrasts(dat$livch) <- "contr.treatment"   # default, but why not be explicit

# Fit model
gm1 <- glmer(use ~ age + I(age^2) + urban + livch + (1 | dist), data = dat,
             family = binomial("logit"))

# Create binary variable for livch

dat$children <- factor(ifelse(dat$livch == 0, "false", "true"),
                       levels = c("false", "true"))
#contrasts(dat$children) <- "contr.sum"

# Fit model with interaction
gm2 <- glmer(use ~ age * children + I(age^2) + urban + (1 | dist), data = dat,
             family = binomial("logit"))

# Compare models
data.frame(models = c("gm1", "gm2"),
           df = AIC(gm1, gm2)[, 1],
           AIC = AIC(gm1, gm2)[, 2],
           BIC = BIC(gm1, gm2)[, 2],
           deviance = c(deviance(gm1), deviance(gm2)))

# It turns out that there can be more difference between urban and rural
# settings within the same political district than there is between districts.

gm3 <- glmer(use ~ age * children + I(age^2) + urban + (1 | urban:dist),
             data = dat, family = binomial("logit"),
             control = glmerControl(calc.derivs = FALSE))

# Compare models
data.frame(models = c("gm1", "gm2", "gm3"),
           df = AIC(gm1, gm2, gm3)[, 1],
           AIC = AIC(gm1, gm2, gm3)[, 2],
           BIC = BIC(gm1, gm2, gm3)[, 2],
           deviance = c(deviance(gm1), deviance(gm2), deviance(gm3)))

# Notice that the parameter count in gm3 is the same as that of gm2 - the thing
# that has changed is the number of levels of the grouping factor- resulting in
# a much lower deviance for gm3. This reinforces the idea that a simple count of
# the number of parameters to be estimated does not always reflect the
# complexity of the model.

# Model predictions

newdat <- data.frame(children = factor(rep(c("true", "false"), each = 2)),
                     urban = factor(rep(c("y", "n"), times = 2)),
                     age = 0)

newdat$pre <- predict(gm3, type = "response", newdata = newdat, re.form = NA)

# Summary

# * From the data plot we can see a quadratic trend in the probability by age.
# * The patterns for women with children are similar and we do not need to
#   distinguish between 1, 2, and 3+ children.
# * We do distinguish between those women who do not have children and those
#   with children. This shows up in a significant age & children interaction
#   term.

# Plot predictions

newdat <- expand.grid(children = factor(c("true", "false")),
                      urban = factor(c("y", "n")),
                      age = -14:20)

newdat$pre <- predict(gm3, type = "response", newdata = newdat, re.form = NA)

lattice::xyplot(pre ~ age | urban, data = newdat, groups = children,
                type = "l",
                auto.key = TRUE)

