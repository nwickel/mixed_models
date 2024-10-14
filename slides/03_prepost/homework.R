library(lme4)

# Data simulation

set.seed(1042)

n <- 90

effects_condition <- c(1, 0.2, 0.1)

condition <- factor(rep(c("contr", "treat1", "treat2"), each = n / 3))
pre  <- rnorm(n)
post <- 0.3 * pre + model.matrix( ~ condition) %*% c(1, 0.2, 0.1) + rnorm(n)

dat <- data.frame(id        = factor(1:n),
                  pre       = pre,
                  post      = post,
                  condition = condition
       )

rm(pre, post, condition)

#dat$pre <- dat$pre mean(dat$pre)

lattice::xyplot(post ~ pre, dat, groups = condition, type = c("g", "p", "r"))

# Observed averaged time effects
aggregate(post - pre ~ condition, dat, mean)

# Differences in pre scores for different groups
aggregate(pre ~ condition, dat, mean)


# Fit models

## Change Score Model

#m1 <- lm(post - pre ~ condition, dat)
m1 <- lm(post ~ condition + offset(pre), dat)
summary(m1)

# Interpretation:
# beta0 is the time effect in the control group: subjects are 0.945 units better
# in the post test than in the pre test
# beta1 is the difference of the time effect in the treatment 1 group to the
# time effect in the control group: The time effect in the treatment 1 group is
# 0.945 + 0.101 = 1.046
# beta2 is the difference of the time effect in the treatment 2 group to the
# time effect in the control group: The time effect in the treatment 2 group is
# 0.945 + 0.391 = 1.336

## ANCOVA Model

m2 <- lm(post ~ condition + pre, dat)
summary(m2)

# Interpretation:
# beta0 is the adjusted time effect in the control group: subjects are 0.960
# units better in the post test than in the pre test when controlling for the
# pre test value
# beta1 is the difference of the time effect in the treatment 1 group to the
# time effect in the control group (again when adjusting for pre test score):
# The time effect in the treatment 1 group is 0.960 + 0.190 = 1.15
# beta2 is the difference of the time effect in the treatment 2 group to the
# time effect in the control group (again when adjusting for pre test score):
# The time effect in the treatment 2 group is 0.960 + 0.260 = 1.220

# Adjusted means
aggregate(predict(m2) ~ condition, dat, mean)

## Mixed Model (equivalent to change score model)

dat_long <- reshape(dat, direction = "long",
                    varying = list(c("pre", "post")),
                    v.names = "resp",
                    idvar = "id",
                    timevar = "time",
                    times = c(0, 1))

m3 <- lmer(resp ~ condition*time + (1 | id), dat_long)
summary(m3)

# Interpretation:
# beta4 is the time effect in the control group: subjects are 0.945 units better
# in the post test than in the pre test
# beta5 is the difference of the time effect in the treatment 1 group to the
# time effect in the control group: The time effect in the treatment 1 group is
# 0.945 + 0.101 = 1.046
# beta6 is the difference of the time effect in the treatment 2 group to the
# time effect in the control group: The time effect in the treatment 2 group is
# 0.945 + 0.391 = 1.336

# Compare parameters of the three models

cbind(coef(m1), coef(m2)[1:3], fixef(m3)[4:6])

