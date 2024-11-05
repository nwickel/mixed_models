# 02_exercises.R
#
# content: (1) Exercise 1
#
# input: --
# output: --
#
# last mod: Oct/16/2024, NW

#--------------- (1) Exercise 1 -----------------------------------------------

dat <- data.frame(x = 37:43,
                  y = c(2, 3, 10, 25, 34, 36, 39),
                  n = 40)

glm1 <- glm(cbind(y, n - y) ~ x, binomial, dat)

a <- 1 / coef(glm1)[2]
c <- -coef(glm1)[1] / coef(glm1)[2]

newx <- seq(37, 43, .1)
pre <- predict(glm1, data.frame(x = newx), type = "response")

plot(y/n ~ x, dat, pch = 16, ylab = "Probability to say brighter")
lines(pre ~ newx, dat)
abline(v = c, h = .5, lty = 3)
text(39, .8, paste("PSE =", round(c,2)))

# goodness-of-fit test
glms <- glm(cbind(y, n - y) ~ factor(x), family = binomial, data = dat)
anova(glm1, glms, test = "Chisq")

# overdispersion
summary(glm(cbind(y, n-y) ~ x, family = quasibinomial, data = dat))


