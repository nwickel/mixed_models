# 02_exercises.R
#
# content: (1) Exercise 1
#          (2) Exercise 2
#
# input: --
# output: --
#
# last mod: April/29/2021, NU

#--------------- (1) Exercise 1 ---------------

x   <- rnorm(100, mean=1)
y   <- rnorm(100, mean=2)
dat <- data.frame(id=1:200, group=rep(c("x","y"), each=100), score=c(x, y))

rm(x,y)

t1   <- t.test(score ~ group, dat, var.equal=TRUE)
lm1  <- lm(score ~ group, dat)
aov1 <- aov(score ~ group, dat)
(stat <- list(
    coef=matrix(c(t1$estimate, lm1$coef, aov1$coef), 2, 3,
        dimnames=list(NULL, c("ttest", "lm", "aov"))),
    statistics=matrix(c(t=t1$statistic^2, Flm=summary(lm1)$fstatistic[1],
        Faov=unlist(summary(aov1))[7]), 1, 3, dimnames=list(NULL,
        c("t","Flm","Faov"))))
)

#--------------- (2) Exercise 2 ---------------

dat <- data.frame(x = 37:43,
                  y = c(2, 3, 10, 25, 34, 36, 39),
                  n = 40)

glm1 <- glm(cbind(y, n-y) ~ x, binomial, dat)

a <- 1 / coef(glm1)[2]
c <- -coef(glm1)[1]/coef(glm1)[2]

newx <- seq(37, 43, .1)
pre <- predict(glm1, data.frame(x=newx), type="response")

plot(y/n ~ x, dat, pch=16, ylab="Probability to say brighter")
lines(pre ~ newx, dat)
abline(v=c, h=.5, lty=3)
text(39, .8, paste("PSE =", round(c,2)))

# goodness-of-fit test
glms <- glm(cbind(y, n-y) ~ factor(x), binomial, dat)
anova(glm1, glms, test="Chisq")

# overdispersion
summary(glm(cbind(y, n-y) ~ x, quasibinomial, dat))


