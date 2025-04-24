
design <- data.frame(F = factor(c("F1", "F2", "F3", "F4")),
                     mu = c(10, 20, 10, 40))

pdf("slides/figures/contrasts_ex4level.pdf", width = 3.375, height = 3.375, pointsize = 10)
par(mai = c(.6, .6, .1, .1), mgp = c(2.4, 1, 0))
plot(mu ~ as.numeric(F), data = design,
     type = "o", pch = 21, bg = "white", ylim = c(0, 50),
     axes = FALSE,
     xlab = "Word frequency",
     ylab = "Mean response time")
axis(1, at = 1:4, labels = c("low", "med-low", "med-high", "high"))
axis(2)
box()
dev.off()

# TODO: Reverse order of labeling! high frequency words should be faster, right?

design <- data.frame(
  F = factor(c("F1", "F2", "F3", "F4")),
  mu = c(10, 20, 10, 40))
contrasts(design$F)

lm(mu ~ F, design) |> coef() |> zapsmall()

# Custom contrasts
means <- c(10, 10, 20, 30)
c1 <- (means - mean(means)) / 2.5

contrasts(design$F) <- matrix(c1, nrow = 4)
print(xtable::xtable(model.matrix( ~ F, design), digits = 2), include.rownames = FALSE)
print(xtable::xtable(model.matrix( ~ F, design) |> MASS::ginv(),
                     digits = 3), include.rownames = FALSE)


c2 <- c(1, 1, 2, 3)
c3 <- c2 - mean(c2)
contrasts(design$F) <- matrix(c3, nrow = 4)
print(xtable::xtable(model.matrix( ~ F, design), digits = 2), include.rownames = FALSE)
print(xtable::xtable(model.matrix( ~ F, design) |> MASS::ginv(),
                     digits = 3), include.rownames = FALSE)


lm(means ~ F, design) |> coef() |> zapsmall()

design$Fcust <- model.matrix( ~ F, design)[, 2]

lm(means ~ Fcust, design) |> coef() |> zapsmall()




# Treatment contrasts
print(xtable::xtable(model.matrix( ~ F, design), digits = 0), include.rownames = FALSE)
print(xtable::xtable(model.matrix( ~ F, design) |> MASS::ginv() |> zapsmall(),
                     digits = 0), include.rownames = FALSE)

# Sum contrasts
contrasts(design$F) <- "contr.sum"
print(xtable::xtable(model.matrix( ~ F, design), digits = 0), include.rownames = FALSE)
print(xtable::xtable(model.matrix( ~ F, design) |> MASS::ginv() |> zapsmall(),
                     digits = 2), include.rownames = FALSE)

# Helmert contrasts
contrasts(design$F) <- "contr.helmert"
print(xtable::xtable(model.matrix( ~ F, design), digits = 2), include.rownames = FALSE)
print(xtable::xtable(model.matrix( ~ F, design) |> MASS::ginv() |> zapsmall(),
                     digits = 3), include.rownames = FALSE)

# Sequential difference contrasts
contrasts(design$F) <- MASS::contr.sdif(4)
print(xtable::xtable(model.matrix( ~ F, design), digits = 2), include.rownames = FALSE)
print(xtable::xtable(model.matrix( ~ F, design) |> MASS::ginv() |> zapsmall(),
                     digits = 0), include.rownames = FALSE)



#' # Simulate data

set.seed(1700)

n <- 20

dat <- data.frame(id = 1:n,
                   F = factor(rep(c("F1", "F2", "F3", "F4"), each = n / 4)),
                  DV = rnorm(n, mean = c(10, 20, 10, 40), sd = 5)) 

datm <- aggregate(DV ~ F, dat, mean)
aggregate(DV ~ F, dat, sd)

lattice::xyplot(DV ~ F, dat, type = c("a", "p"))

#' ## Treatment contrasts

contrasts(dat$F)

lm1 <- lm(DV ~ F, dat)
coef(lm1)
summary(lm1)

#' ## Sum contrasts

contrasts(dat$F) <- "contr.sum"

lm2 <- lm(DV ~ F, dat)
coef(lm2)
summary(lm2)

mean(datm$DV)

#' ## Helmert contrasts

contrasts(dat$F) <- "contr.helmert"

lm3 <- lm(DV ~ F, dat)
coef(lm3)
summary(lm3)

#' ## Sequential difference contrasts

contrasts(dat$F) <- MASS::contr.sdif(4)

lm4 <- lm(DV ~ F, dat)
coef(lm4)
summary(lm4)

diff(datm$DV)

# But maybe rather test this against the hypothesis that all factor levels
# differ in the same way

lm4b <- lm(DV ~ as.numeric(F), dat)
coef(lm4b)
summary(lm4b)

anova(lm4b, lm4)


#' ## Custom contrasts

means <- c(10, 10, 20, 30)
c1 <- (means - mean(means)) / 2.5

contrasts(dat$F) <- matrix(c1, nrow = 4)

lm5 <- lm(DV ~ F, dat)
coef(lm5)
summary(lm5)

Fcust <- model.matrix( ~ F, dat)[, 2]

lm5b <- lm(DV ~ Fcust, dat)
coef(lm5b)
summary(lm5b)


