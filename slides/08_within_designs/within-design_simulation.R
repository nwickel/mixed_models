library(lme4)

###############################################################################

sim_cis <- function(form1, form2, data, params, test_par, nsim = 100) {
  cis <- replicate(nsim, {
    y <- simulate(formula(gsub("^y", "", form2)),
                  newdata = data,
                  newparams = params)$sim_1
    m0 <- lmer(formula(form1), dat)
    m1 <- lmer(formula(form2), dat)
    matrix(c(confint(m0, par = test_par, method = "Wald") |> as.numeric(),
             confint(m1, par = test_par, method = "Wald") |> as.numeric()),
           nrow = 2, byrow = TRUE)
    }, simplify = FALSE
  )
  dat_ci <- as.data.frame(do.call(rbind, cis))
  names(dat_ci) <- c("lb", "ub")
  dat_ci$true_par <- tail(params$beta, 1)
  dat_ci$model <- factor(c("random intercept", "random slope"))
  dat_ci
}

plot_cis <- function(data) {
  plot(I(1:nrow(data)) ~ lb, data, type = "n", xlim = c(-3, 3),
       xlab = "", ylab = "")
  arrows(data$lb, 1:nrow(data), data$ub, 1:nrow(data),
         code = 3, angle = 90, length = 0.05,
         col = c("gray", "lightblue")[data$model])
  with(
    aggregate(cbind(lb, ub) ~ model, data, mean),
    arrows(lb, c(nrow(data) / 2, nrow(data) / 2 + 2),
           ub, c(nrow(data) / 2, nrow(data) / 2 + 2),
           code = 3, angle = 90, length = 0.05,
           col = c("black", "blue"), lwd = 3)
  )
  abline(v = c(0, unique(data$true_par)), lty = c(3, 1))
  legend("topleft", c("random intercept", "random slope"), lty = 1,
         col = c("black", "blue"), lwd = 3, bty = "n", cex = .9)
}

###############################################################################

nsubj <- 10
nitem <- 5

#----- 2x2 within design ------------------------------------------------------

dat <- expand.grid(A = factor(c("a1", "a2")),
                   B = factor(c("b1", "b2")),
                   id = factor(1:nsubj))

# y = mu + a2 + b2 + a2b2 + p + pa + pb + e

beta  <- c(3, .5, .5, 1)
sp    <- c(1, .8, .6)
r     <- -.5
se    <- 1
S     <- r * sp %o% sp; diag(S) <- sp^2
Lt    <- chol(S) / se
theta <- t(Lt)[lower.tri(Lt, diag = TRUE)]

dat_ci <- sim_cis("y ~ A * B + (1 | id)",
                  "y ~ A * B + (A + B | id)",
                  data = dat,
                  params = list(beta = beta, theta = theta, sigma = se),
                  test_par = "Aa2:Bb2")

pdf("slides/figures/nico_cis_2x2design_1obs.pdf",
    width = 3.375, height = 3.375, pointsize = 10)
par(mai = c(.6, .6, .2, .1), mgp = c(2.4, 1, 0))
plot_cis(dat_ci)
dev.off()

# Power
mean(dat_ci$lb[dat_ci$model == "random intercept"] > 0)
mean(dat_ci$lb[dat_ci$model == "random slope"] > 0)

#----- crossed random effects -------------------------------------------------

dat <- expand.grid(A = factor(c("a1", "a2")),
                   B = factor(c("b1", "b2")),
                   item = factor(1:nitem),
                   id = factor(1:nsubj))

# y = mu + a2 + b2 + a2b2 + p + pa + pb + w + e

beta   <- c(3, .5, .5, 1)
sp     <- c(1, .8, .6)
r      <- -.5
se     <- 1
S      <- r * sp %o% sp; diag(S) <- sp^2
Lt     <- chol(S) / se
theta  <- t(Lt)[lower.tri(Lt, diag = TRUE)]
sw     <- 1
theta2 <- c(theta, sw / se)

dat_ci <- sim_cis("y ~ A * B + (1 | id) + (1 | item)",
                  "y ~ A * B + (A + B | id) + (1 | item)",
                  data = dat,
                  params = list(beta = beta, theta = theta2, sigma = se),
                  test_par = "Aa2:Bb2")

plot_cis(dat_ci)

# Power
mean(dat_ci$lb[dat_ci$model == "random intercept"] > 0)
mean(dat_ci$lb[dat_ci$model == "random slope"] > 0)

#----- crossed random effects with interaction --------------------------------

dat <- expand.grid(A = factor(c("a1", "a2")),
                   B = factor(c("b1", "b2")),
                   item = factor(1:nitem),
                   id = factor(1:nsubj))

# y = mu + a2 + b2 + a2b2 + p + pa + pb + papb + w + e

sp     <- c(.4, .8, .6, .4)
r      <- -.2
S      <- r * sp %o% sp; diag(S) <- sp^2
Lt     <- chol(S) / se
theta  <- t(Lt)[lower.tri(Lt, diag = TRUE)]
sw     <- 1
theta2 <- c(theta, sw / se)

dat_ci <- sim_cis("y ~ A * B + (1 | id) + (1 | item)",
                  "y ~ A * B + (A * B | id) + (1 | item)",
                  data = dat,
                  params = list(beta = beta, theta = theta2, sigma = se),
                  test_par = "Aa2:Bb2")

pdf("slides/figures/nico_cis_2x2design_5obs.pdf",
    width = 3.375, height = 3.375, pointsize = 10)
par(mai = c(.6, .6, .2, .1), mgp = c(2.4, 1, 0))
plot_cis(dat_ci)
dev.off()

# Power
mean(dat_ci$lb[dat_ci$model == "random intercept"] > 0)
mean(dat_ci$lb[dat_ci$model == "random slope"] > 0)

#----- one within factor ------------------------------------------------------

dat <- expand.grid(A = factor(c("a1", "a2", "a3")),
                   id = factor(1:nsubj))

# y = mu + a2 + a3 + p + pa2 + pa3 + e

beta  <- c(3, .5, 1)
se    <- 1
sp    <- c(1, .8, .6)
theta <- sp / se

dat_ci <- sim_cis("y ~ A + (1 | id)",
                  "y ~ A + (1 | id) + (0 + dummy(A, 'a2') | id) + (0 + dummy(A, 'a3') | id)",
                  data = dat,
                  params = list(beta = beta, theta = theta, sigma = se),
                  test_par = "Aa3")

plot_cis(dat_ci)

# Power
mean(dat_ci$lb[dat_ci$model == "random intercept"] > 0)
mean(dat_ci$lb[dat_ci$model == "random slope"] > 0)

#----- crossed random effects -------------------------------------------------

dat <- expand.grid(A = factor(c("a1", "a2", "a3")),
                   item = factor(1:nitem),
                   id = factor(1:nsubj))

# y = mu + a2 + a3 + p + pa + w + e

beta   <- c(3, .5, 1)
se     <- 1
sp     <- c(1, .8, .6)
theta  <- sp / se
sw     <- 1
theta2 <- c(sp / se, sw / se)

dat_ci <- sim_cis("y ~ A + (1 | id) + (1 | item)",
                  "y ~ A + (1 | id) + (0 + dummy(A, 'a2') | id) + (0 + dummy(A, 'a3') | id) + (1 | item)",
                  data = dat,
                  params = list(beta = beta, theta = theta2, sigma = se),
                  test_par = "Aa3")

plot_cis(dat_ci)

# Power
mean(dat_ci$lb[dat_ci$model == "random intercept"] > 0)
mean(dat_ci$lb[dat_ci$model == "random slope"] > 0)

