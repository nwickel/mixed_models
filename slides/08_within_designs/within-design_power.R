# comparisons
# * 2x2 within design (one observation per cell)
# * 2x2 within design mit items (several observations per cell)
#   - raw data
#   - aggregated data

set.seed(1157)

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

###############################################################################

nsubj <- 10
nitem <- 5

#pdf("within-design_int-vs-slp.pdf", width = 8.5, height = 3.375, pointsize = 10)
#par(mfrow = c(1, 3), mai = c(.4, .4, .3, .1), mgp = c(2.4, 1, 0))

#----- 2x2 within design ------------------------------------------------------

pwr_int <- NULL
pwr_slp <- NULL

ns <- c(10, 20, 30, 40, 50)

for (nsubj in ns) {

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
  
  pwr_int <- c(pwr_int, mean(dat_ci$lb[dat_ci$model == "random intercept"] > 0))
  pwr_slp <- c(pwr_slp, mean(dat_ci$lb[dat_ci$model == "random slope"] > 0))

}

pdf("slides/figures/nico_power_2x2design_1obs.pdf",
    width = 3.375, height = 3.375, pointsize = 10)
par(mai = c(.6, .6, .2, .1), mgp = c(2.4, 1, 0))

plot(pwr_int ~ ns, type = "o", ylim = 0:1, xlab = "N", ylab = "Power",
     main = "2x2 within design: 1 obs per cell", pch = 21, bg = "white")
points(pwr_slp ~ ns, type = "o", col = "darkgreen", lty = 2, pch = 21,
       bg = "white")

abline(h = .8, col = "gray", lty = 3)

legend("bottomright", c("random intercept", "random slope"),
       col = c("black", "darkgreen"),
       lty = 1:2, bty = "n", pch = 21, pt.bg = "white")

dev.off()

#----- 2x2 within design with items -------------------------------------------

# y = mu + a2 + b2 + a2b2 + p + pa + pb + papb + w + e

sp     <- c(.4, .8, .6, .4)
r      <- -.2
S      <- r * sp %o% sp; diag(S) <- sp^2
Lt     <- chol(S) / se
theta  <- t(Lt)[lower.tri(Lt, diag = TRUE)]
sw     <- 1
theta2 <- c(theta, sw / se)

pwr_int <- NULL
pwr_slp <- NULL

ns <- c(2, 5, 8, 10, 15)

for (nsubj in ns) {

  dat <- expand.grid(A = factor(c("a1", "a2")),
                     B = factor(c("b1", "b2")),
                     item = factor(1:nitem),
                     id = factor(1:nsubj))
  
  
  dat_ci <- sim_cis("y ~ A * B + (1 | id) + (1 | item)",
                    "y ~ A * B + (A * B | id) + (1 | item)",
                    data = dat,
                    params = list(beta = beta, theta = theta2, sigma = se),
                    test_par = "Aa2:Bb2")
  
  pwr_int <- c(pwr_int, mean(dat_ci$lb[dat_ci$model == "random intercept"] > 0))
  pwr_slp <- c(pwr_slp, mean(dat_ci$lb[dat_ci$model == "random slope"] > 0))

}

pdf("slides/figures/nico_power_2x2design_5obs.pdf",
    width = 3.375, height = 3.375, pointsize = 10)
par(mai = c(.6, .6, .2, .1), mgp = c(2.4, 1, 0))
plot(pwr_int ~ ns, type = "o", ylim = 0:1, xlab = "N", ylab = "Power",
     main = "2x2 within design: 5 obs per cell", pch = 21, bg = "white")
points(pwr_slp ~ ns, type = "o", col = "darkgreen", lty = 2, pch = 21,
       bg = "white")

abline(h = .8, col = "gray", lty = 3)

legend("bottomright", c("random intercept", "random slope"),
       col = c("black", "darkgreen"),
       lty = 1:2, bty = "n", pch = 21, pt.bg = "white")
dev.off()

#----- 2x2 within design aggregated over items --------------------------------

# y = mu + a2 + b2 + a2b2 + p + pa + pb + e

pwr_int <- NULL
pwr_slp <- NULL

ns <- c(2, 5, 8, 10, 15)

for (nsubj in ns) {

  dat <- expand.grid(A = factor(c("a1", "a2")),
                     B = factor(c("b1", "b2")),
                     item = factor(1:nitem),
                     id = factor(1:nsubj))
    
  cis <- replicate(100, {
    y <- simulate( ~ A * B + (A * B | id) + (1 | item),
                  newdata = dat,
                  newparams = list(beta = beta, theta = theta2, sigma = se)
                  )$sim_1
    datm <- aggregate(y ~ A + B + id, dat, mean)
    m0 <- lmer(y ~ A * B + (1 | id), dat)
    m1 <- lmer(y ~ A * B + (A + B | id), dat)
    matrix(c(confint(m0, par = "Aa2:Bb2", method = "Wald") |> as.numeric(),
             confint(m1, par = "Aa2:Bb2", method = "Wald") |> as.numeric()),
           nrow = 2, byrow = TRUE)
    }, simplify = FALSE
  )
  dat_ci <- as.data.frame(do.call(rbind, cis))
  names(dat_ci) <- c("lb", "ub")
  dat_ci$model <- factor(c("random intercept", "random slope"))

  pwr_int <- c(pwr_int, mean(dat_ci$lb[dat_ci$model == "random intercept"] > 0))
  pwr_slp <- c(pwr_slp, mean(dat_ci$lb[dat_ci$model == "random slope"] > 0))

}

plot(pwr_int ~ ns, type = "o", ylim = 0:1, xlab = "N", ylab = "Power",
     main = "2x2 within design: 1 obs per cell aggregated",
     pch = 21, bg = "white")
points(pwr_slp ~ ns, type = "o", col = "darkgreen", lty = 2, pch = 21,
       bg = "white")

abline(h = .8, col = "gray", lty = 3)

legend("bottomright", c("random intercept", "random slope"),
       col = c("black", "darkgreen"),
       lty = 1:2, bty = "n", pch = 21, pt.bg = "white")

#dev.off()

