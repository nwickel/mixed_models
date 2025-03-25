# powersimulation_GLMM.R
#
# content: (1) Power simulation
#          (2) Plot
#
# input: --
# output: ps.RData
#
# created: Feb/02/2023, NW
# last mod: Feb/02/2023, NW

library(lme4)

#--------------- (1) Power simulation ---------------

ran <- c("id.(Intercept)"   = 0.6,
         "item.(Intercept)" = 0.8)
fix <- c("(Intercept)"                 = 0, 
         "truthvaluetrue"              = 0.05,
         "conditionlow"                = -0.4,
         "truthvaluetrue:conditionlow" = 0.4)

ns <- c(100, 120, 140, 160, 200)
ps <- list()

for (n in ns) {

  # Create data frame
  dat <- data.frame(
      id         = factor(rep(1:n, each = 40)),
      item       = factor(paste(rep(1:5, each = 40), 1:40, sep = ":")),
      condition  = factor(rep(c("high", "low"), each = 40)),
      truthvalue = factor(rep(c("true", "false"), c(30, 10)))
  )
  
  # Simulate DV
  sim <- simulate( ~ truthvalue * condition + (1|id) + (1|item),
                  newdata   = dat,
                  newparams = list(beta = fix, theta = ran),
                  family    = binomial,
                  nsim      = 400)
  
  pval <- numeric(ncol(sim))
  
  for (i in seq_len(ncol(sim))) {
    m1 <- glmer(sim[, i] ~ truthvalue * condition + 
                  (1|id) + (1|item), dat, family = binomial)
    pval[i] <- summary(m1)$coef["truthvaluetrue:conditionlow", "Pr(>|z|)"]
    print(paste0("n = ", n, ", i = ", i))
    }
  ps[[paste0("n", n)]] <- pval
}

# save(ps, file = "ps.RData")

#--------------- (2) Plot ---------------

pwrs <- sapply(ps, function(x) mean(x < 0.05))

plot(ns, pwrs, type = "b", ylim = 0:1)
text(ns, pwrs + .05, pwrs)

