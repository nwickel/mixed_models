library(JuliaCall)

julia_setup()
julia_source("SimulateJulia.jl")

design <- data.frame(
            A    = factor(rep(c("a1", "a2"), each = 2), levels = c("a1", "a2")),
            B    = factor(rep(c("b1", "b2"), 2), levels = c("b1", "b2")),
            prob = c(0.55, 0.55, 0.50, 0.40)
)

#--------------- Simple model ---------------

print("Starting with simple model")

# Fixed effects (dummy coding)
X    <- model.matrix( ~ A * B, data = design)
beta <- as.vector(solve(X) %*% qlogis(design$prob))
# Random effects
theta <- c(0.5, 0.3)

ns <- c(100, 150, 200)
res <- NULL

t1 <- system.time({

for (n in ns) {

  dat <- data.frame(
      id   = factor(rep(1:n, each = 25)),
      item = factor(paste(rep(1:5, each = 25), 1:25, sep = ":")),
      A    = factor(rep(c("a1", "a2"), each = 25)),
      B    = factor(rep(c("b1", "b2"), c(15, 10))),
      dv   = 1
  )

  res <- rbind(res, julia_call("SimulateSimple", dat, beta, theta))

}

})

#--------------- Complex model ---------------

print("Starting with complex model")

# Random effects
theta <- c(0.5, 0, 0, 0.3, 0, 0)

ns <- c(100, 150, 200)
res2 <- NULL

t2 <- system.time({

  for (n in ns) {
  
    dat <- data.frame(
        id   = factor(rep(1:n, each = 25)),
        item = factor(paste(rep(1:5, each = 25), 1:25, sep = ":")),
        A    = factor(rep(c("a1", "a2"), each = 25)),
        B    = factor(rep(c("b1", "b2"), c(15, 10))),
        dv   = 1
    )
  
    res2 <- rbind(res2, julia_call("SimulateComplex", dat, beta, theta))
  
  }

})

#--------------- Plot ---------------

res_total <- as.data.frame(rbind(res, res2))
res_total$samplesize <- ns
res_total$form <- rep(c(" ~ A * B + (1 | id) + (1 | item)",
                        " ~ A * B + (B | id) + (A | item)"), each = 3)

#   se_mean   pwr   se_boot        par samplesize                             form
# 0.1677803 0.647 0.1720283 -0.3920293        100  ~ A * B + (1 | id) + (1 | item)
# 0.1371642 0.819 0.1344739 -0.3951351        150  ~ A * B + (1 | id) + (1 | item)
#  0.118873 0.924 0.1160073 -0.3975086        200  ~ A * B + (1 | id) + (1 | item)
# 0.1759049 0.629 0.1629491 -0.3992313        100  ~ A * B + (B | id) + (A | item)
# 0.1427536 0.776 0.1412646 -0.3922937        150  ~ A * B + (B | id) + (A | item)
# 0.1231702 0.893 0.1168039 -0.3822269        200  ~ A * B + (B | id) + (A | item)


pdf("pwr.pdf", height = 5, width = 5, pointsize = 10)

lattice::xyplot(pwr ~ samplesize, res_total,
                groups = form,
                type = "b",
                ylim = 0:1,
                auto.key = list(space = "top"))

dev.off()

