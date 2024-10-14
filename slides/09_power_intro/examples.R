
n = 2000 # 10, 500, 2000

p = 0.515 # 0.5, 0.8, .44 .515

rbinom(n = n, size = 1, prob = p)

binom.test(rbinom(1, size = n, prob = p), n = n, p = p)


# power simulation

n <- 22000
p <- 102 / (102 + 100)

pval <- replicate(2000, { 
  x <- rbinom(1, size = n, prob = p)
  binom.test(x, n = n, p = (106) / (106 + 100))$p.value
  }
)

hist(pval)

mean(pval < 0.05)


# 2nd example

n <- 900

sd <- (125.04 - 62.2) / 0.41
diff <- 20

pval <- replicate(1000, {
  g1 <- rnorm(n, 0, sd)
  g2 <- rnorm(n, diff, sd)
  t.test(g1, g2, mu = 0, var.equal = TRUE)$p.value
  }
)

mean(pval < 0.05)


