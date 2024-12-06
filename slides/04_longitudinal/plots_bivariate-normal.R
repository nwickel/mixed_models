set.seed(1212)

r <- -0.8
mu <- c(0, 0)
Sigma <- matrix(c(1, r, r, 1), 2)

dat <- mvtnorm::rmvnorm(500, mean = mu, sigma = Sigma)
x <- seq(-3, 3, 0.25)
y <- seq(-3, 3, 0.25)
f <- function(x, y) mvtnorm::dmvnorm(cbind(x, y), mean = mu, sigma = Sigma)
z <- outer(x, y, f)

pdf(paste0("../figures/bivariate-normal_0", r * 10, ".pdf"), width = 9, height = 3, pointsize = 14)
par(mfrow = c(1, 3), mai = c(.5, .5, .1, .1), mgp = c(2, 1, 0))

plot(dat, xlab = "x", ylab = "y", xlim = c(-3, 3), ylim = c(-3, 3))
legend("topright", paste("r =", r), bty = "n")

contour(x, y, z)
persp(x, y, z, theta = -10, phi = 25, expand = 0.7, shade = 0.2, zlab = "")

dev.off()

