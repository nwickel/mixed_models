# powersimulation.R
#
# content: (1) HSB data
#          (2) Data simulation
#          (3) Power simulation
#
# created: Jul/22/2021

library(lme4)
library(lattice)

#--------------- (1) HSB data ---------------

dat <- read.table("hsbdataset.txt", header=TRUE)

# Look at standard deviations
sd(dat$ses)
sd(dat$cses)
table(dat$sector) / nrow(dat)

lme1 <- lmer(mathach ~ cses*meanses + cses*sector + 
            (cses | school), dat)
summary(lme1)

xyplot(mathach ~ cses | as.factor(sector), dat, 
       groups=school, type="r")

#--------------- (2) Data simulation ---------------

nschool <- 150
nstudent <- 40
simdat <- data.frame( 
                  id = seq_len(nstudent),
                  school = seq_len(nschool),
                  sector = rep(0:1, each=nschool/2),
                  ses = rnorm(nschool*nstudent, 0, .8)
)

# Mean centering ses
simdat$meanses <- ave(simdat$ses, simdat$school)
simdat$cses <- simdat$ses - simdat$meanses

#--------------- (3) Power simulation ---------------

se  <- 6.5
sp  <- 1.6
spw <- 0.3
cov <- 0.4*sp*spw

sig <- matrix(c(sp^2, cov,
                cov, spw^2), 2, 2)

y01 <- mvtnorm::rmvnorm(nschool, mean=c(0, 0), sigma=sig)

# Random effects
theta <- c(y0 = y01[,1],
           y1 = y01[,2])

# Fixed effects
beta <- c("(Intercept)"=12, cses=2.5, meanses=5,
         sector=1, "cses:meanses"=0.7, 
         "cses:sector"=-0.7)

simdat$school <- factor(simdat$school)

X <- model.matrix( ~ cses*meanses + cses*sector, simdat)
Z <- model.matrix( ~ 0 + school + school:cses, simdat, 
                  contrasts.arg=list(school=contrasts(simdat$school,
                                                        contrasts=FALSE)))


pval <- replicate(200, {
  mathach <- X %*% beta + Z %*% theta + rnorm(nschool*nstudent, sd=se)
  m1 <- lmer(mathach ~ meanses + cses + sector
             + (cses | school), simdat, REML=FALSE)
  m2 <- lmer(mathach ~ cses*meanses + cses*sector
             + (cses | school), simdat, REML=FALSE)
  anova(m1, m2)$"Pr(>Chisq)"[2]
})

mean(pval < 0.05)
hist(pval)

