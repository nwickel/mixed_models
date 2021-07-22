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

fix <- c("(Intercept)"=12, cses=2.5, meanses=5,
         sector=1, "cses:meanses"=0.7, 
         "cses:sector"=-0.7)

mat <- matrix(c(sp^2, cov,
                cov, spw^2), 2, 2)
ran <- chol(mat) / se

params <- list(theta=t(ran)[lower.tri(t(ran), TRUE)],
               beta=fix, sigma=se)

pval <- replicate(200, {
  mathach <- simulate(~ cses*meanses + cses*sector + 
                     (cses | school),
                     newparams=params, newdata=simdat,
                     family=gaussian)$sim_1
  m1 <- lmer(mathach ~ meanses + cses + sector
             + (cses | school), simdat, REML=FALSE)
  m2 <- lmer(mathach ~ cses*meanses + cses*sector
             + (cses | school), simdat, REML=FALSE)
  anova(m1, m2)$"Pr(>Chisq)"[2]
})

mean(pval < 0.05)
hist(pval)

