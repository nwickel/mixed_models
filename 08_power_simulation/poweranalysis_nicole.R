# setwd("C:/Users/nwickelmaier/Nextcloud/Documents/teaching/regression/08_power_simulation/")

library(lme4)

dat0 <- read.csv("data_by_trial.csv", row.names = "X")


# VP ausschließen: 18023, 52409, 63779, 73209 
dat0 <- dat0[!dat0$participantID %in% c(18023, 52409, 63779, 73209), ]
dat0 <- dat0[dat0$Position != 81, ]
dat0$itemID <- paste0(dat0$Text_x, dat0$Position)

# only targets
dat <- dat0[dat0$Position %in% 1:40, ]


glm1 <- glmer(correctAnswer ~ truthvalue*condition + (1|participantID) +
              (1|itemID), dat, family = binomial)



dat_par <- as.data.frame(xtabs(correctAnswer ~ truthvalue + participantID, dat))

dat_par$n <- c(10, 30)

dat_par$prob <- dat_par$Freq / dat_par$n


tmp <- dat[!duplicated(dat[, c("participantID", "condition")]), c("participantID", "condition")]

dat_par <- merge(dat_par, tmp, by = "participantID", all = TRUE)
dat_par$Freq <- NULL
dat_par$n <- NULL
dat_par$condition <- factor(dat_par$condition)


datagg <- aggregate(prob ~ truthvalue + condition, dat_par, mean)


dat$predict <- predict(glm1, re.form = NA)
dat$prob <- predict(glm1, type = "resp", re.form = NA)

dat$condition <- factor(dat$condition)
dat$truthvalue <- factor(dat$truthvalue)

dat2 <- aggregate(cbind(prob, predict) ~ truthvalue + condition, dat, mean)


library(lattice)

xyplot(prob ~ condition, datagg, groups = truthvalue, type="b", 
       auto.key = TRUE, ylim = 0:1)
xyplot(prob ~ condition, dat2, groups = factor(truthvalue), type="b", 
       auto.key = TRUE, ylim = 0:1)
xyplot(predict ~ condition, dat2, groups = factor(truthvalue), type="b", 
       auto.key = TRUE)


summary(glm1)
# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod]
#  Family: binomial  ( logit )
# Formula: correctAnswer ~ truthvalue * condition + (1 | participantID) +      (1 | itemID)
#    Data: dat
# 
#      AIC      BIC   logLik deviance df.resid 
#   8058.7   8114.5  -4021.3   8042.7     7952 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.8724  0.2352  0.3947  0.5418  2.0346 
# 
# Random effects:
#  Groups        Name        Variance Std.Dev.
#  itemID        (Intercept) 0.2939   0.5422  
#  participantID (Intercept) 0.5758   0.7588  
# Number of obs: 7960, groups:  itemID, 200; participantID, 199
# 
# Fixed effects:
#                              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                   1.62170    0.14549  11.146  < 2e-16 ***
# truthvaluetrue               -0.01927    0.11845  -0.163 0.870759    
# conditionhigh                -0.49538    0.19161  -2.585 0.009727 ** 
# conditionlow                 -0.64252    0.19155  -3.354 0.000796 ***
# truthvaluetrue:conditionhigh  0.39346    0.16105   2.443 0.014562 *  
# truthvaluetrue:conditionlow   0.39686    0.15982   2.483 0.013023 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#                 (Intr) trthvl cndtnh cndtnl trthvltr:cndtnh
# truthvalutr     -0.611                                     
# conditinhgh     -0.701  0.463                              
# conditionlw     -0.702  0.464  0.531                       
# trthvltr:cndtnh  0.451 -0.736 -0.617 -0.342                
# trthvltr:cndtnl  0.455 -0.742 -0.344 -0.615  0.547     


### power simulation

n     <- 200   # try different values
nitem <- 40
ntext <- 5

# Create data frame

dat <- data.frame(id         = rep(1:n, each = nitem),
                  itemid     = 1:nitem,
                  textid     = rep(1:ntext, each = nitem),
                  condition  = rep(c("high", "low"), each = nitem),
                  truthvalue = rep(c("true", "false"), c(30, 10)),
                  stringsAsFactors = TRUE)

dat$itemtext <- factor(paste(dat$itemid, dat$textid, sep=":"))

pval <- replicate(50, {
      sim <- simulate( ~ truthvalue*condition + (1|id) + (1|itemtext), newdata=dat,
                      newparams=list(beta=c(0, 0.05, -0.4, 0.4),
                                     theta=c(0.6, 0.8)),
                      family=binomial)[,1]

      glm0 <- glmer(sim ~ truthvalue + condition + (1|id) + (1|itemtext), dat, family=binomial)
      glm1 <- glmer(sim ~ truthvalue*condition + (1|id) + (1|itemtext), dat, family=binomial)
      anova(glm0, glm1)["glm1", "Pr(>Chisq)"]
  })

mean(pval < 0.05)    # power

hist(pval)

# check if SD is theta
x <- replicate(100, {
  sim <- simulate( ~ truthvalue*condition + (1|id) + (1|itemtext), newdata=dat,
                  newparams=list(beta=c(0, 0.3, 0.01, 0.4),
                                 theta=c(0.5, 0.5)),
                  family=binomial)[,1]
  
  glm1 <- glmer(sim ~ truthvalue*condition + (1|id) + (1|itemtext), dat, family=binomial)
  as.data.frame(VarCorr(glm1))[, 5]
  }
)
rowMeans(x)
# [1] 0.4906525 0.4884420

# finding suitable parameters

sim <- simulate( ~ truthvalue*condition + (1|id) + (1|itemtext),
                newdata=dat, newparams=list(beta=c(0, 0.05, -0.4, 0.4),
                                            theta=c(0.001, 0.001)),
                family=binomial)[,1]

glm1 <- glmer(sim ~ truthvalue*condition + (1|id) + (1|itemtext), dat, family=binomial)
dat$prob <- predict(glm1, re.form = NA, type = "resp")

datagg <- aggregate(prob ~ truthvalue + condition, dat, mean)

xyplot(prob ~ condition, datagg, groups = truthvalue, type = "b",
       auto.key = TRUE, ylim = 0:1)


