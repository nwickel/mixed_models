# powersimulation_GLMM_NW.R
#
# content: (1) Read data
#           (1.1) Data for Study 1
#           (1.2) Data for Study 2
#          (2) Analysis Study 1 
#          (3) Data simulation
#          (4) Power simulation
#          (5) Parameter recovery
#          (6) Estimation of inflation effect
#          (7) Analysis Study 2
#
# input:  data_by_trial.csv
#         Data_Nora.txt
# output: pval.RData
#         par_rev.RData
#         predlogit.pdf
#         predprob.pdf
#         find_params.pdf
#         inflation.pdf
#
# created: Jan/20/2023, NW
# last mod: Feb/02/2023, NW

# setwd("C:/Users/nwickelmaier/Nextcloud/Documents/teaching/regression/08_power_simulation/")

library(lme4)
library(ggplot2)

#--------------- (1) Read data ---------------

#--------------- (1.1) Data for Study 1
dat0 <- read.csv("data_by_trial.csv", row.names = "X")

# VP ausschließen: 18023, 52409, 63779, 73209 
dat0 <- dat0[!dat0$participantID %in% c(18023, 52409, 63779, 73209), ]
dat0 <- dat0[dat0$Position != 81, ]
dat0$itemID <- paste0(dat0$Text_x, dat0$Position)

# only targets
dat1 <- dat0[dat0$Position %in% 1:40, ]
dat1$condition <- factor(dat1$condition)
dat1$truthvalue <- factor(dat1$truthvalue)

rm(dat0)

#--------------- (1.2) Data for Study 2
dat2 <- na.omit(read.table("Data_Nora.txt", row.names = "X", na.string = ""))
dat2$truthvalue <- factor(dat2$truthvalue)
dat2$condition <- factor(dat2$condition)
dat2$participantID <- factor(dat2$participantID)
dat2$itemID <- factor(dat2$itemID)

#--------------- (2) Analysis Study 1 ---------------

# Model predictions
glm1 <- glmer(correctAnswer ~ truthvalue*condition + (1|participantID) +
              (1|itemID), dat1, family = binomial)
summary(glm1)
#xtable::xtable(summary(glm1)$coef)

dat1$logit <- predict(glm1, re.form = NA)
dat1$prob <- predict(glm1, type = "resp", re.form = NA)

logit <- aggregate(logit ~ truthvalue + condition, dat1, mean)
names(logit)[3] <- "resp"
prob <- aggregate(prob ~ truthvalue + condition, dat1, mean)
names(prob)[3] <- "resp"

datm1 <- rbind(prob, logit)
datm1$type <- factor(rep(c("prob", "logit"), each = 6))

ggplot(data = datm1) + 
  geom_point(mapping = aes(x = condition, y = resp, color = truthvalue)) +
  geom_line(mapping = aes(x = as.numeric(condition), y = resp, color = truthvalue)) +
  ylim(0, 2) +
  theme_bw() +
  facet_wrap(vars(type))

ggplot(data = datm1[datm1$type == "logit",]) + 
  geom_point(mapping = aes(x = condition, y = resp, color = truthvalue)) +
  geom_line(mapping = aes(x = as.numeric(condition), y = resp, color = truthvalue)) +
  ylim(0.5, 2) +
  theme_bw() +
  theme(legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      strip.background = element_rect(fill = "transparent"),
      strip.text = element_text(face = "bold"),
      plot.background = element_rect(fill = "transparent", color = NA),
      )

ggsave("figures/predlogit.pdf", width = 4.75, height = 3.375)

ggplot(data = datm1[datm1$type == "prob",]) + 
  geom_point(mapping = aes(x = condition, y = resp, color = truthvalue)) +
  geom_line(mapping = aes(x = as.numeric(condition), y = resp, color = truthvalue)) +
  ylim(0.5, 1) +
  theme_bw() +
  theme(legend.background = element_rect(fill = "transparent"),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent"),
      panel.background = element_rect(fill = "transparent"),
      strip.background = element_rect(fill = "transparent"),
      strip.text = element_text(face = "bold"),
      plot.background = element_rect(fill = "transparent", color = NA),
      )

ggsave("figures/predprob.pdf", width = 4.75, height = 3.375)

#--------------- (3) Data simulation ---------------

n <- 200

# Create data frame
dat <- data.frame(
    id         = factor(rep(1:n, each = 40)),
    item       = factor(paste(rep(1:5, each = 40), 1:40, sep = ":")),
    condition  = factor(rep(c("high", "low"), each = 40)),
    truthvalue = factor(rep(c("true", "false"), c(30, 10)))
)

# Do some checks
xtabs( ~ id + item, dat)
xtabs( ~ condition + truthvalue, dat)
xtabs( ~ condition + truthvalue + id, dat)

# Visually finding suitable parameters

set.seed(1226)

sim <- simulate( ~ truthvalue * condition + (1|id) + (1|item),
                newdata = dat,
                newparams = list(beta = c(1.5, -0.02, -0.4, 0.4),
                                 theta = c(0.001, 0.001)),
                family = binomial)[,1]

#g <- glmer(sim ~ truthvalue * condition + (1|id) + (1|item), dat, family=binomial)
g <- glm(sim ~ truthvalue * condition, dat, family = binomial)
summary(g)

dat$prob <- predict(g, type = "resp")
dat$logit <- predict(g)

prob <- aggregate(prob ~ truthvalue + condition, dat, mean)
names(prob)[3] <- "resp"
logit <- aggregate(logit ~ truthvalue + condition, dat, mean)
names(logit)[3] <- "resp"

datm <- rbind(prob, logit)
datm$type <- rep(c("prob", "logit"), each = 4)

ggplot(data = datm[datm$type == "prob", ]) + 
  geom_point(mapping = aes(x = condition, y = resp, color = truthvalue)) +
  geom_line(mapping = aes(x = as.numeric(condition), y = resp, color = truthvalue)) +
  ylim(0.5, 1) +
  #ylim(-0.5, 1) +
  theme_bw() +
  theme(legend.position="top")# +
  #facet_wrap(vars(type))

ggsave("figures/find_params.pdf", width = 5.75, height = 3.375)

#--------------- (4) Power simulation ---------------

ran <- c("id.(Intercept)"   = 0.6,
         "item.(Intercept)" = 0.8)
fix <- c("(Intercept)"                 = 0, 
         "truthvaluetrue"              = 0.05,
         "conditionlow"                = -0.4,
         "truthvaluetrue:conditionlow" = 0.4)

# Simulate DV
sim <- simulate( ~ truthvalue * condition + (1|id) + (1|item),
                newdata   = dat,
                newparams = list(beta = fix, theta = ran),
                family    = binomial,
                nsim      = 10)

pval <- numeric(ncol(sim))

for (i in seq_len(ncol(sim))) {
  m1 <- glmer(sim[, i] ~ truthvalue * condition + 
                (1|id) + (1|item), dat, family = binomial)
  pval[i] <- summary(m1)$coef["truthvaluetrue:conditionlow", "Pr(>|z|)"]
  }

# Power
mean(pval < 0.05)
hist(pval)

save(pval, file = "pval.RData")

#--------------- (5) Parameter recovery ---------------

par_rev <- replicate(400, {
  sim <- simulate( ~ truthvalue*condition + (1|id) + (1|item),
                  newdata = dat,
                  newparams = list(beta = c(0, 0.01, -0.2, 0.2),
                                   theta = c(0.5, 0.5)),
                  family = binomial)[,1]
  glmer(sim ~ truthvalue * condition + (1|id) + (1|item), 
        data = dat, family = binomial)
  })

save(par_rev, file = "par_rev.RData")

mean(sapply(par_rev, fixef)[1,]); mean(sapply(par_rev, fixef)[2,])
mean(sapply(par_rev, fixef)[3,]); mean(sapply(par_rev, fixef)[4,])

mean(sqrt(sapply(par_rev, function(x) unlist(VarCorr(x)))[1,]))
mean(sqrt(sapply(par_rev, function(x) unlist(VarCorr(x)))[2,]))

#--------------- (6) Estimation of inflation effect ---------------

idx <- order(sapply(par_rev, function(x) fixef(x)["truthvaluetrue:conditionlow"]))
# 
int <- sapply(par_rev[idx], function(x) fixef(x)["truthvaluetrue:conditionlow"])
ci <- sapply(par_rev[idx], function(x) confint(x, method = "Wald")["truthvaluetrue:conditionlow", ])
p <- sapply(par_rev[idx], function(x) summary(x)$coef["truthvaluetrue:conditionlow", "Pr(>|z|)"])

int <- sapply(par_rev, 
              function(x) fixef(x)["truthvaluetrue:conditionlow"])
ci <- sapply(par_rev, 
             function(x) confint(x, method = "Wald")["truthvaluetrue:conditionlow", ])
p <- sapply(par_rev, 
            function(x) summary(x)$coef["truthvaluetrue:conditionlow", "Pr(>|z|)"])

mean(p < 0.05)
hist(p)

pdf("figures/inflation.pdf", width = 7, height = 3.375, pointsize = 10)
par(mai = c(0.6, 0.6, 0.1, 0.1), mgp = c(2, 0.7, 0))

plot(int, type = "n", ylim = c(-.5, 1), ylab = "truthvaluetrue:conditionlow", 
     xlab = "sample id")
arrows(seq_along(int), ci[1, ], seq_along(int), ci[2, ], 0.02, 90, 3, col = "darkgray")
points(int, pch = 21, col = "darkmagenta", bg = c("darkmagenta", "white")[(p < 0.05) + 1])
abline(h = c(0, 0.2), lty = c(2, 1))
legend("bottomright", c("not significant", "significant"), pch = 21,
       col = "darkmagenta", pt.bg = c("darkmagenta", "white"), bty = "n")
legend("topleft",
       c(expression(Effect == 0.2),
         expression(list(n == 200, power %~~% 0.5))),
       bty = "n")

dev.off()

summary(int[p < 0.05])

#--------------- (7) Analysis Study 2 ---------------

glm2 <- glmer(correctAnswer ~ truthvalue * condition + (1|participantID) +
              (1|itemID), dat2, family = binomial)
summary(glm2)

dat2$prob <- predict(glm2, type = "resp")
dat2$logit <- predict(glm2)

prob <- aggregate(prob ~ truthvalue + condition, dat2, mean)
names(prob)[3] <- "resp"
logit <- aggregate(logit ~ truthvalue + condition, dat2, mean)
names(logit)[3] <- "resp"

datm2 <- rbind(prob, logit)
datm2$type <- rep(c("prob", "logit"), each = 4)

ggplot(data = datm2) + 
  geom_point(mapping = aes(x = condition, y = resp, color = truthvalue)) +
  geom_line(mapping = aes(x = as.numeric(condition), y = resp, color = truthvalue)) +
  #ylim(-0.5, 1) +
  theme_bw() +
  facet_wrap(vars(type))

datagg <- aggregate(correctAnswer ~ truthvalue + condition, dat2, mean)

ggplot(data = datagg) + 
  geom_point(mapping = aes(x = condition, y = correctAnswer, color = truthvalue)) +
  geom_line(mapping = aes(x = as.numeric(condition), y = correctAnswer, color = truthvalue)) +
  ylim(0, 1) +
  theme_bw() +
  theme(legend.position="top")

ggsave("figures/study2.pdf", width = 3.375, height = 3.375)

