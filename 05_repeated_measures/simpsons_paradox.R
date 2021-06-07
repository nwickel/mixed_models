
library(lattice)
library(lme4)

setwd("C:/Users/numbach/Nextcloud/Documents/teaching/regression/05_repeated_measures/figures/")

dat <- data.frame(subj=factor(rep(1:5, each=2)),
                  x = c(1, 2, 2, 4, 3, 4, 4, 6, 6, 7),
                  y = c(3, 2, 4.5, 4.1, 5, 4.5, 6.5, 6, 7.8, 7)
                  )

lm1 <- lm(y ~ x, dat)

pdf("simp_lm.pdf", height=4.5, width=4.5)
xyplot(y ~ x, dat, pch=16, key=list(space="top", columns=2,
       text=list(c("", ""))))
dev.off()


pdf("simp_lme.pdf", height=4.5, width=4.5)
xyplot(y ~ x, dat, groups=subj, pch=16, key=list(space="top", columns=2,
       text=list(c("", ""))), type="b")
dev.off()


lme1 <- lmer(y ~ x + (1 | subj), dat)

pdf("simp_lme_pre.pdf", height=4.5, width=4.5)

xyplot(y ~ x, dat, groups=subj, type="b", pch=16,
       panel = function(...) {
         panel.xyplot(...)
         panel.abline(lm1, lty=2)
         panel.abline(fixef(lme1), lty=4)
       },
       key=list(space="top", columns=2, text=list(c("lm", "lmm")),
                lines=list(lty=c(2, 4)))
)

dev.off()

## But is this sensible??

dat$time <- 0:1

lm2 <- lm(y ~ x + time, dat)
lme2 <- lmer(y ~ x + time + (1 | subj), dat)

