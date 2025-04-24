# Maybe pick one of the examples by Reinhold Kliegl

* https://repsychling.github.io/SMLP2024/contrasts_fggk21.html
* https://repsychling.github.io/SMLP2024/contrasts_kwdyz11.html

# Nice technical details overview

* https://juliastats.org/StatsModels.jl/stable/contrasts/

* https://stats.stackexchange.com/questions/345842/what-is-the-appropriate-zero-correlation-parameter-model-for-factors-in-lmer

* https://github.com/lme4/lme4/issues/818

# Other data sets

```r
library("lme4")
data("Machines", package = "MEMSS")
```

# ANOVA

Remember:

```r
summary(aov2, split = list(cond = list("ise" = 1, "short" = 2, "long" = 3)))
```

Useful to look at hand-made contrasts.


