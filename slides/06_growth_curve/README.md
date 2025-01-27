# R and workflow

Now, that we are actually starting to fit LMMs, I thought some tips on how to
deal with problems while fitting them would be helpful.

In general, all your questions have already been asked. Ben Bolker has a FAQ
list for (G)LMMs and `lme4` (and many other related packages) that gets updated
quite regularly: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html.

The R-sig-ME mailing list can be very informative about current discussions
about (G)LMMs. A lot of interesing people answer questions there. Ben Bolker is
very active, for example.

In order to search the list use google:
site://stat.ethz.ch/pipermail/r-sig-mixed-models/ your search terms

You can subscribe here: https://stat.ethz.ch/mailman/listinfo/r-sig-mixed-models

## Why are there no $$p$$ values?

The problem are actually not the $$p$$ values but how to calculate the degrees
of freedom for linear mixed-effects models. You can find some pointers with

```r
?lme4::pvalues
```

Here is the bit about $$p$$ values:
https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#why-doesnt-lme4-display-denominator-degrees-of-freedomp-values-what-other-options-do-i-have

## What do I do when my model does not converge?

Again, check out

```r
?lme4::convergence
```

A straightforward approach is to refit your model with all possible optimizers
and then look at the results with

```r
library(lme4)
model <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

modelfit.all <- allFit(model)
ss <- summary(modelfit.all)
```

but try out all the 5 steps you find at `?lme4::convergence` with your next
model!

And maybe read this:
https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#estimation. You do not
need to understand everything. Re-reading is also highly recommended.

# TODO

* Maybe add example from Hedecker & Gibbons (2006, Chap. 6) as another exercise

