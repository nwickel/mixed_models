# TODO

* Add model with nonlinear subjects trends for imi data from rep meas session?

* Add this example:
  https://embraceuncertaintybook.com/longitudinal.html#the-elstongrizzle-data

* Maybe add example from Hedecker & Gibbons (2006, Chap. 6) as exercise

### Code for shrinkage plots:
https://www.r-bloggers.com/2013/08/visualising-shrinkage/

```r
# *--------------------------------------------------------------------
# | FUNCTION: shrinkPlot
# | Function for visualising shrinkage in hierarchical models
# *--------------------------------------------------------------------
# | Version |Date      |Programmer  |Details of Change                
# |     01  |31/08/2013|Simon Raper |first version.      
# *--------------------------------------------------------------------
# | INPUTS:  orig      Estimates obtained from individual level 
# |                    modelling
# |          shrink    Estimates obtained from hierarchical modelling
# |          prior     Priors in Bayesian model or fixed effects in 
# |                    mixed effects model (i.e. what it is shrinking
# |                    towards.
# |          window    Limits for the plot (as a vector)
# |
# *--------------------------------------------------------------------
# | OUTPUTS: A ggplot object
# *--------------------------------------------------------------------
# | DEPENDS: grid, ggplot2
# |
# *--------------------------------------------------------------------

library(ggplot)
library(grid)
shrinkPlot<-function(orig, shrink, prior, window=NULL){
  
    group<-factor(signif(prior,3))
    
    data<-data.frame(orig, shrink, prior, group)
  
  g<-ggplot(data=data, aes(x=orig, xend=orig, y=orig, yend=shrink, col=group))
    g2<-g+geom_segment(arrow = arrow(length = unit(0.3, "cm"))) +geom_point(data=comp.in, aes(x=coef, y=mean))  
  g3<-g2+xlab("Estimate")+ylab("Shrinkage")+ ggtitle("Shrinkage Plot")
    
    if (is.null(window)==FALSE){
        g3<-g3+ylim(window)+xlim(window) 
    }
    
    print(g3)
    
}
```

#### Another example
https://www.r-bloggers.com/2015/01/visualizing-bivariate-shrinkage/

```r
## 2d shrikage for random slope, random intercept model
library(lme4)
library(ggplot2)
library(grid)
library(mvtnorm)
library(reshape2)
library(ellipse)
# fit models
m0 <- lm(Reaction ~ Days * Subject, data=sleepstudy)
m1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# extract fixed effect estimates
intercepts <- grepl("Days", names(coef(m0))) == FALSE
m0_intercepts <- coef(m0)[intercepts]
m0_intercepts[-1] <- m0_intercepts[-1] + m0_intercepts[1]
slopes <- !intercepts
m0_slopes <- coef(m0)[slopes]
m0_slopes[-1] <- m0_slopes[-1] + m0_slopes[1]
# extract random effect estimates
m1_intercepts <- coef(m1)$Subject[, 1]
m1_slopes <- coef(m1)$Subject[, 2]
d <- data.frame(interceptF = m0_intercepts,
                slopeF = m0_slopes,
                interceptR = m1_intercepts,
                slopeR = m1_slopes
                )
# 95% bivariate normal ellipse for random effects
df_ell <- data.frame(ellipse(VarCorr(m1)$Subject, centre=fixef(m1)))
names(df_ell) <- c("intercept", "slope")
# bivariate normal density surface
lo <- 200 # length.out of x and y grid
xvals <- seq(from = min(df_ell$intercept) - .1 * abs(min(df_ell$intercept)),
             to = max(df_ell$intercept) + .05 * abs(max(df_ell$intercept)),
             length.out = lo)
yvals <- seq(from = min(df_ell$slope) - .4 * abs(min(df_ell$slope)),
             to = max(df_ell$slope) + .1 * abs(max(df_ell$slope)),
             length.out = lo)
z <- matrix(0, lo, lo)
for (i in 1:lo) {
  x = xvals
  y = yvals[i]
  z[,i] = dmvnorm(cbind(x,y),
                  mean = fixef(m1),
                  sigma = VarCorr(m1)$Subject)
}
mv_ranef <- melt(z)
names(mv_ranef) <- c("x", "y", "z")
mv_ranef$x <- xvals[mv_ranef$x]
mv_ranef$y <- yvals[mv_ranef$y]
p <- ggplot(d) +
  geom_raster(aes(x=x, y=y, fill=z), data=mv_ranef) +
  scale_fill_gradient(low="white", high="red") +
  guides(fill=FALSE) +
  geom_path(data=df_ell, aes(x=intercept, y=slope), size=.5) +
  geom_contour(aes(x=x, y=y, z=z), data=mv_ranef, size=.1, color="black") +
  geom_segment(aes(x=interceptF, y=slopeF,
                   xend=interceptR, yend=slopeR),
               arrow = arrow(length = unit(0.3, "cm")),
               alpha=.7) +
  xlab("Estimated intercepts") +
  ylab("Estimated slopes") +
  theme(legend.position="none") +
  ggtitle("Bivariate shrinkage plot") +
  theme_bw()  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p
```

