# R and workflow

## S3 methods in R

I got the following question per e-mail this week:

> I used the function `corr_coef()` from the metan package to get a
> visualization of a correlation matrix, like this

```r
correlations <- corr_coef(data)
plot(correlations)
```

> The plot now sorts the correlations in a way that I don't understand. I want
> the original order. Do you know how to do this? And one more question: Is
> there a way to only display the correlations that are siginificant?

How would you try to find out about these questions? Assume that you have never
heard about this metan package (I had not).

```r
data <- iris[, -5]

correlations <- metan::corr_coef(data)

class(correlations)
typeof(correlations)

methods(class = "corr_coef")

?plot.corr_coef

plot(correlations, show = "signif", reorder = FALSE)

# Try out
?plot
?plot.default
methods(plot)
```

If you want to read up about S3 methods

* https://www.r-bloggers.com/2016/11/a-simple-guide-to-s3-methods/

* http://adv-r.had.co.nz/S3.html

* https://rstudio-education.github.io/hopr/s3.html

