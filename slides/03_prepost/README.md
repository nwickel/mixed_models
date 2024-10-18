# R and workflow

Here are some clean coding tips:

* Check out the tidyverse styleguide: https://style.tidyverse.org/

* It is very concise and easy to follow.

* Things to incorporate into your workflow __right now__:

  - Put some effort into your file names. It is always worth it.

  - Use sensible Spacing:

```{r}
## Spacing
# Good
x[, 1]

# Bad
x[,1]
x[ ,1]
x[ , 1]

## Parantheses
# Good
mean(x, na.rm = TRUE)

# Bad
mean (x, na.rm = TRUE)
mean( x, na.rm = TRUE )

## Infix operators
# Good
height <- (feet * 12) + inches
mean(x, na.rm = TRUE)

# Bad
height<-feet*12+inches
mean(x, na.rm=TRUE)

## Exceptions, which should never be surrounded by spaces:
# ::, :::, $, @, [, [[, ^, unary -, unary +, and :

# Good
sqrt(x^2 + y^2)
df$z
x <- 1:10

# Bad
sqrt(x ^ 2 + y ^ 2)
df $ z
x <- 1 : 10

# Good
package?stats
?mean

# Bad
package ? stats
? mean

## Adding extra spaces is ok if it improves alignment of = or <-
# Good
list(
  total = a + b + c,
  mean  = (a + b + c) / n
)

# Also fine
list(
  total = a + b + c,
  mean = (a + b + c) / n
)

## Function calls
# Good
mean(1:10, na.rm = TRUE)

# Bad
mean(x = 1:10, , FALSE)
mean(, TRUE, x = c(1:10, NA))

## Avoid partial matching (also for TRUE and FALSE!)
# Good
rep(1:2, times = 3)
cut(1:10, breaks = c(0, 4, 11))

# Bad
rep(1:2, t = 3)
cut(1:10, br = c(0, 4, 11))
```

